/** *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * *****************************************************************************/

package oscar.modeling.solvers.cp.decompositions

import java.util.concurrent.Executors

import oscar.modeling.constraints.{Constraint, ExpressionConstraint}
import oscar.modeling.models.{UninstantiatedModel, cp}
import oscar.cp.core.NoSolutionException
import oscar.modeling.misc.ComputeTimeTaken
import oscar.modeling.misc.ComputeTimeTaken._
import oscar.modeling.models.cp.MemoCPModel
import oscar.modeling.solvers.cp.Branchings.BranchingInstantiator
import oscar.modeling.solvers.cp.distributed.{SubProblem, SubProblemDiscrepancy, SubProblemMinBound}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
 * A refinement strategy for decomposing a CP problem into subproblems. The idea is to put all current subproblems into a priority queue.
 * While there is not enough subproblems, take the first subproblem on the queue and divide it
 *
 * @param searchInstantiator the search to be used
 * @tparam SubproblemOrdering an object that allows to order the subproblems
 */
abstract class RefinementStrategy[SubproblemOrdering](searchInstantiator: BranchingInstantiator)(implicit ordering: Ordering[SubproblemOrdering])
  extends DecompositionStrategy {

  protected def generate(memoCPModel: MemoCPModel, assignment: List[Constraint], path: List[Int]): SubproblemOrdering

  case class SubproblemInfo(assignment: List[Constraint], path: List[Int], orderInfo: SubproblemOrdering) extends Ordered[SubproblemInfo] {
    override def compare(that: SubproblemInfo): Int = ordering.compare(orderInfo, that.orderInfo)
  }

  /**
   * Decompose the problem
   *
   * @param baseModel the model to decompose
   * @param count     the (minimum) number of subproblems wanted
   * @return A list of assignation to variable that makes the subproblem, along with the associated SubproblemData
   */
  override def decompose(baseModel: UninstantiatedModel, count: Int): List[SubProblem] = {
    if (count == 0) //no decomposition
    return List[SubProblem](new SubProblem(List()))

    //Initialise a CP Model
    val model = try {
      new MemoCPModel(baseModel.removeOptimisation())
    }
    catch {
      case _: NoSolutionException => return List[SubProblem]()
    }


    //Initialise the Priority Queue
    val q = mutable.PriorityQueue[SubproblemInfo]()
    val solutions = mutable.ArrayBuffer[SubproblemInfo]()

    model.declaration.apply(model) {
      q += SubproblemInfo(List(), List(), generate(model, List(), List()))

      val search = searchInstantiator(model)

      while (q.size < count && q.nonEmpty) {
        /*if (q.size % 10 == 0) {
          println("Actual queue size: " + q.size)
        }*/

        //Dequeue the largest subproblem, and compute its domain
        val sp = q.dequeue()

        model.pushState()
        for (c <- sp.assignment)
          model.post(c)

        //Get the children for this state
        val alternatives = search.alternatives()

        if (alternatives.isEmpty) {
          solutions += sp
        }
        else {
          for ((alternative, idx) <- alternatives.zipWithIndex) {
            //println(alternative +" "+idx)
            model.pushState()
            try {
              alternative()
              val addedConstraints = model.getAddedConstraints
              val newPath = sp.path ++ List(idx)
              q += SubproblemInfo(addedConstraints, newPath, generate(model, addedConstraints, newPath))
            }
            catch {
              case _: NoSolutionException =>
            }
            model.popState()
          }
        }
        //Do not forget to pop the state
        model.popState()
      }
    }

    //Sort by appearance in the tree (restore search order)
    val r = q.toList.sortWith((a, b) => {
      var ok = false
      var result = false
      for ((i, j) <- a.path.zip(b.path); if !ok) {
        if (i < j) {
          ok = true
          result = true
        }
        else if (j < i) {
          ok = true
          result = false
        }
      }
      result
    })

    (solutions.toList ++ r).map(sp => extendSubProblem(
      new SubProblem(sp.assignment)
        .addData(SubProblemDiscrepancy, sp.path.sum)
        .addData(SubProblemMinBound, SubProblemMinBound.compute(model.optimisationMethod)),
      sp.orderInfo)
    )
  }


  override def parallelDecompose(baseModel: UninstantiatedModel, count: Int, numberThreads: Int, n: Int): List[SubProblem] = {
    if (count == 0) //no decomposition
    return List[SubProblem](new SubProblem(List()))

    //Initialise a CP Model
    val model = try {
      new MemoCPModel(baseModel.removeOptimisation())
    }
    catch {
      case _: NoSolutionException => return List[SubProblem]()
    }

    //Initialise the Priority Queue
    var q = mutable.PriorityQueue[SubproblemInfo]()
    var solutions = mutable.ArrayBuffer[SubproblemInfo]()

    //First, we build numberThreads subProblems

    model.declaration.apply(model) {
      q += SubproblemInfo(List(), List(), generate(model, List(), List()))

      val search = searchInstantiator(model)

      while (q.size < numberThreads && q.nonEmpty) {
        //Dequeue the largest subproblem, and compute its domain
        val sp = q.dequeue()

        model.pushState()
        for (c <- sp.assignment)
          model.post(c)

        //Get the children for this state
        val alternatives = search.alternatives()

        if (alternatives.isEmpty) {
          solutions += sp
        }
        else {
          for ((alternative, idx) <- alternatives.zipWithIndex) {
            //println(alternative +" "+idx)
            model.pushState()
            try {
              alternative()
              val addedConstraints = model.getAddedConstraints
              val newPath = sp.path ++ List(idx)
              q += SubproblemInfo(addedConstraints, newPath, generate(model, addedConstraints, newPath))
            }
            catch {
              case e: NoSolutionException => println("No solution Exception sequential")
            }
            model.popState()
          }
        }
        //Do not forget to pop the state
        model.popState()
      }
    }
    //Do I need more subProblems?
    if (count > q.size) {
      println("Begins parallelization")
      var value = decomposeWorker(baseModel, numberThreads, count, q, solutions, n)
      println("Ends parallelization")
      solutions = value._1
      q = value._2
      value=null
    }

    //Sort by appearance in the tree (restore search order)
    val r = q.toList.sortWith((a, b) => {
      var ok = false
      var result = false
      for ((i, j) <- a.path.zip(b.path); if !ok) {
        if (i < j) {
          ok = true
          result = true
        }
        else if (j < i) {
          ok = true
          result = false
        }
      }
      result
    })

    (solutions.toList ++ r).map(sp => extendSubProblem(
      new SubProblem(sp.assignment)
        .addData(SubProblemDiscrepancy, sp.path.sum)
        .addData(SubProblemMinBound, SubProblemMinBound.compute(model.optimisationMethod)),
      sp.orderInfo)
    )
  }

  def decomposeWorker(baseModel: UninstantiatedModel, numberThreads: Int, count: Int, orQ: mutable.PriorityQueue[SubproblemInfo], sol: mutable.ArrayBuffer[SubproblemInfo], n: Int): (mutable.ArrayBuffer[SubproblemInfo], mutable.PriorityQueue[SubproblemInfo]) = {
    implicit val ec = new ExecutionContext {
      val threadPool = Executors.newFixedThreadPool(numberThreads)
      def execute(runnable: Runnable) {
        threadPool.submit(runnable)
      }
      def reportFailure(t: Throwable): Unit = {
        t.printStackTrace()
      }
    }
    val m = ((count-orQ.size).toDouble/numberThreads.toDouble).ceil.toInt+1
    var arrAux2 = mutable.Queue[Future[(mutable.ArrayBuffer[SubproblemInfo], mutable.PriorityQueue[SubproblemInfo])]]()
    for (_ <- 0 until numberThreads) {
      val yourQ = mutable.PriorityQueue[SubproblemInfo]()
      yourQ.enqueue(orQ.dequeue())
      //Initialize Thread
      val fut = Future {
        decomposerThread(baseModel, m, yourQ)
      }
      arrAux2 += fut
    }
    while (arrAux2.nonEmpty) {
      val i = arrAux2.dequeue()
      val value = Await.result(i, Duration.Inf)
      sol ++= value._1
      orQ ++= value._2
    }
    (sol, orQ)
  }


  def decomposerThread(baseModel: UninstantiatedModel, count: Int, q: mutable.PriorityQueue[SubproblemInfo]): ( mutable.ArrayBuffer[SubproblemInfo], mutable.PriorityQueue[SubproblemInfo]) = {
    val model = try {
      new MemoCPModel(baseModel.removeOptimisation())
    }
    catch {
      case _: NoSolutionException => return (mutable.ArrayBuffer[SubproblemInfo](), q)
    }
    val solutions = mutable.ArrayBuffer[SubproblemInfo]()
    model.declaration.apply(model) {
      val search = searchInstantiator(model)
      while (q.size < count && q.nonEmpty) {
        //Dequeue the largest subproblem, and compute its domain
        val sp = q.dequeue()
        model.pushState()
        for (c <- sp.assignment)
          model.post(c)
        //Get the children for this state
        val alternatives = search.alternatives()
        if (alternatives.isEmpty) {
          solutions += sp
        }
        else {
          for ((alternative, idx) <- alternatives.zipWithIndex) {
            model.pushState()
            try {
              alternative()
              val addedConstraints = model.getAddedConstraints
              val newPath = sp.path ++ List(idx)
              q += SubproblemInfo(addedConstraints, newPath, generate(model, addedConstraints, newPath))
            }
            catch {
              case _: NoSolutionException => println(s"No solution exception inside thread. Alternative: ${alternative}, idx: ${idx}")
            }
            model.popState()
          }
        }
        //Do not forget to pop the state
        model.popState()
      }
    }
    (solutions, q)
  }

  def extendSubProblem(subproblem: SubProblem, orderInfo: SubproblemOrdering): SubProblem = subproblem
}
