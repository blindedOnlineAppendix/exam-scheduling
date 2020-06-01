# Final Exam Scheduling Evaluation Results

## Description
This repo contains the data and results used for the evaluation of a Constraint Programming (CP)-based parallel decomposition algorithm, applied to constraint optimization problem of scheduling final exams. The purpose of this evaluation is to study the performance behavior of the search algorithm with large data sets.

- This repository supports the submission of the paper **Evaluation of Parallel Search for a Large Scheduling Problems** submitted to **CP'20:** [International Conference on Principles and Practice of Constraint Programming](https://cp2020.a4cp.org)


## Code

The code folder contains the main files used in the definition of our algorithm.
This files should be integrated with the [OscaR-Modeling library]()


## Data & Execution Setting

The data used in the evaluation is a subset of the complete information of students, courses, and course sections at the university. The data set used takes into account 321 course sections and 5042 students. All the data used is defined in the `.xls` files at the root directory
  - `rooms.xls` defines the university rooms
  - `students_isis_fisi_mate.xls` defines the students following the computer science, mathematics, or physics majors
  - `exams_isis_fisi_mate.xls` defines de exams to scheduled from the computer science, mathematics, and physics departments  

A summary of the execution time results is found in the `execution-results.csv` file. While the full results for all runs are in the results folder.
