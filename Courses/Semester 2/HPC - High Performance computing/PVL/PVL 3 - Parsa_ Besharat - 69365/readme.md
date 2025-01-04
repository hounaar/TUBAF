
# Parallel Computation of π Using MPI: C++ and Python Implementations

This repository contains C++ and Python implementations of a program designed to compute an approximation of \(\pi\) using numerical integration and parallel computing with the Message Passing Interface (MPI). The program evaluates the integral:

\[
\int_0^1 \frac{1}{1 + x^2} dx = \frac{\pi}{4}
\]

and leverages the trapezoidal rule for numerical integration. It is part of the course assignment for *Introduction to High Performance Computing and Optimization*.

---

## **Description**

The program computes the integral \(\int_0^1 \frac{1}{1+x^2} dx\) using the trapezoidal rule and multiplies the result by 4 to approximate \(\pi\). The computation is parallelized using MPI, where each process computes a portion of the integral, and the results are combined using `MPI_Reduce`.

Key features:
- C++ implementation using `MPI` for parallelization.
- Python implementation using `mpi4py` for a similar workflow.
- Strong scaling analysis by running the program with 1, 2, 4, 8, and 16 MPI processes.

---

## **Cluster Setup**

The program was executed on an HPC cluster with the following loaded modules:
1. **`gcc/11.4.0`**: GNU Compiler Collection for C++.
2. **`openmpi/gcc/11.4.0/5.0.3`**: OpenMPI library for parallel message passing.
3. **`cmake/gcc/11.4.0/3.27.9`**: Build system generator for C++ projects.
4. **`gdb/python/gcc/11.4.0/3.11.7/0.14.1`**: Debugging tools with Python integration.

---

## **Usage**

### **C++ Implementation**

#### **Compiling the Code**
To compile the C++ program, use the following command:
```bash
mpic++ -o pi_calculation pi_calculation.cpp
```

#### **Running the Program**
Run the compiled program with the desired number of MPI processes:
```bash
mpirun -np <number_of_processes> ./pi_calculation
```
Replace `<number_of_processes>` with the desired number of processes (e.g., 1, 2, 4, 8, 16).

---

### **Python Implementation**

#### **Dependencies**
Ensure `mpi4py` is installed:
```bash
pip install mpi4py
```

#### **Running the Program**
Run the Python program with the desired number of MPI processes:
```bash
mpirun -np <number_of_processes> python pi_calculation.py
```

---

## **Output**

Upon successful execution, the program displays:
1. **Computed \(\pi\)**: The approximated value of \(\pi\) to 7 decimal places.
2. **Execution Time**: The time taken for the computation.

Example output:
```bash
Computed π: 3.141593
Execution Time: 0.262564 seconds
```

---

## **Performance Analysis**

The following table summarizes the results from running the program with different MPI ranks:

| Number of Processes (Ranks) | Computed π   | Execution Time (seconds) |
|-----------------------------|-------------|---------------------------|
| 1                           | 3.141593    | 0.262564                 |
| 2                           | 3.141593    | 0.132734                 |
| 4                           | 3.141593    | 0.114006                 |
| 8                           | 3.141593    | 0.068759                 |
| 16                          | 3.141593    | 0.036361                 |

The results demonstrate the efficiency of parallel computation. Execution time decreases significantly as the number of processes increases, showcasing strong scaling. However, diminishing returns are observed as communication overhead becomes significant at higher process counts.

---

## **Conclusion**

This project demonstrates the effectiveness of MPI for parallelizing numerical computations. By applying the trapezoidal rule, the integral \(\int_0^1 \frac{1}{1+x^2} dx = \frac{\pi}{4}\) is computed accurately to 7 decimal places. The implementation showcases the synergy between mathematical techniques and high-performance computing, achieving substantial speedups while maintaining precision.

---

## **References**
- MPI Forum: MPI Standard Documentation - https://www.mpi-forum.org/
- OpenMPI: https://www.open-mpi.org/
- mpi4py: https://mpi4py.readthedocs.io/
- GNU Compiler Collection (GCC): https://gcc.gnu.org/
- C++ Programming Language: The C++ Programming Language (4th Edition) by Bjarne Stroustrup.

---

## **License**
This project is licensed under the GNU License.
