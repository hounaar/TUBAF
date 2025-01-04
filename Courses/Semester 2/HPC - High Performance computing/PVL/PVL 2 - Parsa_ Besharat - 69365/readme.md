# MPI Message Passing: C++ Implementation

This repository contains the C++ implementation of a Message Passing Interface (MPI) program designed to measure the time taken for message exchanges between two ranks in a distributed memory environment. The program is part of an assignment for the course *Introduction to High Performance Computing and Optimization*.

---

## **Description**

The program demonstrates the use of MPI for basic message passing. It sends a single `double` value from rank 0 to rank 1 and back, looping this process 50 times. The total time for these 100 message exchanges is measured using `MPI_Wtime()`, and the average time per message is computed and displayed in microseconds.

---

## **Cluster Setup**

This program was executed on an HPC cluster using the following modules:
1. **`intel-oneapi-mpi/2021.13.0`**: Intel's MPI library.
2. **`openmpi/gcc/11.4.0/5.0.3`**: OpenMPI library for message passing.
3. **`python/gcc/11.4.0/3.11.7`**: Python environment (not used in this implementation).

---

## **Usage**

### **Compiling the Code**
To compile the program, use the MPI C++ compiler:
```bash
mpicxx -o runner main.cpp
```

## Running the Program
Run the compiled program with exactly two MPI processes:

```bash
mpirun -np 2 ./runner
```

## Output
Upon successful execution, the program will display:

- Total Time: The total time taken for 100 message exchanges (50 sends and 50 receives).
- Average Time Per Message: The average time for a single message exchange in microseconds.


```bash
Total time: 5.4765e-05 seconds
Average time per message: 0.54765 microseconds
```

## Performance Analysis
The program's results demonstrate the efficiency of C++ in high-performance computing. With an average message time of 0.54765 microseconds, the implementation achieves low-latency communication, leveraging the benefits of C++'s compiled nature and minimal runtime overhead. This makes it ideal for performance-critical distributed systems.




## References
- MPI Forum: MPI Standard Documentation - https://www.mpi-forum.org/
- OpenMPI: https://www.open-mpi.org/
- Intel MPI Library: https://www.intel.com/content/www/us/en/developer/tools/oneapi/mpi-library.html
- C++ Programming Language: The C++ Programming Language (4th Edition) by Bjarne Stroustrup.


## License
This project is licensed under the GNU License.
