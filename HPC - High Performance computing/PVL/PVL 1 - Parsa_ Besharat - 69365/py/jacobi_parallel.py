import numpy as np
import time
import argparse
from multiprocessing import Pool

def initialize_matrix(n):
    A = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            if i == j:
                A[i, j] = 20
            else:
                A[i, j] = -2 ** (-(2 * abs(i - j)))
    return A

def initialize_vector(n):
    return np.ones(n)

def compute_residual(A, x, b):
    return np.linalg.norm(b - np.dot(A, x))


def compute_row(i, A, b, x, n):
    sum_ = sum(A[i, j] * x[j] for j in range(n) if i != j)
    return (b[i] - sum_) / A[i, i]

def jacobi_parallel(A, b, tol, num_threads):
    n = len(b)
    x = np.zeros(n)
    x_new = np.zeros(n)
    residual = compute_residual(A, x, b)
    initial_residual = residual

    while residual > tol * initial_residual:
        with Pool(num_threads) as pool:
            x_new = np.array(pool.starmap(compute_row, [(i, A, b, x, n) for i in range(n)]))
        x = x_new.copy()
        residual = compute_residual(A, x, b)
    return x

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Parallel Jacobi Method with Strong Scaling Test")
    parser.add_argument("-n", type=int, required=True, help="Size of the matrix (n x n)")
    parser.add_argument("-t", type=int, default=1, help="Number of threads")
    parser.add_argument("--test-scaling", action="store_true", help="Run strong scaling test")
    args = parser.parse_args()

    n = args.n
    num_threads = args.t
    tol = 1e-6

    A = initialize_matrix(n)
    b = initialize_vector(n)

    if args.test_scaling:
        for threads in [1, 2, 4, 8]:
            start_time = time.time()
            x_parallel = jacobi_parallel(A, b, tol, num_threads=threads)
            end_time = time.time()
            print(f"Threads: {threads}, Time: {end_time - start_time:.2f} seconds")
    else:

        start_time = time.time()
        x_parallel = jacobi_parallel(A, b, tol, num_threads=num_threads)
        end_time = time.time()
        print(f"Parallel Jacobi method completed for n={n}, threads={num_threads}.")
        print(f"Execution time: {end_time - start_time:.2f} seconds.")
