import numpy as np
import time
import argparse

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

def jacobi_serial(A, b, tol):
    n = len(b)
    x = np.zeros(n)
    x_new = np.zeros(n)
    residual = compute_residual(A, x, b)
    initial_residual = residual

    while residual > tol * initial_residual:
        for i in range(n):
            sum_ = sum(A[i, j] * x[j] for j in range(n) if i != j)
            x_new[i] = (b[i] - sum_) / A[i, i]
        x = x_new.copy()
        residual = compute_residual(A, x, b)
    return x

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Serial Jacobi Method")
    parser.add_argument("-n", type=int, required=True, help="Size of the matrix (n x n)")
    args = parser.parse_args()

    n = args.n
    A = initialize_matrix(n)
    b = initialize_vector(n)
    tol = 1e-6


    start_time = time.time()
    x = jacobi_serial(A, b, tol)
    end_time = time.time()


    print(f"Serial Jacobi method completed for n={n}.")
    print(f"Execution time: {end_time - start_time:.2f} seconds.")
