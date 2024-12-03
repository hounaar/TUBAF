#include <iostream>
#include <vector>
#include <cmath>
#include <thread>
#include <mutex>
#include <numeric>
#include <chrono>
#include <functional>
#include <sstream>
#include <iomanip>

std::vector<std::vector<double>> initialize_matrix(int n) {
    std::vector<std::vector<double>> A(n, std::vector<double>(n, 0));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i == j) {
                A[i][j] = 20;
            } else {
                A[i][j] = -pow(2, -(2 * std::abs(i - j)));
            }
        }
    }
    return A;
}

std::vector<double> initialize_vector(int n) {
    return std::vector<double>(n, 1.0);
}

double compute_residual(const std::vector<std::vector<double>>& A, const std::vector<double>& x, const std::vector<double>& b) {
    int n = b.size();
    double norm = 0.0;
    for (int i = 0; i < n; ++i) {
        double sum = 0.0;
        for (int j = 0; j < n; ++j) {
            sum += A[i][j] * x[j];
        }
        norm += pow(b[i] - sum, 2);
    }
    return sqrt(norm);
}

double compute_row(int i, const std::vector<std::vector<double>>& A, const std::vector<double>& b, const std::vector<double>& x, int n) {
    double sum = 0.0;
    for (int j = 0; j < n; ++j) {
        if (i != j) {
            sum += A[i][j] * x[j];
        }
    }
    return (b[i] - sum) / A[i][i];
}

std::vector<double> jacobi_parallel(const std::vector<std::vector<double>>& A, const std::vector<double>& b, double tol, int num_threads) {
    int n = b.size();
    std::vector<double> x(n, 0.0);
    std::vector<double> x_new(n, 0.0);

    double residual = compute_residual(A, x, b);
    double initial_residual = residual;

    while (residual > tol * initial_residual) {
        std::vector<std::thread> threads;

        for (int t = 0; t < num_threads; ++t) {
            threads.emplace_back([&, t]() {
                for (int i = t; i < n; i += num_threads) {
                    x_new[i] = compute_row(i, A, b, x, n);
                }
            });
        }

        for (auto& th : threads) {
            th.join();
        }

        x = x_new;
        residual = compute_residual(A, x, b);
    }
    return x;
}

int main(int argc, char* argv[]) {
    int n = 0;
    int num_threads = 1;
    bool test_scaling = false;
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-n" && i + 1 < argc) {
            n = std::stoi(argv[++i]);
        } else if (arg == "-t" && i + 1 < argc) {
            num_threads = std::stoi(argv[++i]);
        } else if (arg == "--test-scaling") {
            test_scaling = true;
        }
    }

    if (n <= 0) {
        std::cerr << "Matrix size (-n) must be specified and greater than 0.\n";
        return 1;
    }

    double tol = 1e-6;
    auto A = initialize_matrix(n);
    auto b = initialize_vector(n);

    if (test_scaling) {
        for (int threads : {1, 2, 4, 8}) {
            auto start = std::chrono::high_resolution_clock::now();
            jacobi_parallel(A, b, tol, threads);
            auto end = std::chrono::high_resolution_clock::now();
            std::chrono::duration<double> elapsed = end - start;
            std::cout << "Threads: " << threads << ", Time: " << std::fixed << std::setprecision(2) << elapsed.count() << " seconds\n";
        }
    } else {
        auto start = std::chrono::high_resolution_clock::now();
        auto result = jacobi_parallel(A, b, tol, num_threads);
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed = end - start;
        std::cout << "Parallel Jacobi method completed for n=" << n << ", threads=" << num_threads << ".\n";
        std::cout << "Execution time: " << std::fixed << std::setprecision(2) << elapsed.count() << " seconds\n";
    }

    return 0;
}
