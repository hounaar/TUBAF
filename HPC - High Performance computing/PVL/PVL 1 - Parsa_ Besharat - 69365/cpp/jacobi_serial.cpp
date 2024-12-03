#include <iostream>
#include <vector>
#include <cmath>
#include <chrono>

using namespace std;

void initializeMatrix(vector<vector<double>> &A, int n) {
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
            if (i == j) {
                A[i][j] = 20.0;
            } else {
                A[i][j] = -pow(2.0, -(2 * abs(i - j)));
            }
        }
    }
}

void initializeVector(vector<double> &b, int n) {
    for (int i = 0; i < n; ++i) {
        b[i] = 1.0;
    }
}

double computeResidual(const vector<vector<double>> &A, const vector<double> &x, const vector<double> &b) {
    int n = A.size();
    double residual = 0.0;
    for (int i = 0; i < n; ++i) {
        double Ax_i = 0.0;
        for (int j = 0; j < n; ++j) {
            Ax_i += A[i][j] * x[j];
        }
        residual += pow(b[i] - Ax_i, 2);
    }
    return sqrt(residual);
}

void jacobiSerial(const vector<vector<double>> &A, const vector<double> &b, vector<double> &x, double tol) {
    int n = A.size();
    vector<double> x_new(n, 0.0);
    double residual = computeResidual(A, x, b);
    double initial_residual = residual;

    while (residual > tol * initial_residual) {
        for (int i = 0; i < n; ++i) {
            double sum = 0.0;
            for (int j = 0; j < n; ++j) {
                if (i != j) {
                    sum += A[i][j] * x[j];
                }
            }
            x_new[i] = (b[i] - sum) / A[i][i];
        }
        x = x_new;
        residual = computeResidual(A, x, b);
    }
}

int main() {
    int n = 10000;
    vector<vector<double>> A(n, vector<double>(n, 0.0));
    vector<double> b(n, 0.0);
    vector<double> x(n, 0.0);

    initializeMatrix(A, n);
    initializeVector(b, n);

    double tol = 1e-6;

    // Measure execution time
    auto start = chrono::high_resolution_clock::now();
    jacobiSerial(A, b, x, tol);
    auto end = chrono::high_resolution_clock::now();

    chrono::duration<double> elapsed = end - start;

    cout << "Serial Jacobi method completed." << endl;
    cout << "Execution time: " << elapsed.count() << " seconds." << endl;

    return 0;
}
