#include <iostream>
#include <cmath>
#include <mpi.h>
#include <iomanip>

double compute_integral(int start, int end, int num_intervals, double step) {
    double sum = 0.0;
    for (int i = start; i < end; ++i) {
        double x = (i + 0.5) * step;
        sum += 1.0 / (1.0 + x * x);
    }
    return sum;
}

int main(int argc, char *argv[]) {
    const int num_intervals = 1e8;
    const double step = 1.0 / num_intervals;

    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    int local_start = rank * (num_intervals / size);
    int local_end = (rank + 1) * (num_intervals / size);
    if (rank == size - 1) {
        local_end = num_intervals;
    }

    double start_time = MPI_Wtime();
    double local_sum = compute_integral(local_start, local_end, num_intervals, step);

    double global_sum = 0.0;
    MPI_Reduce(&local_sum, &global_sum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        double pi = 4.0 * step * global_sum;
        double end_time = MPI_Wtime();
        std::cout << std::setprecision(7) << "Computed π: " << pi << std::endl;
        std::cout << "Execution Time: " << end_time - start_time << " seconds" << std::endl;
    }

    MPI_Finalize();
    return 0;
}
