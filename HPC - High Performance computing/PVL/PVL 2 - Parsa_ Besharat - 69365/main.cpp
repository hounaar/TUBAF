#include <mpi.h>
#include <iostream>
#include <iomanip>

int main(int argc, char** argv) {
    // Initialize MPI envicronment
    MPI_Init(&argc, &argv);

    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (size != 2) {
        if (rank == 0) {
            std::cerr << "Error: This program requires exactly 2 MPI processes.\n";
        }
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    const int numMessages = 50;
    double message = 0.0;
    double t_start, t_end;

    // Synchronize before starting the timing
    MPI_Barrier(MPI_COMM_WORLD);

    if (rank == 0) {
        // Rank 0 starts the timer
        t_start = MPI_Wtime();
        for (int i = 0; i < numMessages; ++i) {
            // Send message to rank 1
            MPI_Send(&message, 1, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD);
            // Receive message back from rank 1
            MPI_Recv(&message, 1, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        // Rank 0 stops the timer
        t_end = MPI_Wtime();

        // Calculate the time per message in milliseconds
        double totalTime = t_end - t_start;
        double timePerMessage = (totalTime / (2 * numMessages)) * 1e6; // Convert to microseconds
        std::cout << "Total time: " << totalTime << " seconds\n";
        std::cout << "Average time per message: " << timePerMessage << " microseconds\n";
    } else if (rank == 1) {
        for (int i = 0; i < numMessages; ++i) {
            // Receive message from rank 0
            MPI_Recv(&message, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            // Send message back to rank 0
            MPI_Send(&message, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);
        }
    }

    // Finalize MPI environment
    MPI_Finalize();
    return 0;
}
