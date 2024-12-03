from mpi4py import MPI
import numpy as np

numMessages = 50
message = np.array([0.0], dtype='d')  # Use numpy array for MPI compatibility

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

if size != 2:
    if rank == 0:
        print("Error: This program requires exactly 2 MPI processes.")
    MPI.Abort(comm, 1)

comm.Barrier()

if rank == 0:
    start_time = MPI.Wtime()
    for _ in range(numMessages):
        comm.Send(message, dest=1, tag=0)
        comm.Recv(message, source=1, tag=0)
    end_time = MPI.Wtime()
    total_time = end_time - start_time
    avg_time_per_message = (total_time / (2 * numMessages)) * 1e6
    print(f"Total time: {total_time} seconds")
    print(f"Average time per message: {avg_time_per_message} microseconds")

elif rank == 1:
    for _ in range(numMessages):
        comm.Recv(message, source=0, tag=0)
        comm.Send(message, dest=0, tag=0)
