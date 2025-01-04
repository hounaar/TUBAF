from mpi4py import MPI
import time

def compute_integral(start, end, num_intervals, step):
    total = 0.0
    for i in range(start, end):
        x = (i + 0.5) * step
        total += 1.0 / (1.0 + x * x)
    return total

def main():
    num_intervals = int(1e8)
    step = 1.0 / num_intervals

    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    size = comm.Get_size()

    local_start = rank * (num_intervals // size)
    local_end = (rank + 1) * (num_intervals // size)
    if rank == size - 1:
        local_end = num_intervals

    start_time = time.time()

    local_sum = compute_integral(local_start, local_end, num_intervals, step)

    global_sum = comm.reduce(local_sum, op=MPI.SUM, root=0)

    if rank == 0:
        pi = 4.0 * step * global_sum
        end_time = time.time()
        print(f"Computed π: {pi:.7f}")
        print(f"Execution Time: {end_time - start_time:.6f} seconds")

if __name__ == "__main__":
    main()
