#include <iostream>
#include <vector>
#include <random>
#include <chrono>
#include <stdexcept>

// Function to read parameters
void readParameters(int argc, char** argv, unsigned int &N, unsigned int &t_steps, double &probability) {
    try {
        if (argc < 4) {
            throw std::invalid_argument("No probability given.");
        } else if (argc < 3) {
            throw std::invalid_argument("No #timesteps given.");
        } else if (argc < 2) {
            throw std::invalid_argument("No problem size given.");
        }

        // Check if the input string is non-positive
        std::string arg = argv[1];
        if (arg[0] == '-' || arg[0] == '0') {
            throw std::invalid_argument("Problem size non-positive.");
        }

        N = std::stoi(argv[1]);
        t_steps = std::stoi(argv[2]);
        probability = std::stod(argv[3]);
    } catch (const std::invalid_argument& e) {
        std::cout << "Usage ./<exe> <unsigned int = problem size> <#timesteps> <probability>\n";
        N = 10;
        t_steps = 10;
        probability = 0.4;
    }
}

// Function to initialize the board randomly
void randomInitialization(std::vector<std::vector<int>> &board, const double &probability) {
    std::mt19937 generator(0); // Mersenne Twister engine
    std::bernoulli_distribution distribution(probability); // chance to be alive at the start

    for (unsigned int i = 1; i < board.size() - 1; i++) {
        for (unsigned int j = 1; j < board[0].size() - 1; j++) {
            board[i][j] = distribution(generator) ? 1 : 0;
        }
    }
}

// Function to print the board
void printBoard(std::vector<std::vector<int>> &board) {
    for (const auto& row : board) {
        for (const auto& cell : row) {
            std::cout << (cell ? "O" : ".") << " "; // "O" for alive, "." for dead
        }
        std::cout << "\n";
    }
}

// Function to compute a timestep
void computeTimestep(const std::vector<std::vector<int>> &source, std::vector<std::vector<int>> &target) {
    unsigned int count;

    for (unsigned int i = 1; i < source.size() - 1; i++) {
        for (unsigned int j = 1; j < source[0].size() - 1; j++) {
            count = source[i-1][j-1] + source[i-1][j] + source[i-1][j+1]
                  + source[i][j-1]                 + source[i][j+1]
                  + source[i+1][j-1] + source[i+1][j] + source[i+1][j+1];
            if (count < 2) {
                target[i][j] = 0; // Underpopulation
            } else if (count == 2) {
                target[i][j] = source[i][j]; // Stays the same
            } else if (count == 3) {
                target[i][j] = 1; // Becomes or stays alive
            } else {
                target[i][j] = 0; // Overpopulation
            }
        }
    }
}

// Main function
int main(int argc, char** argv) {
    // Get problem size and time steps
    unsigned int N, t_steps;
    double probability;

    readParameters(argc, argv, N, t_steps, probability);

    std::cout << "Game of Life with N = " << N << ", " << t_steps
              << " timesteps and initial probability = " << probability << "\n";

    // Declare boards
    const unsigned int padding = 2;
    std::vector<std::vector<int>> boardA(N + padding, std::vector<int>(N + padding, 0));
    std::vector<std::vector<int>> boardB(N + padding, std::vector<int>(N + padding, 0));

    // Initialize boards
    randomInitialization(boardA, probability);
    if (N < 31) printBoard(boardA);

    // Compute timesteps
    auto t_start = std::chrono::high_resolution_clock::now();

    for (unsigned int i = 0; i < t_steps; i++) {
        if (i % 2 == 0) { // board A -> board B
            computeTimestep(boardA, boardB);
            if (N < 31) printBoard(boardB);
        } else { // board B -> board A
            computeTimestep(boardB, boardA);
            if (N < 31) printBoard(boardA);
        }
    }

    auto t_end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> t_duration = t_end - t_start;

    std::cout << "Computation took: " << t_duration.count() << " seconds.\n";
    return 0;
}

