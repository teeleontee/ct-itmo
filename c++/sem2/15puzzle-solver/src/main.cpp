#include "solver.h"

#include <iostream>

int main()
{
    Board b = Board({{2, 3}, {0, 1}});
    const auto solution = Solver::solve(b);
    std::cout << solution.moves() << "\n\n";

    for (const auto & move : solution) {
        std::cout << move << std::endl;
    }
}