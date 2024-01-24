## 15puzzle

Программa, которая решает головоломку 15 Puzzle (и её обобщения) с использованием алгоритма A*.

https://en.wikipedia.org/wiki/15_puzzle

https://en.wikipedia.org/wiki/A*_search_algorithm

Пример работы программы

```C++
int main() {
    Board b = Board({{4, 2, 10, 8}, 
                     {0, 13, 1, 14}, 
                     {9, 11, 3, 12}, 
                     {5, 7, 15, 6}});
    const auto solution = Solver::solve(b);
    std::cout << solution.moves() << "\n\n";

    for (const auto& move : solution) {
        std::cout << move << std::endl;
    }
}
