#include "solver.h"

#include <queue>

Solver::Solution Solver::solve(const Board & initial)
{
    auto brd = Solution();

    std::vector<Board> tmp;
    tmp.reserve(1000);
    std::priority_queue<Board, std::vector<Board>, AllCompare> m_all_moves(AllCompare{}, std::move(tmp));
    std::map<Board, int, CustomCompare> m_visited_moves;

    brd.m_moves.reserve(150);
    if (initial.size() == 0 || initial.size() == 1 || initial.is_goal()) {
        brd.m_moves.push_back(initial);
        return brd;
    }

    if (!initial.is_solvable()) {
        return brd;
    }

    m_all_moves.push(initial);

    Board ptr;
    while (!m_all_moves.empty()) {
        auto iter = m_all_moves.top();
        m_all_moves.pop();
        if (iter.is_goal()) {
            ptr = std::move(iter);
            break;
        }
        if (m_visited_moves.find(iter) == m_visited_moves.end() || m_visited_moves[iter] > iter.e) {
            m_visited_moves[iter] = iter.e;
            find_kids(iter, m_all_moves);
        }
    }

    while (ptr != initial) {
        brd.m_moves.push_back(ptr);
        std::vector<Board> temp;
        temp.reserve(80);
        std::priority_queue<Board, std::vector<Board>, AllCompare> kids(AllCompare{}, std::move(temp));
        find_kids(ptr, kids);
        int cnt_min = 100;
        while (!kids.empty()) {
            auto kiddo = kids.top();
            kids.pop();
            if (m_visited_moves.find(kiddo) == m_visited_moves.end()) {
                continue;
            }
            if (m_visited_moves[kiddo] < cnt_min) {
                cnt_min = m_visited_moves[kiddo];
                ptr = kiddo;
            }
        }
    }
    brd.m_moves.push_back(ptr);
    std::reverse(brd.m_moves.begin(), brd.m_moves.end());
    return brd;
}

void Solver::find_kids(const Board & initial,
                       std::priority_queue<Board, std::vector<Board>, AllCompare> & m_all_moves)
{
    std::size_t i_zero = 0, j_zero = 0;
    auto pair = zero_finder(initial);
    i_zero = pair.first;
    j_zero = pair.second;

    if (i_zero != 0) {
        m_all_moves.emplace(initial, i_zero, j_zero, -1, 0);
    }
    if (i_zero != initial.size() - 1) {
        m_all_moves.emplace(initial, i_zero, j_zero, 1, 0);
    }
    if (j_zero != 0) {
        m_all_moves.emplace(initial, i_zero, j_zero, 0, -1);
    }
    if (j_zero != initial.size() - 1) {
        m_all_moves.emplace(initial, i_zero, j_zero, 0, 1);
    }
}

std::pair<std::size_t, std::size_t> Solver::zero_finder(const Board & initial)
{
    for (int i = initial.size() - 1; i >= 0; i--) {
        for (int j = initial.size() - 1; j >= 0; j--) {
            if (initial[i][j] == 0) {
                return std::make_pair(i, j);
            }
        }
    }
    return std::make_pair(-1, -1);
}