#pragma once

#include "board.h"

#include <algorithm>
#include <map>
#include <queue>
#include <set>

struct CustomCompare
{
    bool operator()(const Board & lhs, const Board & rhs) const
    {
        if (lhs.g + lhs.h == rhs.g + rhs.h) {
            if (lhs.h == rhs.h) {
                return lhs.m_data < rhs.m_data;
            }
            return lhs.h < rhs.h;
        }
        return lhs.g + lhs.h < rhs.g + rhs.h;
    }
};

struct AllCompare
{
    bool operator()(const Board & lhs, const Board & rhs) const
    {
        return (lhs.g + lhs.h) * 1.853 + lhs.e > (rhs.g + rhs.h) * 1.853 + rhs.e;
    }
};

class Solver
{
private:
    static void find_kids(const Board & initial,
                          std::priority_queue<Board, std::vector<Board>, AllCompare> & all_moves);

    static std::pair<std::size_t, std::size_t> zero_finder(const Board & initial);

    class Solution
    {
        friend class Solver;

    public:
        std::size_t moves() const
        {
            if (m_moves.size() == 1 || m_moves.empty()) {
                return 0;
            }
            return m_moves.size() - 1;
        }

        using const_iterator = std::vector<Board>::const_iterator;

        const_iterator begin() const { return m_moves.begin(); }

        const_iterator end() const { return m_moves.end(); }

    private:
        std::vector<Board> m_moves;
    };

public:
    static Solution solve(const Board & initial);
};