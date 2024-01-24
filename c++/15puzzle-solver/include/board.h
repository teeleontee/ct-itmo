#pragma once

#include "RandomGenerator.h"

#include <string>
#include <vector>

class Board
{
public:
    static Board create_goal(unsigned size);

    static Board create_random(unsigned size);

    Board() = default;

    explicit Board(const std::vector<std::vector<unsigned>> & data);

    explicit Board(unsigned int);

    Board(const Board & b, int i_zero, int j_zero, int i, int j);

    std::size_t size() const;

    bool is_goal() const;

    unsigned hamming() const;

    unsigned manhattan() const;

    std::string to_string() const;

    bool is_solvable() const;

    friend bool operator==(const Board & lhs, const Board & rhs)
    {
        if (lhs.g != rhs.g) {
            return false;
        }
        if (lhs.h != rhs.h) {
            return false;
        }
        return lhs.m_data == rhs.m_data;
    }

    friend bool operator!=(const Board & lhs, const Board & rhs)
    {
        return !(lhs == rhs);
    }

    friend std::ostream & operator<<(std::ostream & out, const Board & board)
    {
        return out << board.to_string();
    }

    std::vector<unsigned> & operator[](int i)
    {
        return m_data[i];
    }

    const std::vector<unsigned> & operator[](int i) const
    {
        return m_data[i];
    }

    int empty_position() const;

    friend struct AllCompare;
    friend struct CustomCompare;
    friend class Solver;

private:
    std::size_t m_size_of_board = 0;
    std::vector<std::vector<unsigned>> m_data;

    std::size_t g;
    std::size_t h;
    int e = 0;
};

