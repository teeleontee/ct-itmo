#include "board.h"

#include <algorithm>
#include <ctime>
#include <sstream>

Board Board::create_goal(const unsigned size)
{
    std::vector<std::vector<unsigned>> brd;
    brd.reserve(size);
    for (std::size_t i = 0; i < size; ++i) {
        std::vector<unsigned> temp;
        temp.reserve(size);
        for (std::size_t j = 0; j < size; ++j) {
            temp.push_back(i + j + 1);
        }
        brd.push_back(temp);
    }
    brd[size - 1][size - 1] = 0;
    return Board(brd);
}

Board Board::create_random(const unsigned size)
{
    std::size_t sqr = size * size;
    std::vector<unsigned> nums;
    nums.reserve(sqr);
    for (std::size_t i = 0; i < sqr; ++i) {
        nums.push_back(i);
    }

    std::shuffle(nums.begin(), nums.end(), get_random_number());

    std::vector<std::vector<unsigned>> brd;
    brd.reserve(size);
    for (std::size_t i = 0; i < size; ++i) {
        std::vector<unsigned> temp;
        temp.reserve(size);
        int offset = i * size;
        for (std::size_t j = 0; j < size; ++j) {
            temp.push_back(nums[offset + j]);
        }
        brd.push_back(temp);
    }
    return Board(brd);
}

std::size_t Board::size() const
{
    return m_size_of_board;
}

bool Board::is_goal() const
{
    if (m_size_of_board == 0 || m_size_of_board == 1) {
        return true;
    }
    return g == 0;
}

unsigned calc_hamming(const std::vector<std::vector<unsigned>> & data)
{
    unsigned cnt = 0;
    if (data.size() == 1) {
        return 0;
    }
    for (std::size_t i = 0; i < data.size(); ++i) {
        for (std::size_t j = 0; j < data.size(); ++j) {
            if (data[i][j] != i * data.size() + j + 1 && data[i][j]) {
                cnt += 1;
            }
            if (!data[i][j] && (i != data.size() - 1 || j != data.size() - 1)) {
                cnt += 1;
            }
        }
    }
    return cnt;
}

unsigned Board::hamming() const
{
    return calc_hamming(m_data);
}

unsigned calc_manhattan(const std::vector<std::vector<unsigned>> & data)
{
    if (data.size() == 1) {
        return 0;
    }
    std::size_t manhattan_dist = 0;
    for (std::size_t i = 0; i < data.size(); ++i) {
        for (std::size_t j = 0; j < data.size(); ++j) {
            std::size_t i_place, j_place;
            int cur = data[i][j];
            i_place = (cur - 1) / data.size();
            j_place = (cur - 1) % data.size();
            if (cur != 0) {
                if (i_place >= i) {
                    manhattan_dist += i_place - i;
                }
                else {
                    manhattan_dist += i - i_place;
                }
                if (j_place >= j) {
                    manhattan_dist += j_place - j;
                }
                else {
                    manhattan_dist += j - j_place;
                }
            }
        }
    }
    return manhattan_dist;
}

unsigned Board::manhattan() const
{
    return calc_manhattan(m_data);
}

std::string Board::to_string() const
{
    std::stringstream strm;
    for (std::size_t i = 0; i < m_size_of_board; ++i) {
        strm << "|";
        for (std::size_t j = 0; j < m_size_of_board; ++j) {
            int temp = m_data[i][j];
            if (temp >= 10)
                strm << std::to_string(temp) << "|";
            else
                strm << " " << std::to_string(temp) << "|";
        }
        strm << "\n";
    }
    return strm.str();
}

bool Board::is_solvable() const
{
    int inv_count = 0;
    std::vector<unsigned> temp;
    temp.reserve(m_size_of_board * m_size_of_board);
    for (const auto & x : m_data) {
        for (const auto y : x) {
            temp.push_back(y);
        }
    }

    for (std::size_t i = 0; i < temp.size(); ++i) {
        for (std::size_t j = i + 1; j < temp.size(); ++j) {
            if (temp[i] && temp[j] && temp[i] > temp[j]) {
                inv_count++;
            }
        }
    }

    int empty_pos = empty_position();
    if (m_size_of_board & 1) {
        return (inv_count & 1) == 0;
    }
    else {
        if (empty_pos & 1) {
            return (inv_count & 1) == 0;
        }
        else {
            return (inv_count & 1) != 0;
        }
    }
}

Board::Board(const unsigned int size)
{
    Board board = Board::create_random(size);
    m_size_of_board = board.m_size_of_board;
    m_data = board.m_data;
}

int Board::empty_position() const
{
    int empty_pos;
    for (int i = m_size_of_board - 1; i >= 0; i--) {
        for (int j = m_size_of_board - 1; j >= 0; j--) {
            if (m_data[i][j] == 0) {
                empty_pos = m_size_of_board - i;
                return empty_pos;
            }
        }
    }
    return -1;
}

Board::Board(const Board & b, int i_zero, int j_zero, int i, int j)
    : m_size_of_board(b.m_size_of_board)
    , m_data(b.m_data)
    , e(b.e + 1)
{
    int temp = m_data[i_zero + i][j_zero + j];
    m_data[i_zero + i][j_zero + j] = 0;
    m_data[i_zero][j_zero] = temp;
    g = calc_hamming(m_data);
    h = calc_manhattan(m_data);
}

Board::Board(const std::vector<std::vector<unsigned>> & data)
    : m_size_of_board(data.size())
    , m_data(data)
    , g(calc_hamming(data))
    , h(calc_manhattan(data))
    , e(0)
{
}
