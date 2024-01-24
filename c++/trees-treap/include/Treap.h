#pragma once

#include "RandomGenerator.h"

#include <vector>

struct Node
{
    int x;
    double y;

    Node * leftChild = nullptr;
    Node * rightChild = nullptr;

    Node(int x);

    ~Node();
};

class Treap
{
private:
    std::size_t sizeOfTree = 0;
    Node * root = nullptr;
    std::pair<Node *, Node *> split(Node * root, int x);
    Node * merge(Node * a, Node * b);
    void printer(Node * root, std::vector<int> & arr) const;
    Node * remove_deleter(Node * tmp);

public:
    bool contains(int value) const;
    bool insert(int value);
    bool remove(int value);
    std::size_t size() const;
    bool empty() const;
    std::vector<int> values() const;

    ~Treap();
};
