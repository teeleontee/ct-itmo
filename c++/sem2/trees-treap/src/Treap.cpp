#include "Treap.h"

bool Treap::remove(int value)
{
    if (!contains(value)) {
        return false;
    }
    sizeOfTree--;

    if (sizeOfTree == 0) {
        delete root;
        root = nullptr;
        return true;
    }

    if (root->x == value) {
        root = remove_deleter(root);
        return true;
    }

    Node * tmp = root;
    while (tmp != nullptr) {
        if (tmp->rightChild && tmp->rightChild->x == value) {
            tmp->rightChild = remove_deleter(tmp->rightChild);
            return true;
        }
        if (tmp->leftChild && tmp->leftChild->x == value) {
            tmp->leftChild = remove_deleter(tmp->leftChild);
            return true;
        }
        if (tmp->x < value) {
            tmp = tmp->rightChild;
        }
        else {
            tmp = tmp->leftChild;
        }
    }
    return false;
}

Node * Treap::remove_deleter(Node * tmp)
{
    Node * tmp2 = merge(tmp->leftChild, tmp->rightChild);
    tmp->leftChild = nullptr;
    tmp->rightChild = nullptr;
    delete tmp;
    tmp = tmp2;
    return tmp;
}

bool Treap::insert(int value)
{
    if (contains(value)) {
        return false;
    }
    sizeOfTree++;
    Node * tmp = new Node(value);
    auto p = split(root, value);     // p.first < value and p.second > value
    p.first = merge(p.first, tmp);   // al in p.first < all in tmp
    root = merge(p.first, p.second); // all in p.second > all in p.first
    return true;
}

bool Treap::contains(int value) const
{
    Node * tmp = root;
    while (tmp != nullptr) {
        if (tmp->x == value) {
            return true;
        }
        else if (tmp->x > value) {
            tmp = tmp->leftChild;
        }
        else {
            tmp = tmp->rightChild;
        }
    }
    return false;
}

std::size_t Treap::size() const
{
    return sizeOfTree;
}

bool Treap::empty() const
{
    return sizeOfTree == 0;
}

void Treap::printer(Node * r, std::vector<int> & arr) const
{
    if (r != nullptr) {
        printer(r->leftChild, arr);
        arr.push_back(r->x);
        printer(r->rightChild, arr);
    }
}

std::vector<int> Treap::values() const
{
    std::vector<int> arr;
    arr.reserve(sizeOfTree);
    printer(root, arr);
    return arr;
}

Treap::~Treap()
{
    delete root;
}

std::pair<Node *, Node *> Treap::split(Node * r, int x) // p.first < x and p.second > x
{
    if (r == nullptr) {
        return {nullptr, nullptr};
    }
    if (x > r->x) {
        auto p = split(r->rightChild, x);
        r->rightChild = p.first;
        return {r, p.second};
    }
    else {
        auto p = split(r->leftChild, x);
        r->leftChild = p.second;
        return {p.first, r};
    }
}
Node * Treap::merge(Node * a, Node * b) // all in a < all in b
{
    if (a == nullptr) {
        return b;
    }
    if (b == nullptr) {
        return a;
    }
    if (a->y > b->y) {
        a->rightChild = merge(a->rightChild, b);
        return a;
    }
    else {
        b->leftChild = merge(a, b->leftChild);
        return b;
    }
}

Node::~Node()
{
    delete leftChild;
    delete rightChild;
}

Node::Node(int x)
    : x(x)
{
    y = get_random_number();
}
