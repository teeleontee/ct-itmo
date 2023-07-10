#include "Treap.h"

#include <iostream>

int main()
{
    Treap tree;
    for (int i = 0; i < 10; ++i) {
        tree.insert(i);
    }

    tree.remove(1);
    tree.remove(0);
    tree.remove(5);
    tree.remove(8);
    tree.remove(9);
    tree.remove(3);
    tree.remove(2);

    //    for (auto value : tree.values()) {
    //        std::cout << value << " ";
    //    }
}
