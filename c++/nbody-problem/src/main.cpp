#include "nbody.h"

#include <iostream>

int main()
{
    std::string filename = "/home/teeleontee/Documents/uni/cppintro/n-body-teeleontee/test/etc/planets2.txt";
    BasicPositionTracker tracker(filename);

    Track track = tracker.track("Venus", 1000, 1);

    for (const auto & t : track) {
        std::cout << t.x << ' ' << t.y << '\n';
    }
}
