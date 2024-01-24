#!/bin/bash

read -p "please input a lambda for parsing purposes > " var
./gradlew run <<< "$var"
dot -Tpng out/graph.dot > out/output.png
echo "to picture of the graph is located at out/output.png"