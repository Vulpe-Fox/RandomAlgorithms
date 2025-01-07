#ifndef SHAPE_H
#define SHAPE_H

#include <vector>
#include "constants.h"

using namespace std;

class Shape {
public:
    vector<vector<int>> matrix;
    int x, y;
    int color;

    Shape(vector<vector<int>> m, int c);
    void rotate();
};

#endif