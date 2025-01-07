#ifndef GRID_H
#define GRID_H

#include <iostream>
#include <vector>
#include "shape.h"
#include "constants.h"

class Grid {
private:
    vector<vector<int>> grid;

public:
    Grid();
    bool checkCollision(const Shape& shape);
    void mergeShape(const Shape& shape);
    void clearFullRows();
    void display();
};

#endif