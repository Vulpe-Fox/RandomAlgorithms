#ifndef GAME_H
#define GAME_H

#include <conio.h>
#include <vector>
#include "grid.h"
#include "shape.h"

class Game {
private:
    Grid grid;
    Shape* currentPiece;
    Shape* nextPiece;
    vector<vector<vector<int>>> shapes;

public:
    Game();
    ~Game();
    Shape* createRandomPiece();
    void spawnNewPiece();
    void handleInput();
    void run();
};

#endif