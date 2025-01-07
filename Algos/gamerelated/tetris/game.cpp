#include "game.h"

using namespace std;

class Game {
private:
    Grid grid;
    Shape* currentPiece;
    Shape* nextPiece;
    vector<vector<vector<int>>> shapes;

public:
    Game() {
        srand(time(0));
        shapes = {{{1, 1, 1, 1}},
                  {{1, 1}, {1, 1}},
                  {{0, 1, 0}, {1, 1, 1}},
                  {{1, 0, 0}, {1, 1, 1}},
                  {{0, 0, 1}, {1, 1, 1}},
                  {{1, 1, 0}, {0, 1, 1}},
                  {{0, 1, 1}, {1, 1, 0}}};
        nextPiece = createRandomPiece();
        spawnNewPiece();
    }

    ~Game() {
        delete currentPiece;
        delete nextPiece;
    }

    Shape* createRandomPiece() {
        int idx = rand() % shapes.size();
        return new Shape(shapes[idx], idx + 1);
    }

    void spawnNewPiece() {
        currentPiece = nextPiece;
        nextPiece = createRandomPiece();
        if (grid.checkCollision(*currentPiece)) {
            cout << "Game Over!" << endl;
            exit(0);
        }
    }

    void handleInput() {
        if (_kbhit()) {
            char key = _getch();
            if (key == 'a') {
                --currentPiece->x;
                if (grid.checkCollision(*currentPiece)) ++currentPiece->x;
            } else if (key == 'd') {
                ++currentPiece->x;
                if (grid.checkCollision(*currentPiece)) --currentPiece->x;
            } else if (key == 's') {
                ++currentPiece->y;
                if (grid.checkCollision(*currentPiece)) {
                    --currentPiece->y;
                    grid.mergeShape(*currentPiece);
                    grid.clearFullRows();
                    spawnNewPiece();
                }
            } else if (key == 'w') {
                currentPiece->rotate();
                if (grid.checkCollision(*currentPiece)) {
                    currentPiece->rotate();
                    currentPiece->rotate();
                    currentPiece->rotate();
                }
            }
        }
    }

    void run() {
        while (true) {
            grid.display();
            handleInput();
            ++currentPiece->y;
            if (grid.checkCollision(*currentPiece)) {
                --currentPiece->y;
                grid.mergeShape(*currentPiece);
                grid.clearFullRows();
                spawnNewPiece();
            }
            _sleep(500);
        }
    }
};