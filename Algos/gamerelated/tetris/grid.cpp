#include "grid.h"

using namespace std;

class Grid {
private:
    vector<vector<int>> grid;

public:
    Grid() : grid(GRID_HEIGHT, vector<int>(GRID_WIDTH, 0)) {}

    bool checkCollision(const Shape& shape) {
        for (int i = 0; i < shape.matrix.size(); ++i) {
            for (int j = 0; j < shape.matrix[i].size(); ++j) {
                if (shape.matrix[i][j] && (shape.y + i >= GRID_HEIGHT || shape.x + j < 0 || shape.x + j >= GRID_WIDTH || grid[shape.y + i][shape.x + j])) {
                    return true;
                }
            }
        }
        return false;
    }

    void mergeShape(const Shape& shape) {
        for (int i = 0; i < shape.matrix.size(); ++i) {
            for (int j = 0; j < shape.matrix[i].size(); ++j) {
                if (shape.matrix[i][j]) {
                    grid[shape.y + i][shape.x + j] = shape.color;
                }
            }
        }
    }

    void clearFullRows() {
        for (int i = 0; i < GRID_HEIGHT; ++i) {
            bool fullRow = true;
            for (int j = 0; j < GRID_WIDTH; ++j) {
                if (grid[i][j] == 0) {
                    fullRow = false;
                    break;
                }
            }
            if (fullRow) {
                grid.erase(grid.begin() + i);
                grid.insert(grid.begin(), vector<int>(GRID_WIDTH, 0));
            }
        }
    }

    void display() {
        system("cls");
        for (const auto& row : grid) {
            for (const auto& cell : row) {
                cout << (cell ? "#" : ".") << " ";
            }
            cout << endl;
        }
    }
};