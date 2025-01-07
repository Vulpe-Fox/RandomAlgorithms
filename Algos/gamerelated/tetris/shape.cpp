#include "shape.h"

using namespace std;

class Shape {
public:
    vector<vector<int>> matrix;
    int x, y;
    int color;

    Shape(vector<vector<int>> m, int c) : matrix(m), color(c), x(GRID_WIDTH / 2 - m[0].size() / 2), y(0) {}

    void rotate() {
        vector<vector<int>> rotated(matrix[0].size(), vector<int>(matrix.size()));
        for (int i = 0; i < matrix.size(); ++i) {
            for (int j = 0; j < matrix[i].size(); ++j) {
                rotated[j][matrix.size() - i - 1] = matrix[i][j];
            }
        }
        matrix = rotated;
    }
}