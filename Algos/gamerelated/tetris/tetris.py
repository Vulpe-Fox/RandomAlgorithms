import pygame
import random

# Initialize pygame
pygame.init()

# Screen dimensions
SCREEN_WIDTH = 300
SCREEN_HEIGHT = 600
GRID_SIZE = 30

# Colors
COLORS = [
    (0, 0, 0),      # black
    (255, 0, 0),    # red
    (0, 255, 0),    # green
    (0, 0, 255),    # blue
    (255, 255, 0),  # yellow
    (255, 0, 255),  # magenta
    (0, 255, 255),  # cyan
    (255, 255, 255) # white
]

# Shapes
SHAPES = [
    [[1, 1, 1, 1]], # line
    [[1, 1],        # box
     [1, 1]],
    [[0, 1, 0],     # t
     [1, 1, 1]],
    [[1, 0, 0],     # L
     [1, 1, 1]],
    [[0, 0, 1],     # reverse L
     [1, 1, 1]],
    [[1, 1, 0],     # S
     [0, 1, 1]],
    [[0, 1, 1],     # reverse S
     [1, 1, 0]]
]

# Initialize grid
def create_grid():
    return [[0] * (SCREEN_WIDTH // GRID_SIZE) for _ in range(SCREEN_HEIGHT // GRID_SIZE)]

def draw_grid(surface, grid):
    for y, row in enumerate(grid):
        for x, value in enumerate(row):
            pygame.draw.rect(surface, COLORS[value], (x * GRID_SIZE, y * GRID_SIZE, GRID_SIZE, GRID_SIZE), 0)

class Piece:
    def __init__(self, shape):
        self.shape = shape
        self.color = random.randint(1, len(COLORS) - 1)
        self.x = (SCREEN_WIDTH // GRID_SIZE) // 2 - len(shape[0]) // 2
        self.y = 0

    def rotate(self):
        self.shape = [[self.shape[y][x] for y in range(len(self.shape))][::-1] for x in range(len(self.shape[0]))]

def check_collision(grid, piece):
    for y, row in enumerate(piece.shape):
        for x, cell in enumerate(row):
            if cell and (piece.x + x < 0 or piece.x + x >= len(grid[0]) or piece.y + y >= len(grid) or grid[piece.y + y][piece.x + x]):
                return True
    return False

def merge_piece(grid, piece):
    for y, row in enumerate(piece.shape):
        for x, cell in enumerate(row):
            if cell:
                grid[piece.y + y][piece.x + x] = piece.color


def clear_rows(grid):
    cleared_rows = 0
    for i, row in enumerate(grid):
        if all(row):
            del grid[i]
            grid.insert(0, [0] * len(row))
            cleared_rows += 1
    return cleared_rows

def main():
    screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
    clock = pygame.time.Clock()
    grid = create_grid()

    current_piece = Piece(random.choice(SHAPES))
    next_piece = Piece(random.choice(SHAPES))

    fall_time = 0
    fall_speed = 200 # arbitrary, but can be higher

    running = True
    while running:
        grid_copy = create_grid()
        for y, row in enumerate(grid):
            for x, cell in enumerate(row):
                grid_copy[y][x] = cell

        for y, row in enumerate(current_piece.shape):
            for x, cell in enumerate(row):
                if cell:
                    grid_copy[current_piece.y + y][current_piece.x + x] = current_piece.color

        screen.fill((0, 0, 0))
        draw_grid(screen, grid_copy)
        pygame.display.flip()

        fall_time += clock.get_rawtime()
        clock.tick()

        if fall_time > fall_speed:
            current_piece.y += 1
            if check_collision(grid, current_piece):
                current_piece.y -= 1
                merge_piece(grid, current_piece)
                clear_rows(grid)
                current_piece = next_piece
                next_piece = Piece(random.choice(SHAPES))
                if check_collision(grid, current_piece):
                    running = False
            fall_time = 0

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_LEFT:
                    current_piece.x -= 1
                    if check_collision(grid, current_piece):
                        current_piece.x += 1
                if event.key == pygame.K_RIGHT:
                    current_piece.x += 1
                    if check_collision(grid, current_piece):
                        current_piece.x -= 1
                if event.key == pygame.K_DOWN:
                    current_piece.y += 1
                    if check_collision(grid, current_piece):
                        current_piece.y -= 1
                if event.key == pygame.K_UP:
                    current_piece.rotate()
                    if check_collision(grid, current_piece):
                        current_piece.rotate()
                        current_piece.rotate()
                        current_piece.rotate()

    pygame.quit()

if __name__ == "__main__":
    main()