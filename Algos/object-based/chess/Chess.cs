using System;
using System.Collections.Generic;


namespace Chess
{
    /// <summary>
    /// Chess board which takes in lists of moves to play out a chess game.
    /// </summary>

    /// <summary>
    /// Class <c>Piece</c> is an abstract piece with a colour and logic for 
    /// whether a move is valid from some position to another
    /// </summary>
    public abstract class Piece
    {
        public string Colour { get; }
        public char Symbol { get; protected set; }

    
        public Piece(string colour) {
            Colour = colour;
        }

        public abstract bool IsValidMove(int startX, int startY, int endX, int endY);
    }


    // Non-abstract: Pawn, Rook, Knight, Bishop, Queen, King
    public class Pawn : Piece
    {
        public Pawn(string colour) : base(colour) {
            Symbol = colour == "White" ? 'P' : 'p';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            int direction = Colour == "White" ? 1 : -1;
            return (endX == startX + direction && startY == endY) ||
                   (endX == startX + direction && Math.Abs(endY - startY) == 1);
        }
    }

    public class Rook : Piece
    {
        public Rook(string colour) : base(colour) {
            Symbol = colour == "White" ? 'R' : 'r';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            return startX == endX || startY == endY;
        }
    }

    public class Knight : Piece
    {
        public Knight(string colour) : base(colour) {
            Symbol = colour == "White" ? 'N' : 'n';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            int dx = Math.Abs(endX - startX);
            int dy = Math.Abs(endY - startY);
            return dx * dy == 2;
        }
    }

    public class Bishop : Piece
    {
        public Bishop(string colour) : base(colour) {
            Symbol = colour == "White" ? 'B' : 'b';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            return Math.Abs(endX - startX) == Math.Abs(endY - startY);
        }
    }

    public class Queen : Piece
    {
        public Queen(string colour) : base(colour) {
            Symbol = colour == "White" ? 'Q' : 'q';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            return Math.Abs(endX - startX) == Math.Abs(endY - startY) || startX == endX || startY == endY;
        }
    }

    public class King : Piece
    {
        public King(string colour) : base(colour) {
            Symbol = colour == "White" ? 'K' : 'k';
        }

        public override bool IsValidMove(int startX, int startY, int endX, int endY) {
            return Math.Abs(endX - startX) <= 1 && Math.Abs(endY - startY) <= 1;
        }
    }

    public class Board
    {
        private readonly Piece[,] board;

        public Board() {
            board = new Piece[8, 8];
            InitializeBoard();
        }

        private void InitializeBoard() {
            // Set up initial positions for pieces on the board
            // Pawns
            for (int i = 0; i < 8; i++) {
                board[1, i] = new Pawn("White");
                board[6, i] = new Pawn("Black");
            }

            // Other pieces
            SetupMajorPieces(0, "White");
            SetupMajorPieces(7, "Black");
        }

        private void SetupMajorPieces(int row, string colour) {
            board[row, 0] = new Rook(colour);
            board[row, 1] = new Knight(colour);
            board[row, 2] = new Bishop(colour);
            board[row, 3] = new Queen(colour);
            board[row, 4] = new King(colour);
            board[row, 5] = new Bishop(colour);
            board[row, 6] = new Knight(colour);
            board[row, 7] = new Rook(colour);
        }

        private (int X, int Y) ParseAlgebraicNotation(string position) {
            int x = 8 - int.Parse(position[1].ToString());
            int y = position[0] - 'a';
            return (x, y);
        }

        private void MoveUsingNotation(string notation) {
            /// Regex Matching:
            /// O-O-O|O-O: castles
            /// ([KQRBN])?: capture initial piece
            /// ([a-h])?: capture file of departure (for pawns)
            /// (\d)?: capture the rank of departure
            /// (x)?: indicator of taking (shouldn't matter, but capturing for potential future logic)
            /// ([a-h][1-8]): capture destination of piece
            /// (=([QRBN])): alternate capture destination for pawn promotion on a file
            /// (\+|#)?: check or checkmate
            var match = Regex.Match(notation, "^(O-O-O|O-O|([KQRBN])?([a-h])?(\d)?(x)?([a-h][1-8])(=([QRBN]))?)(\+|#)?$", RegexOptions.IgnoreCase);
            if (!match.Success) {
                Console.WriteLine($"Invalid move: {notation}");
                return;
            }

            if (match.Groups[1].Value == "O-O" || match.Groups[1].Value.ToUpper() == "0-0") {
                Console.WriteLine("Kingside castle");
                return;
            }
            if (match.Groups[1].Value == "O-O-O" || match.Groups[1].Value.ToUpper() == "0-0-0") {
                Console.WriteLine("Queenside castle");
                return;
            }

            string pieceType = match.Groups[2].Value;
            string sourceFile = match.Groups[3].Value;
            string sourceRank = match.Groups[4].Value;
            bool isCapture = match.Groups[5].Value == "x";
            string destination = match.Groups[6].Value;
            string promotion = match.Groups[8].Value;

            var (endX, endY) = ParseAlgebraicNotation(destination);

            if (string.IsNullOrEmpty(pieceType)) {
                // Pawn move
                if (!string.IsNullOrEmpty(sourceFile)) {
                    Console.WriteLine($"Pawn capture from {sourceFile} to {destination}");
                }
                else{
                    Console.WriteLine($"Pawn moves to {destination}");
                }

                if (!string.IsNullOrEmpty(promotion)) {
                    Piece newPiece = promotion.ToUpper() switch
                    {
                        "Q" => new Queen(board[endX, endY].Color),
                        "R" => new Rook(board[endX, endY].Color),
                        "B" => new Bishop(board[endX, endY].Color),
                        "N" => new Knight(board[endX, endY].Color),
                        _ => null
                    };

                    if (newPiece != null) {
                        board[endX, endY] = newPiece;
                        Console.WriteLine($"Pawn promoted to {promotion.ToUpper()} at {destination}");
                    }
                }
            }
            else
            {
                Console.WriteLine($"{pieceType.ToUpper()} moves to {destination}");
            }
        }

        public void ProcessMoveList(string filePath) {
            try
            {
                var lines = File.ReadAllLines(filePath);
                foreach (var line in lines) {
                    MoveUsingNotation(line.Trim());
                }
            }
            catch (Exception ex) {
                Console.WriteLine("Unable to read move list");
            }
        }

        public void DisplayBoard() {
            for (int i = 0; i < 8; i++) {
                for (int j = 0; j < 8; j++) {
                    Console.Write(board[i, j]?.Symbol ?? '.');
                    Console.Write(' ');
                }
                Console.WriteLine();
            }
        }
    }

    class Program
    {
        static void Main(string[] args) {
            Board chessBoard = new Board();
            chessBoard.DisplayBoard();

            // Process moves
            Console.WriteLine("Processing moves...");
            chessBoard.ProcessMoveList("moves.txt");
            chessBoard.DisplayBoard();
        }
    }
}
