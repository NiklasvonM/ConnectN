

# TODO: default values if not provided in $new()
NWins <- setRefClass("nWins",
                     fields = list(
                       n = "integer",
                       k = "integer",
                       l = "integer",
                       board = "matrix",
                       verbose = "logical"),
                     methods = list(
                       initialize = function(...) {
                         initFields(...)
                         board <<- matrix(0, nrow = k, ncol = l)
                       },
                       isValidMove = function(column) {
                         board[1, column] == 0
                       },
                       getEmptyCellRow = function(column) {
                         max(which(board[, column] == 0))
                       },
                       makeMove = function(player, column) {
                         if (!isValidMove(column))
                           stop("Not a valid move.")
                         row <- getEmptyCellRow(column)
                         board[row, column] <<- player
                         if (verbose) printBoard()
                         if (getWinner(row, column) != 0) {
                           if (verbose) message(paste0("Player ", getWinner(), " won!"))
                           # TODO: Stop the game.
                         }
                       },
                       # Don't check the whole board, just check adjacent cells
                       getWinner = function(row, column) {
                         if (FALSE) {
                           
                         } else if (FALSE) {
                           
                         } else {
                           return(0)
                         }
                       },
                       printBoard = function() {
                         print(board)
                       },
                       clearBoard = function() {
                         board <<- board * 0
                       }
                     )
                     )




