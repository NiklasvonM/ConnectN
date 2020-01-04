

# TODO: default values if not provided in $new()
ConnectN <- setRefClass("ConnectN",
                     fields = list(
                       n = "integer",
                       k = "integer",
                       l = "integer",
                       board = "matrix",
                       verbose = "logical",
                       locked = "logical"
                       ),
                     methods = list(
                       initialize = function(...) {
                         verbose <<- T
                         locked <<- F
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
                         if (locked) stop("Board is locked. Clear to play a new game.")
                         if (!isValidMove(column))
                           stop("Not a valid move.")
                         row <- getEmptyCellRow(column)
                         board[row, column] <<- player
                         if (verbose) printBoard()
                         winner <- getWinner(row, column)
                         if (winner != 0) {
                           if (verbose) message(paste0("Player ", winner, " won!"))
                           locked <<- T
                         }
                       },
                       # Don't check the whole board, just check adjacent cells
                       getWinner = function(row, column) {
                         player <- board[row, column]
                         if (any(c(
                                   checkRow(row, column, player),
                                   checkColumn(row, column, player),
                                   checkDiagLURL(row, column, player),
                                   checkDiagLLRU(row, column, player)
                                  ) == player))
                             return(player)
                         return(0)
                       },
                       # Check the current row for winners
                       # Returns the winner if there is one, otherwise returns 0
                       checkRow = function(row, column, player) {
                         relevantCells <- board[row, ]
                         if (which(relevantCells == player) %>% hasSeq(n)) {
                           return(player)
                         }
                         return(0)
                       },
                       checkColumn = function(row, column, player) {
                         relevantCells <- board[, column]
                         if (which(relevantCells == player) %>% hasSeq(n)) {
                           return(player)
                         }
                         return(0)
                       },
                       checkDiagLURL = function(row, column, player) {
                         relevantCells <- board[col(board) + row == row(board) + column]
                         if (which(relevantCells == player) %>% hasSeq(n)) {
                           return(player)
                         }
                         return(0)
                       },
                       checkDiagLLRU = function(row, column, player) {
                         rotMat <- rotate(rotate(rotate(board)))
                         newRow <- ncol(board) - column + 1
                         newColumn <- row
                         relevantCells <- rotMat[col(rotMat) + newRow == row(rotMat) + newColumn] %>% rev()
                         if (which(relevantCells == player) %>% hasSeq(n)) {
                           return(player)
                         }
                         return(0)
                       },
                       printBoard = function() {
                         print(board)
                       },
                       clearBoard = function() {
                         board[T, T] <<- 0
                         locked <<- F
                       }
                     )
                     )

