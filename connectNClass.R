

# TODO: default values if not provided in $new()
ConnectN <- setRefClass("ConnectN",
                        
                     fields = list(
                       n = "integer",
                       nrow = "integer",
                       ncol = "integer",
                       board = "matrix",
                       verbose = "logical",
                       locked = "logical"
                       ),
                     
                     methods = list(
                       # Called by $new()
                       initialize = function(...) {
                         verbose <<- T
                         locked <<- F
                         initFields(...)
                         board <<- matrix(0, nrow = nrow, ncol = ncol)
                       },
                       # Vector of non-full columns if the board is not locked
                       getValidMoves = function() {
                         if (locked) return(0)
                         which(isValidMove(1:ncol(board)))
                       },
                       isValidMove = function(column) {
                         board[1, column] == 0
                       },
                       getEmptyCellRow = function(column) {
                         max(which(board[, column] == 0))
                       },
                       makeMove = function(column, player) {
                         if (locked) stop("Board is locked. Clear to play a new game.")
                         if (!isValidMove(column)) {
                           # For debugging purposes
                           printBoard()
                           print(column)
                           stop("Not a valid move.")
                         }
                         row <- getEmptyCellRow(column)
                         board[row, column] <<- player
                         if (verbose) printBoard()
                         winner <- getWinner(row, column)
                         if (winner != 0) {
                           if (verbose) message(paste0("Player ", winner, " won!"))
                           locked <<- T
                           return(invisible(winner))
                         } else if (isDraw()) {
                           locked <<- T
                           if (verbose) message("Draw.")
                           return(invisible(0))
                         }
                       },
                       isDraw = function() {
                         all(board == 0)
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
                         newRow <- ncol - column + 1
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

