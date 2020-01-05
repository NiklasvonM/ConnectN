AI <- setRefClass("AI",
                  fields = list(
                    # space of possible game states:
                    # each element ("state_k") contains the board state ("board"), the
                    # number of games this state has occured in ("n")
                    # and the reward function values for each move ("r")
                    S = "list",
                    # Board parameters from ConnectN
                    n = "integer",
                    nrow = "integer",
                    ncol = "integer",
                    oddsBestMove = "numeric"
                  ),
                  
                  methods = list(
                    initialize = function(...) {
                      S <<- list()
                      oddsBestMove <<- 0.5
                      initFields(...)
                    },
                    maybeAttachGameState = function(game) {
                      if (length(S) > 0) for (i in 1:length(S)) {
                        if (all(game$board == S[[i]]$board)) {
                          return(invisible(i))
                        }
                      }
                      # Initialize reward function: -Inf for invalid moves, 0 for valid ones
                      r <- vector(mode = "numeric", length = game$ncol)
                      r[T] <- -Inf
                      r[game$getValidMoves()] <- 0
                      index <- length(S) + 1
                      S[[paste0("state_", index)]] <<- list(
                        board = game$board,
                        n = 1,
                        r = r
                      )
                      return(invisible(index))
                    },
                    getMove = function(board) {
                      stopifnot(0 <= oddsBestMove & oddsBestMove <= 1)
                      index <- getSIndex(board)
                      possibleMoves <- which(S[[index]]$board[1, ] == 0)
                      # Make the best move
                      if (runif(1, 0, 1) <= oddsBestMove) {
                        # sample(1) in case there are multiple best moves
                        move <- which(S[[index]]$r == max(S[[index]]$r[possibleMoves]))[1]
                      # Make a random move
                      } else {
                        move <- possibleMoves[sample(1:length(possibleMoves), 1)]
                        #assertthat::assert_that(S[[index]]$board[1, move] == 0)
                      }
                      return(move)
                    },
                    # Increase or decrease reward vectors for the board states and moves based on the outcome
                    #
                    # boards: list of boards (i.e. matrices)
                    # moves: list or vector of moves (i.e. numbers from 1 to ncol)
                    # outcome: -1, 0 or 1
                    adjustRewards = function(boardIndices, moves, outcome) {
                      if (!outcome %in% c(-1, 0, 1)) stop("Outcome must be -1, 0 or 1.")
                      if (length(boardIndices) != length(moves)) stop("Number of states must agree with number of moves.")
                      # Iterate through states to adjust the reward function
                      for (i in 1:length(boardIndices)) {
                        # Get the index of the board in S
                        index <- boardIndices[[i]]
                        # S: state list
                        # index: index of the board state
                        # r: reward function (vector)
                        # i: index of current move/state
                        # S[[index]]$n: number of times that game state has occured before
                        S[[index]]$r[moves[[i]]] <<- (S[[index]]$r[moves[[i]]] * S[[index]]$n + outcome) / (S[[index]]$n + 1)
                        S[[index]]$n <<- S[[index]]$n + 1
                      }
                    },
                    # Get the index in S for which S$board agrees with board
                    getSIndex = function(board) {
                      for (j in 1:length(S)) {
                        if (all(S[[j]]$board == board)) {
                          return(j)
                        }
                      }
                      stop("Board state not found! Please call maybeAttachGameState() on the game this board state occures on.")
                    }
                  )
                  )





