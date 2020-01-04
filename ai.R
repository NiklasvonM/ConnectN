AI <- setRefClass("AI",
                  fields = list(
                    # space of possible game states:
                    # each element ("state_k") contains the board state ("board"), the
                    # number of games this state has occured in ("n")
                    # and the reward function values for each move ("r")
                    S = "list",
                    n = "integer",
                    nrow = "integer",
                    ncol = "integer"
                  ),
                  
                  methods = list(
                    initialize = function(...) {
                      S <<- list()
                      initFields(...)
                    },
                    maybeAttachGameState = function(game) {
                      for (s in S) {
                        if (all(game$board == s$board)) {
                          return(invisible(0))
                        }
                      }
                      # Initialize reward function: -Inf for invalid moves, 0 for valid ones
                      r <- vector(mode = "numeric", length = game$ncol)
                      r[T] <- -Inf
                      r[game$getValidMoves()] <- 0
                      S[[paste0("state_", length(S) + 1)]] <<- list(
                        board = game$board,
                        n = 1,
                        r = r
                      )
                    },
                    adjustRewards = function(states, moves, outcome) {
                      if (!outcome %in% c(-1, 0, 1)) stop("Outcome must be -1, 0 or 1.")
                      if (length(states) != length(moves)) stop("Number of states must agree with number of moves.")
                      # Iterate through states to adjust the reward function
                      for (i in 1:length(states)) {
                        # Get the index of the state in S
                        for (j in 1:length(S)) {
                          if (all(S[[j]]$board == states[[i]])) {
                            index <- j
                            break
                          }
                        }
                        # S: state list
                        # index: index of the board state
                        # r: reward function (vector)
                        # i: index of current move/state
                        S[[index]]$r[moves[[i]]] <<- S[[index]]$r[moves[[i]]] + outcome
                      }
                    }
                  )
                  )

ai <- AI$new(n = 4L, nrow = 6L, ncol = 7L)
ai$S
source("source.R")
game <- ConnectN$new(n = 4L, nrow = 6L, ncol = 7L)
game$printBoard()
ai$maybeAttachGameState(game)
ai$adjustRewards(list(state_1 = game$board), list(move_1 = 4), 1)
game$makeMove(4, 1)
ai$maybeAttachGameState(game)
ai$adjustRewards(list(state_1 = game$board), list(move_1 = 4), 1)
ai$S




