# If !all(players == humanPlayers), ais must be a list containing an AI at
# position players (minus humanPlayers).
# I.e. if players = c(1, 2), humanPlayers = 1, then ais must be a list with
# an AI at position 2.
playGame <- function(players = c(1, 2), humanPlayers = integer(),  
                     ais = list(), game = NULL,
                     n = 4L, nrow = 6L, ncol = 7L, seed = NA, maxIts = 1000, ...) {
  if (!is.na(seed)) set.seed(seed)
  players <- unique(players)
  humanPlayers <- unique(humanPlayers)
  computerPlayers <- players[!players %in% humanPlayers]
  if (is.null(game)) game <- ConnectN$new(n = n, nrow = nrow, ncol = ncol, ...)
  it <- 1
  # Play games until aborted
  repeat {
    player <- players[1]
    if (player %in% humanPlayers) game$printBoard()
    # Store the board states and moves made for learning
    # Contains a list for each ai.
    boardStates <- initializeAiLists(computerPlayers)
    movesMade <- initializeAiLists(computerPlayers)
    counter <- initializeCounter(computerPlayers)
    game$clearBoard()
    # Make moves until the game is locked
    repeat {
      # Human player case
      if (player %in% humanPlayers) {
        move <- getHumanPlayerMove(player, game)
        if (move == -1) {
          message("Aborting game.")
          return(invisible(-1))
        }
        winner <- game$makeMove(move, player)
      # AI case
      } else {
        ai <- ais[[player]]
        boardStates[[player]][[counter[[player]]]] <- ai$maybeAttachGameState(game)
        move <- ai$getMove(game$board)
        winner <- game$makeMove(move, player)
        movesMade[[player]][[counter[[player]]]] <- move
        counter[[player]] <- counter[[player]] + 1
      }
      player %<>% cyclePlayer(players)
      if (game$locked) {
        if (length(computerPlayers) > 0) {
          cat(paste0("\rTraining ai (iteration ", it, ")...\n"))
          for (aiIndex in computerPlayers) {
            outcome <- if (winner == 0) {
              0
            } else if (winner == aiIndex) {
              1
            } else {
              -1
            }
            ais[[aiIndex]]$adjustRewards(boardStates[[aiIndex]], movesMade[[aiIndex]], outcome)
          }
        }
        break
      }
    }
    if (any(humanPlayers %in% players)) {
      again <- readline("Play again? ")
      if (!as.character(again) %in% c(
        "T", "TRUE", "true", "True", "Yes", "Yes.", "Ja", "Ja.", "ja", "yes", "1", "Klar.", "y", "j"
      )) {
        game$clearBoard()
        break
      }
    }
    it <- it + 1
    if (it > maxIts) {
      cat(paste0("Reached ", maxIts, " iterations. Stopping."))
      return(invisible(ais))
    }
  }
  return(invisible(ais))
}

getHumanPlayerMove <- function(player, game) {
  move <- as.integer(readline(paste0("Player ", player, "'s move? ")))
  repeat {
    if (move == -1) {
      return(invisible(-1))
    }
    if (game$isValidMove(move))
      return(move)
    move <- readline("Invalid move. Try again. ")
  }
}

initializeAiLists <- function(computerPlayers) {
  res <- list()
  for (player in computerPlayers) {
    res[[player]] <- list()
  }
  res
}

initializeCounter <- function(computerPlayers) {
  res <- list()
  for (player in computerPlayers) {
    res[[player]] <- 1
  }
  res
}

