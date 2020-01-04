playGame <- function(n = 4L, nrow = 6L, ncol = 7L, seed = NA, players = c(1, 2), humanPlayers = c(1, 2), ...) {
  if (!is.na(seed)) set.seed(seed)
  players <- unique(players)
  humanPlayers <- unique(humanPlayers)
  game <- ConnectN$new(n = n, nrow = nrow, ncol = ncol, ...)
  repeat {
    player <- players[1]
    if (player %in% humanPlayers) game$printBoard()
    repeat {
      if (player %in% humanPlayers) {
        move <- as.integer(readline(paste0("Player ", player, "'s move? ")))
        repeat {
          if (move == -1) {
            message("Aborting game.")
            return(invisible(-1))
          }
          if (game$isValidMove(move))
            break
          move <- readline("Invalid move. Try again. ")
        }
        game$makeMove(move, player)
      } else {
        game$getValidMoves() %>% sample(1) %>% game$makeMove(player)
      }
      player %<>% cyclePlayer(players)
      if (game$locked) break
    }
    if (any(humanPlayers %in% players)) {
      again <- readline("Play again? ")
      if (!as.character(again) %in% c(
        "T", "TRUE", "true", "True", "Yes", "Yes.", "Ja", "Ja.", "ja", "yes", "1", "Klar."
      )) {
        break
      } else {
        game$clearBoard()
      }
    }
  }
}
