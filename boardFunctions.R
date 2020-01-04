isSubset <- function(v1, v2) {
  all(v1 %in% v2)
}

# Does vec contain length consecutive numbers?
hasSeq <- function(vec, length) {
  if (length > length(vec)) return (F)
  for (i in 1:(length(vec) - (length - 1))) {
    subVec <- vec[i:(i + (length - 1))]
    if (all(subVec == subVec[1]:(subVec[1] + length - 1))) return(T)
  }
  return (F)
}

rotate <- function(x) t(apply(x, 2, rev))

cyclePlayer <- function(player, players) {
  if (player == players[length(players)]) {
    player <- players[1]
  } else {
    player <- players[which(players == player) + 1]
  }
  return(player)
}



