source("source.R")
ai1 <- AI$new(n = 4L, ncol = 7L, nrow = 6L)
ai2 <- AI$new(n = 4L, ncol = 7L, nrow = 6L)
ais <- list()
ais[[1]] <- ai1
ais[[2]] <- ai2
game <- ConnectN$new(n = 4L, ncol = 7L, nrow = 6L, verbose = F)
playGame(players = c(1, 2), ais = ais, maxIts = 10000, game = game)
ai2$oddsBestMove <- 1
game$clearBoard()
ais <- list()
ais[[2]] <- ai2
game$verbose <- T
playGame(players = c(1, 2), humanPlayers = 1, ais = ais, game = game)
