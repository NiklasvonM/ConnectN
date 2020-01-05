source("source.R")
connectFour <- ConnectN$new(n = 4L, nrow = 6L, ncol = 7L, verbose = T)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)

connectFour$clearBoard()

connectFour$getValidMoves()

source("source.R")
ai1 <- AI$new(n = 4L, ncol = 7L, nrow = 6L)
ai2 <- AI$new(n = 4L, ncol = 7L, nrow = 6L)
ais <- list()
ais[[1]] <- ai1
ais[[2]] <- ai2
game <- ConnectN$new(n = 4L, ncol = 7L, nrow = 6L)
playGame(players = c(1, 2), ais = ais, maxIts = 100, game = game)



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
game$clearBoard()
ai$getMove(game$board, 1)