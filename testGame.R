source("source.R")
connectFour <- ConnectN$new(n = 4L, nrow = 6L, ncol = 7L, verbose = T)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)
connectFour$makeMove(4, 1)

connectFour$clearBoard()

connectFour$getValidMoves()


playGame(humanPlayers = 1)
