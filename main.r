pos.scores <- read.csv('./Data/stockfish.csv', stringsAsFactor = FALSE)

opening.pos <- c(mapply(paste0, letters[1:8], rep(1, 8)),
                 mapply(paste0, letters[1:8], rep(2, 8)),
                 mapply(paste0, letters[1:8], rep(7, 8)),
                 mapply(paste0, letters[1:8], rep(8, 8)))
opening.pos <- data.frame(t(opening.pos), stringsAsFactors=FALSE)
names(opening.pos) <- c('WLrook', 'WLknight', 'WLbishop', 'Wqueen',
                        'Wking', 'WRbishop', 'WRknight', 'WRrook',
                        'a2pawn', 'b2pawn', 'c2pawn', 'd2pawn',
                        'e2pawn', 'f2pawn', 'g2pawn', 'h2pawn',
                        'a7pawn', 'b7pawn', 'c7pawn', 'd7pawn',
                        'e7pawn', 'f7pawn', 'g7pawn', 'h7pawn',
                        'BLrook', 'BLknight', 'BLbishop', 'Bqueen',
                        'Bking', 'BRbishop', 'BRknight', 'BRrook')

test.game <- strsplit("e2e4 d7d5 e4d5 g8f6 d2d4 f6d5 g1f3 g7g6 f1e2 f8g7 c2c4 d5b6 b1c3 e8g8 e1g1 b8c6 c1e3 c8g4 d4d5 g4f3 e2f3 c6e5 e3b6 e5f3 d1f3 a7b6 a2a3 d8d7 a1d1 d7f5 f3f5 g6f5 d1d3 f8d8 f1d1 g8f8 d1d2 d8d7 f2f4 a8d8 g1f2 e7e6 b2b3 g7c3 d3c3 c7c6 f2e3 e6d5 c4d5 d7d5 d2d5 d8d5 e3f3 f8e7 h2h3 h7h5 f3g3 d5d4 g3f3 h5h4 f3e3 d4d6 e3f3 d6g6 f3f2 e7d6 c3d3 d6c5 d3d7 g6g3 b3b4 c5b5 a3a4 b5a4 d7b7 b6b5 b7c7 g3c3 c7f7 a4b4 f7f5 c6c5 f5e5 c3a3 f4f5 a3a7 f2e3 c5c4 f5f6 c4c3 e3d3 a7d7 d3c2 d7d2 c2c1 d2f2 e5e4 b4b3 e4e1 f2f6 c1d1 b5b4 e1e4 c3c2 d1d2 f6d6 0-1", split=' ')[[1]]

perform.move <- function(position, uci.move) {
  len <- nchar(uci.move)
  src.square  <- substr(uci.move, 1, 2)
  dest.square <- substr(uci.move, 3, 4)
  
  position[position == dest.square] <- 'X'
  position[position == src.square] <- dest.square
  position
}

parse.game <- function(move.list) {
  game <- opening.pos
  for (i in 1:(length(move.list)-1))
    game <- rbind(game, perform.move(game[i,], move.list[i]))
  
  rownames(game) <- c()
  game$turn <- 0:(nrow(game)-1)
  game
}
