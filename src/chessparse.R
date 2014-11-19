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

parse.move <- function(position, uci.move) {
  len <- nchar(uci.move)
  if (len != 4) {
    warning("uci.move not valid, returning position")
    return(position)
  }
  uci.move <- tolower(uci.move)
  src.square  <- substr(uci.move, 1, 2)
  dest.square <- substr(uci.move, 3, 4)

  if (sum(position == src.square) <= 0) {
    warning(paste("no piece at", src.square))
    return(position)
  }

  position[position == dest.square] <- 'X'
  position[position == src.square] <- dest.square
  position
}

parse.game <- function(move.list, start = opening.pos) {
  game <- as.data.frame(start, stringsAsFactors=FALSE)

    for (i in 1:(length(move.list))) {
        move <- move.list[i]

        if (move == '1-0')
            attr(game, 'winner') <- "White"
        else if (move == '0-1')
            attr(game, 'winner') <- "Black"
        else {
            game <- rbind(game, parse.move(game[i,], move))
        }

    }

  rownames(game) <- c()
  game$turn <- 0:(nrow(game)-1)
  game
}
