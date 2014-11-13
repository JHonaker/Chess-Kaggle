parse.move <- function(position, uci.move) {
  len <- nchar(uci.move)
  if (len != 4) {
    warning("uci.move not valid, returning position")
    return(position)
  }
  uci.move <- tolower(uci.move)
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
