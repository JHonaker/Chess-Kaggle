context("Game Parser")

source('../../src/chessparse.R')

# test.game <- unlist(strsplit("e2e4 d7d5 e4d5 g8f6 d2d4 f6d5 g1f3 g7g6 f1e2 f8g7 c2c4 d5b6 b1c3 e8g8 e1g1 b8c6 c1e3 c8g4 d4d5 g4f3 e2f3 c6e5 e3b6 e5f3 d1f3 a7b6 a2a3 d8d7 a1d1 d7f5 f3f5 g6f5 d1d3 f8d8 f1d1 g8f8 d1d2 d8d7 f2f4 a8d8 g1f2 e7e6 b2b3 g7c3 d3c3 c7c6 f2e3 e6d5 c4d5 d7d5 d2d5 d8d5 e3f3 f8e7 h2h3 h7h5 f3g3 d5d4 g3f3 h5h4 f3e3 d4d6 e3f3 d6g6 f3f2 e7d6 c3d3 d6c5 d3d7 g6g3 b3b4 c5b5 a3a4 b5a4 d7b7 b6b5 b7c7 g3c3 c7f7 a4b4 f7f5 c6c5 f5e5 c3a3 f4f5 a3a7 f2e3 c5c4 f5f6 c4c3 e3d3 a7d7 d3c2 d7d2 c2c1 d2f2 e5e4 b4b3 e4e1 f2f6 c1d1 b5b4 e1e4 c3c2 d1d2 f6d6 0-1", split=' '))

test_that("parse.game assigns winner", {
    expect_equal(attr(parse.game('1-0'), 'winner'), "White")
    expect_equal(attr(parse.game('0-1'), 'winner'), "Black")
})

test_that("parse.game handles a non-terminal game", {
    pos <- list(piece1='a2', piece2='b2')
    game_to <- function(moves) parse.game(moves, start=pos)

    moves <- c('a2a3', 'b2b3')
    result <- as.list(game_to(moves)[3,1:2])
    test <- list(piece1='a3', piece2='b3')

    expect_equal(result, test)
})

test_that("parse.game handles game continuation", {

})

test_that("parse.game handles game from beginning to end", {

})
