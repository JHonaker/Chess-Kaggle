context("Move Parser")

source("../../src/chessparse.R")

test_that("parse.move handles non-moves", {
    move_from_start <- function(move) parse.move(list(piece='a1'), move)
    warning_message <- "uci.move not valid"

    expect_warning(move_from_start(""), warning_message)
    expect_warning(move_from_start(NA), warning_message)
    expect_warning(move_from_start(1),  warning_message)
    expect_warning(move_from_start("e2"), warning_message)
    expect_warning(move_from_start("z39"), warning_message)
})

test_that("parse.move recognizes invalid move", {
    move_from <- function(move) parse.move(list(piece='a1'), move)

    expect_warning(move_from('a2g2'), "no piece")
})

test_that("parse.move properly moves pieces", {
    pos <- list(piece1="a1")
    move_from_pos <- function(move) parse.move(pos, move)

    expect_equal(move_from_pos("a1a2"), list(piece1="a2"))
    expect_equal(move_from_pos("a1g8"), list(piece1="g8"))
})

test_that("parse.move properly captures pieces", {
    pos <- list(piece1="a1", piece2="g8")
    move_from_pos <- function(move) parse.move(pos, move)

    expect_equal(move_from_pos("a1g8"), list(piece1="g8", piece2="X"))
    expect_equal(move_from_pos("g8a1"), list(piece1="X", piece2="a1"))
})
