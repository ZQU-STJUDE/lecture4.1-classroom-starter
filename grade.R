library(testthat)
library(rmarkdown)

# Render the student's RMarkdown file
render("Assignment_Student_FINAL.rmd", output_format = "html_document", envir = globalenv())

# Problem 1
test_that("1a - 5-letter code, no repetition, order matters", {
  expect_equal(code_a, factorial(26) / factorial(21))  # 26P5
})

test_that("1b - 5-letter code, with repetition, order matters", {
  expect_equal(code_b, 26^5)
})

test_that("1c - 5-letter group, no repetition, order doesn't matter", {
  expect_equal(code_c, choose(26, 5))
})

test_that("1d - 5-letter group, with repetition, order doesn't matter", {
  expect_equal(code_d, choose(26 + 5 - 1, 5))  # stars and bars
})

# Problem 2
test_that("2a - 6 different candies, order matters", {
  expect_equal(candy_a, factorial(10) / factorial(4))  # 10P6
})

test_that("2b - 6 different candies, order doesn't matter", {
  expect_equal(candy_b, choose(10, 6))
})

test_that("2c - 6 candies, with repetition, order doesn't matter", {
  expect_equal(candy_c, choose(10 + 6 - 1, 6))  # stars and bars
})

# Problem 3
test_that("3a - Total 5-card hands", {
  expect_equal(total_hands, choose(52, 5))
})

test_that("3b - Hands with 2 Kings & 2 Queens", {
  expect_equal(kings, choose(4, 2))
  expect_equal(queens, choose(4, 2))
  expect_equal(other_cards, 44)
  expect_equal(favorable_b, choose(4,2) * choose(4,2) * 44)
})

test_that("3c - Probability of 2 Kings & 2 Queens", {
  expect_equal(prob_c, favorable_b / total_hands)
})

test_that("3d - Probability of no Kings or Queens", {
  expect_equal(no_qk, choose(44, 5))
  expect_equal(prob_d, no_qk / total_hands)
})

# Problem 4 — Poker Hands
test_that("4a - High Card", {
  expect_equal(high_card, (choose(13, 5) - 10) * (4^5 - 4))
})

test_that("4b - One Pair", {
  expect_equal(one_pair, 13 * choose(4,2) * choose(12,3) * 4^3)
})

test_that("4c - Two Pairs", {
  expect_equal(two_pairs, choose(13,2) * choose(4,2)^2 * 11 * 4)
})

test_that("4d - Three of a Kind", {
  expect_equal(three_kind, 13 * choose(4,3) * choose(12,2) * 4^2)
})

test_that("4e - Straight", {
  expect_equal(straight, 10 * (4^5 - 4))
})

test_that("4f - Flush", {
  expect_equal(flush, 4 * (choose(13,5) - 10))
})

test_that("4g - Full House", {
  expect_equal(full_house, 13 * choose(4,3) * 12 * choose(4,2))
})

test_that("4h - Four of a Kind", {
  expect_equal(four_kind, 13 * choose(4,4) * 48)
})

test_that("4i - Straight Flush", {
  expect_equal(straight_flush, 9 * 4)
})

test_that("4j - Royal Flush", {
  expect_equal(royal_flush, 4)
})
