library(testthat)

rmarkdown::render("Assignment_Student.Rmd", output_format = "html_document", envir = globalenv())

test_that("1. Phone prefix count is correct", {
  expect_true(exists("phone_prefix"), info = "phone_prefix not found")
  if (exists("phone_prefix")) expect_equal(phone_prefix, 1000)
})

test_that("2. Cookie selection count is correct", {
  expect_true(exists("cookie_ways"), info = "cookie_ways not found")
  if (exists("cookie_ways")) expect_equal(cookie_ways, choose(15 + 4 - 1, 4 - 1))
})

test_that("3. Letter codes with distinct characters", {
  expect_true(exists("code_distinct"), info = "code_distinct not found")
  if (exists("code_distinct")) expect_equal(code_distinct, factorial(26) / factorial(21))
})

test_that("4. Letter codes with repetition", {
  expect_true(exists("code_repeat"), info = "code_repeat not found")
  if (exists("code_repeat")) expect_equal(code_repeat, 26^5)
})

test_that("5. Group of letters without order", {
  expect_true(exists("group_no_order"), info = "group_no_order not found")
  if (exists("group_no_order")) expect_equal(group_no_order, choose(26, 5))
})

test_that("6. Group with repetition but no order", {
  expect_true(exists("group_repeat"), info = "group_repeat not found")
  if (exists("group_repeat")) expect_equal(group_repeat, choose(26 + 5 - 1, 5))
})
