library(testthat)

# Render the student's RMarkdown file and load its environment
rmarkdown::render("Assignment_Student.Rmd", output_format = "html_document", envir = globalenv())

# Grading checks
test_that("1. Phone prefix count is correct", {
  expect_equal(phone_prefix, 1000)
})

test_that("2. Cookie selection count is correct", {
  expect_equal(cookie_ways, choose(15 + 4 - 1, 4 - 1))  # Example: choosing 15 items into 4 bins
})

test_that("3. Letter codes with distinct characters", {
  expect_equal(code_distinct, factorial(26) / factorial(21))  # 26P5
})

test_that("4. Letter codes with repetition", {
  expect_equal(code_repeat, 26^5)
})

test_that("5. Group of letters without order", {
  expect_equal(group_no_order, choose(26, 5))
})

test_that("6. Group with repetition but no order", {
  expect_equal(group_repeat, choose(26 + 5 - 1, 5))
})
