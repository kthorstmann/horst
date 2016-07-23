context("vlookup")


data <- data.frame(a = c(1, 2, 3, 5, 5, 3, NA, 4, 4),
                   b = c("A", "B", "C", "D", "D", "E", "F", "D", NA),
                   stringsAsFactors = FALSE)

test_that("get warning when length of look for and return is not equal", {
  expect_message(vlookup(data, look.for = c(1, 5), look.in = "a", return.from = "b", return.multiple=TRUE, return.na = TRUE), "duplicate return values are returned, i. e. length of look.for is not length of return. To avoid this, set `return.multiple = FALSE`")
  })

test_that("Output is correct", {
  expect_equal(vlookup(data, look.for = c(1, 5), look.in = "a",
                       return.from = "b", return.multiple=TRUE, return.na = TRUE),
               c("A", NA, "D", "D", NA))
  expect_equal(vlookup(data, look.for = c(1, 5), look.in = "a",
                       return.from = "b", return.multiple=FALSE, return.na = TRUE),
               c("A", "D"))
  })


data <- data.frame(a = as.factor(c(1, 2, 3, 5, 5, 3, NA, 4, 4)),
                   b = c("A", "B", "C", "D", "D", "E", "F", "D", NA),
                   stringsAsFactors = FALSE)

test_that("get warning when factor", {
  expect_error(vlookup(data, look.for = c(1, 5), look.in = "a", return.from = "b", return.multiple=TRUE, return.na = TRUE), "look.in = `a`.a cannot be a factor. Please change to numeric or character")
})




