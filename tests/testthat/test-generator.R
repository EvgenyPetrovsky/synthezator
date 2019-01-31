library(synthezator)

test_that("Generate vector of required size", {
  c <- 100
  t <- "Fixed"
  d <- generateAttr(
    count = c, attr_type = "Date", value_type = t, fix_offset_value = "19991231"
  )
  n <- generateAttr(
    count = c, attr_type = "Number", value_type = t, fix_offset_value = "1.54", attr_len = 3, attr_num_dec = 2
  )
  v <- generateAttr(
    count = c, attr_type = "Varchar", value_type = t, fix_offset_value = "Hello world!"
  )
  expect_equal(length(d), c)
  expect_equal(length(n), c)
  expect_equal(length(v), c)
})

test_that("Cast values", {
  expect_equal(castValue("19991231", "Date"), as.Date("1999-12-31"))
  expect_equal(castValue("1.43", "Number"), 1.43)
  expect_equal(castValue("abcd", "Varchar"), "abcd")
})

test_that("Generation mode: Fixed value", {
  f <- "Hello world!"
  t <- "Fixed"
  c <- 100
  v <- generateAttr(value_type = t,
    count = c, attr_type = "Varchar", fix_offset_value = f
  )
  expect_equal(length(v), c)
  expect_equal(unique(v), f)
  expect_equal(length(unique(v)), 1)

})

test_that("Conditional evaluation: check generated values", {
  t <- "Expression"
  c <- 100
  e <- paste("1", c, sep = ":")
  p <- "x %% 2 == 1"
  v <- generateAttr(
    count = c, attr_type = "Number", attr_len = 3, attr_num_dec = 0,
    eval_cond = p, value_type = t, expression = e
  )
  expect_equal(!is.na(v), (1:c) %% 2 == 1)
})

test_that("Conditional evaluation: check based on data in dataframe", {
  t <- "Expression"
  c <- 100
  e <- paste("1", c, sep = ":")
  p <- (1:c) %% 2 == 1
  v <- generateAttr(
    count = c, attr_type = "Number", attr_len = 3, attr_num_dec = 0,
    eval_cond = "Predicate", value_type = t, expression = e,
    data = data.frame(Predicate = p)
  )
  expect_equal(!is.na(v), p)

})

test_that("Type preservation after conditional evaluation", {
  c <- 100
  f <- "sample(c(T,F),size=100,replace=T)"
  d <- generateAttr(
    count = c, attr_type = "Date", fix_offset_value = "19991231", eval_cond = f
  )
  n <- generateAttr(
    count = c, attr_type = "Number", fix_offset_value = "1.54", attr_len = 3, attr_num_dec = 2, eval_cond = f
  )
  v <- generateAttr(
    count = c, attr_type = "Varchar", fix_offset_value = "Hello world!", eval_cond = f
  )
  expect_equal(class(d), "Date")
  expect_equal(typeof(n), "double")
  expect_equal(typeof(v), "character")
})

