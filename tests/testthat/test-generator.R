library(synthezator)

test_that("Generate vector of required size", {
  c <- 100
  d <- generateAttr(
    count = c, attr_type = "Date", fix_offset_value = "19991231"
  )
  n <- generateAttr(
    count = c, attr_type = "Number", fix_offset_value = "1.54"
  )
  v <- generateAttr(
    count = c, attr_type = "Varchar", fix_offset_value = "Hello world!"
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

