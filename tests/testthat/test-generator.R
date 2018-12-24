library(synthezator)

test_that("Generate vector of required size", {
  c <- 100
  g <- generateAttr(
    count = c, attr_type = "Varchar", fix_offset_value = "Hello world!"
  )
  expect_equal(length(g), c)
})

