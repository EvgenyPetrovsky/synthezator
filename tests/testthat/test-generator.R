library(synthezator)

test_that("Generate vector of Fixed values", {
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

test_that("Number of generated elements must be as requested", {
  c <- 100
  v0 <- generateAttr(count = c, attr_type = "Varchar", value_type = "Empty")
  v1 <- generateAttr(count = c, attr_type = "Varchar", value_type = "Fixed", fix_offset_value = "A")
  v2 <- generateAttr(count = c, attr_type = "Varchar", eval_cond = "FALSE", value_type = "Fixed", fix_offset_value = "A")
  v3 <- generateAttr(count = c, attr_type = "Varchar", value_type = "LOV", lov = LETTERS)
  v4 <- generateAttr(count = c, attr_type = "Number", attr_len = 10, attr_num_dec = 0, value_type = "Random", fix_offset_value = 0, rand_dist_name = "Normal", rand_dist_mean = 0, rand_dist_sd = 10, sign_type = "Any")
  v5 <- generateAttr(count = c, attr_type = "Varchar", value_type = "Expression", expression = "LETTERS")

  expect_equal(length(v0), c)
  expect_equal(length(v1), c)
  expect_equal(length(v2), c)
  expect_equal(length(v3), c)
  expect_equal(length(v4), c)
  expect_equal(length(v5), c)
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

test_that("Generation mode: Random value", {
  t <- "Random"
  c <- 100
  v <- generateAttr(value_type = t,
    count = c, attr_type = "Number", fix_offset_value = 0, 
    rand_dist_name = "Normal", rand_dist_mean = 0, rand_dist_sd = 1,
    sign_type = "Any", attr_len = 5, attr_num_dec = 3
  )
  expect_equal(length(v), c)
  v <- generateAttr(value_type = t,
    count = c, attr_type = "Number", fix_offset_value = 0, 
    rand_dist_name = "Poisson", rand_dist_mean = 0,
    sign_type = "Any", attr_len = 5, attr_num_dec = 3
  )
  expect_equal(length(v), c)
})

test_that("Random numbers", {
  cnt = 100
  mean = 0
  sdev = 10
  expect_error(random(cnt))
  random(cnt, "Normal", mean, sdev) %>% length %>% expect_equal(cnt)
  expect_error(random(cnt, "Normal"))
  expect_error(random(cnt, "Normal", rand_dist_mean = mean))
  expect_error(random(cnt, "Normal", rand_dist_sd = sdev))
  random(cnt, "Poisson", cnt, mean) %>% length %>% expect_equal(cnt)
  expect_error(random(cnt, "Poisson"))
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

test_that("If Value type is fixed then argument with Fixed Value must be provided", {
  expect_error(
    generateAttr(
      count = c, attr_type = "Varchar", value_type = "Fixed", fix_offset_value = NULL
    )
  )
})

test_that("If Value type is from LOV then argument list of possible values must be provided", {
  expect_error(
    generateAttr(
      count = c, attr_type = "Varchar", value_type = "LOV", lov = NULL
    )
  )
})

test_that("Test evaluation: trivial case", {
  xpr <- "1:10"
  val <- evaluate(xpr, data = NULL)
  expect_equal(val, 1:10)
})

test_that("Test evaluation: fail if data argument is not passed", {
  xpr <- "1:10"
  expect_error(evaluate(xpr))
})

test_that("Test evaluation: access count", {
  cnt <- 10
  xpr <- "1:count"
  val <- evaluate(xpr, data = NULL, count = cnt)
  expect_equal(val, 1:cnt)
})

test_that("Test evaluation: access data", {
  cnt <- 10
  dta <- data.frame(Column.A = 1:cnt)
  xpr <- "Column.A + count"
  val <- evaluate(xpr, data = dta, count = cnt)
  expect_equal(val, 1:cnt + cnt)
})

test_that("Test validateSign: Any", {
  nums <- c(-1, 0, +1, NA)
  validateSign(nums, "Any") %>% expect_equal(nums)
})

test_that("Test validateSign: Not Negative", {
  nums <- c(-1, 0, +1, NA)
  validateSign(nums, "Not Negative") %>% expect_equal(c(0,0,1,NA))
})

test_that("Test validateSign: Not Positive", {
  nums <- c(-1, 0, +1, NA)
  validateSign(nums, "Not Positive") %>% expect_equal(c(-1,0,0,NA))
})

test_that("Test validateSign: Flip Negative", {
  nums <- c(-1, 0, +1, NA)
  validateSign(nums, "Flip Negative") %>% expect_equal(c(1,0,1,NA))
})

test_that("Test validateSign: Flip Positive", {
  nums <- c(-1, 0, +1, NA)
  validateSign(nums, "Flip Positive") %>% expect_equal(c(-1,0,-1,NA))
})

test_that("Reduce length of Varchar values", {
  txt <- "1234567"
  expect_equal(reduceLength(txt, "Varchar"), txt)
  expect_equal(reduceLength(txt, "Varchar", 5), "12345")
  expect_equal(reduceLength(txt, "Varchar", 100), txt)
})

test_that("Reduce length of Number values", {
  num <- 11.55555555
  expect_error(reduceLength(num, "Number"))
  expect_error(reduceLength(num, "Number", len = 5))
  expect_error(reduceLength(num, "Number", num_dec = 5))
  expect_equal(reduceLength(num, "Number", len = 2,  num_dec = 0), 12)
  expect_equal(reduceLength(num, "Number", len = 5,  num_dec = 3), 11.556)
  expect_equal(reduceLength(num, "Number", len = 1,  num_dec = 0), 2)
  expect_equal(reduceLength(num, "Number", len = 5,  num_dec = 4), 1.5556)
  expect_equal(reduceLength(num, "Number", len = 10, num_dec = 8), num)
  expect_equal(reduceLength(num, "Number", len = 20, num_dec = 16), num)
})
