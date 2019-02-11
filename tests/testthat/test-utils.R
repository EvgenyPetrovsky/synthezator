library(synthezator)

test_that("Make sets from data.frame: ordinary case", {
  sets <- c("A", "A", "B")
  vals <- c("A1", "A2", "B1")
  lovs <- list(A = c("A1", "A2"), B = "B1")
  expect_equal(makeSetsFromDF(sets, vals)$A, lovs$A)
  expect_equal(makeSetsFromDF(sets, vals)$B, lovs$B)
})

test_that("Make sets from data.frame: random order", {
  sets <- c("C", "A", "B", "A")
  vals <- c("C1", "A1", "B1", "A2")
  lovs <- list(A = c("A1", "A2"), B = "B1", C = "C1")
  expect_equal(makeSetsFromDF(sets, vals)$A, lovs$A)
  expect_equal(makeSetsFromDF(sets, vals)$B, lovs$B)
  expect_equal(makeSetsFromDF(sets, vals)$C, lovs$C)
})

test_that("Make sets from data.frame: including duplicates", {
  sets <- c("A", "A", "A", "B")
  vals <- c("A1", "A2", "A2", "B1")
  lovs <- list(A = c("A1", "A2", "A2"), B = "B1")
  expect_equal(makeSetsFromDF(sets, vals)$A, lovs$A)
  expect_equal(makeSetsFromDF(sets, vals)$B, lovs$B)
})

test_that("Make sets from data.frame: including NAs", {
  sets <- c("A", "A", "A", "B")
  vals <- c("A1", "A2", NA, NA)
  lovs <- list(
    A = c("A1", "A2", NA),
    B = as.character(NA))
  expect_equal(makeSetsFromDF(sets, vals)$A, lovs$A)
  expect_equal(makeSetsFromDF(sets, vals)$B, lovs$B)
})

test_that("Nest data.frame: check list names", {
  df <- data.frame(
    col1 = c("A", "A", "B", "C"),
    col2 = c("A1", "A2", "B1", "C1"),
    stringsAsFactors = FALSE
  )
  ns <- nest_df(df, "col1")
  expect_equal(ns %>% names, df$col1 %>% unique())
})

test_that("Nest data.frame: check nested frames", {
  df <- data.frame(
    col1 = c("A", "A", "B", "C"),
    col2 = c("A1", "A2", "B1", NA),
    stringsAsFactors = FALSE
  )
  ns <- nest_df(df, "col1")
  expect_equal(ns$A, df %>% subset(col1 == "A", select = c("col2")))
  expect_equal(ns$B, df %>% subset(col1 == "B", select = c("col2")))
  expect_equal(ns$C, df %>% subset(col1 == "C", select = c("col2")))
})

test_that("Incomplete column set in rules table", {
  cnt <- 100
  rls <- data.frame(
    Table = "T1",
    Attribute = "A1",
    Attribute.Type = "Varchar",
    Attribute.Length = 10,
    Number.Decimals = NA,
    Evaluation.Sequence = NA,
    Evaluation.Condition = NA,
    Value.Type = "Fixed",
    Fixed.Value...Offset = "Hello",
    List.Of.Values = NA,
    Random.Distribution = NA,
    Mean = NA,
    Standard.Dev = NA,
    Sign = NA,
    Evaluation.Expression = NA,
    Seed = NA,
    stringsAsFactors = F
  )
  lov <- data.frame(
    Set = character(),
    Value = character(),
    stringsAsFactors = F
  )
  result_success <- processRules(rls, lov, cnt)
  expect_equal(
    result_success,
    list("T1" = data.frame("A1" = rep("Hello", cnt), stringsAsFactors = F))
  )

  rls_damaged <- subset(rls, select = c(
    "Table", "Attribute", "Attribute.Type", "Attribute.Length", "Value.Type",
    "Fixed.Value...Offset"
  ))
  expect_error(
    processRules(rls_damaged, lov, cnt),
    regexp = "Following mandatory columns are not in Rules dataframe: .+")
})

