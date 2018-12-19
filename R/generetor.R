#' Convert value into specific data-type
#'
#' @param value value to be converted
#' @param type data type to be returned
castValue <- function(
  value,
  type
) {
  if (type == "Number") as.double(value)
  else if (type == "Date") as.Date(value, format = "%Y%m%d")
  else if (type == "Varchar") as.character(value)
}

#' Validate and adjust sign of a number
#'
#' @description Function adjust sign of a number based on a given limitation
#'   (sign_type). Sign type can be of different values: "Any" (no adjustment
#'   applied), "Not negative" / "Not Positive" (if number is negative / positive
#'   then it is replaced with 0), "Flip Negative" / "Flip Positive" (if number
#'   is negative / positive then it is multiplied by -1)
#' @param number is a number to be validated and adjusted
#' @param sign_type sign type / limitation that should be applied
validateSign <- function(
  number,
  sign_type = c("Any", "Not Negative", "Not Positive", "Flip Negative", "Flip Positive")
) {
  if(is.null(sign_type)) {
    number
  } else {
    if (sign_type == "Any") {
      number
    } else if (sign_type == "Not Negative") {
      ifelse(number < 0, number * 0, number)
    } else if (sign_type == "Not Positive") {
      ifelse(number > 0, number * 0, number)
    } else if (sign_type == "Flip Negative") {
      ifelse(number < 0, number * -1, number)
    } else if (sign_type == "Flip Positive") {
      ifelse(number > 0, number * -1, number)
    } else {
      NULL
    }

  }

}

#' Random number generator
random <- function(count, rand_dist_name, rand_dist_mean, rand_dist_sd) {
  if (rand_dist_name == "normal") {
    rnorm(n = count, mean = rand_dist_mean, sd = rand_dist_sd)
  } else if (rand_dist_name == "poisson") {
    rpois(n = count, lambda = rand_dist_mean)
  } else {
    NULL
  }
}

#' Evaluate result of expression
#'
#' @description Function evaluates expression, expression may refer to
#'   \code{data} provided as a second parameter
#'
#' @param expression text expression in R syntax
#' @param data data frame to be referred, columns can be referred directly by
#'   column name
evaluate <- function(expression, data) {
  with(data,{
    eval(parse(text = expression))
  })
}

#' Apply evaluation condition to result
#'
#' @description Function evaluates condition and if condition is met (TRUE),
#'   then result value returned. If condition is not met (FALSE) empty value
#'   will be returned. Condition may refer to \code{data} provided as a
#'   parameter
#'
#' @param x result to be adjusted (emptied when condition criteria are not met)
#' @param condition text expression in R syntax
#' @param data data frame to be referred, columns can be referred directly by
#'   column name
applyCond <- function(x, condition, data){
  if(is.null(condition) || is.na(condition) || condition == "") {
    x
  } else {
    with(data,{
      ifelse(eval(parse(text = condition)), x, NA)
    })
  }
}

#' Generate values
#'
#' @export
#' @param count number of values to generate
#' @param attr_type data type of attribute (value).
#' @param eval_cond evaluation condition (R syntax, can refer to other columns
#'   in \code{data} param)
#' @param value_type mode to generate value
#' @param fix_offset_value fixed value to be returned / offset for random
#'   numbers or dates
#' @param lov vector of exact values that can be used for value_type = "LOV"
#' @param rand_dist_name distribution name for random number generation
#' @param rand_dist_mean distribution mean for random number generation
#' @param rand_dist_sd distribution standard deviation for random number
#'   generation
#' @param sign_type type of sign for randomly generated number (see
#'   \link{validateSign})
#' @param expression evaluation expression (R syntax, can refer to other columns
#'   in \code{data} param)
#' @param data already generated result in form of data frame, so it can be
#'   referred by \code{eval_cond} or \code{expression}
generateAttr <- function(
  count,
  attr_type = c("Number", "Date", "Varchar"),
  eval_cond = NULL,
  value_type = c("Empty", "Fixed", "LOV", "Random", "Expression"),
  fix_offset_value = NULL,
  lov = NULL,
  rand_dist_name = c("", "normal", "poisson"),
  rand_dist_mean,
  rand_dist_sd,
  sign_type,
  expression = NULL,
  data = NULL
) {

  result <-
    if (value_type == "Empty") {
      rep(NA, count)
    } else if(value_type == "Fixed") {
      rep(fix_offset_value, count)
    } else if (value_type == "LOV") {
      sample(x = lov, size = count, replace = TRUE)
    } else if (value_type == "Random") {
      offset_value <- if (is.null(fix_offset_value)) 0 else fix_offset_value
      base <- if (!is.null(fix_offset_value)) {
        castValue(fix_offset_value, attr_type)
      } else {
        0
      }

      random_num <- random(count, rand_dist_name, rand_dist_mean, rand_dist_sd)

      random_num <-
        if (is.null(sign_type)) {
          random_num
        } else {
          validateSign(number = random_num, sign_type = sign_type)
        }

      if (attr_type == "Number") {
        base + random_num
      } else if (attr_type == "Date") {
        as.Date(round(x = random_num, digits = 0), origin = base)
      }

    } else if (value_type == "Expression") {
      evaluate(expression = expression, data = data)
    } else {
      NA
    }

  result %>%
    applyCond(condition = eval_cond, data = data) %>%
    castValue(type = attr_type)

}
