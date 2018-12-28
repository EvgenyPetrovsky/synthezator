#' Convert value into specific data-type
#'
#' @param value value to be converted
#' @param type data type to be returned
castValue <- function(
  value,
  type = c("Number", "Date", "Varchar")
) {
  type <- match.arg(type)

  if (type == "Number") as.double(value)
  else if (type == "Date") as.Date(value, format = "%Y%m%d")
  else if (type == "Varchar") as.character(value)
  else {
    stop(paste("Attribute (column) type <", type, "> is not recognized", sep = ""))
  }
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
  sign_type <- match.arg(sign_type)

  if(is.null(sign_type)) {
    number
  } else {
    if (sign_type == "Any" | is.na(sign_type)) {
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
      stop(paste("Sign type <", sign_type, "> is not recognized", sep = ""))
    }

  }

}

#' Random number generator
#'
#' @description Generate number or date
#'
#' @param count number of values to generate
#' @param rand_dist_name random distribution name; Normal and Poisson are
#'   supported
#' @param rand_dist_mean random distribution mean
#' @param rand_dist_sd random distribution standard deviation
random <- function(
  count,
  rand_dist_name = c("Normal", "Poisson"),
  rand_dist_mean,
  rand_dist_sd
) {
  rand_dist_name <- match.arg(rand_dist_name)

  if (tolower(rand_dist_name) == "normal") {
    stats::rnorm(n = count, mean = rand_dist_mean, sd = rand_dist_sd)
  } else if (tolower(rand_dist_name) == "poisson") {
    stats::rpois(n = count, lambda = rand_dist_mean)
  } else {
    stop(paste("Distribution name <", rand_dist_name, "> is not recognized", sep = ""))
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

#' Reduce length of value according to specification
#'
#' @param value value to be processed
#' @param type data type of value
#' @param len desired length of value; must be specified for Number type
#' @param num_dec number of decimals; must be specified for Number type
reduceLength <- function(
  value,
  type = c("Number", "Date", "Varchar"),
  len = NULL,
  num_dec = NULL
) {
  type <- match.arg(type)

  if (type == "Number") {
    # always modify numeric value according to parameters
    if (is.null(len)) {
      stop(paste("Attribute Length for Number must be specified", sep = ""))
    } else if (is.null(num_dec)) {
      stop(paste("Number Decimals for Number must be specified", sep = ""))
    }
    abs(round(x = value, digits = num_dec)) %% (10^(len - num_dec)) * sign(value)

  } else if (type == "Date") {
    # return value without modifications
    value

  } else if (type == "Varchar") {
    # truncate value if length is specified
    if (is.null(len)) {
      value
    } else {
      substr(value, 1, len)
    }

  }
}

#' Generate values
#'
#' @export
#' @param count number of values to generate
#' @param attr_type data type of attribute (value).
#' @param attr_len maximum length of attribute
#' @param attr_num_dec maximum number of digits in decimal part of number
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
  attr_type,
  attr_len = NULL,
  attr_num_dec = NULL,
  eval_cond = NULL,
  value_type = c("Empty", "Fixed", "LOV", "Random", "Expression"),
  fix_offset_value = NULL,
  lov = NULL,
  rand_dist_name = NULL,
  rand_dist_mean = NULL,
  rand_dist_sd = NULL,
  sign_type = NULL,
  expression = NULL,
  data = NULL
) {
  value_type <- match.arg(value_type)

  result <-
    if (value_type == "Empty") {
      rep(NA, count)
    } else if(value_type == "Fixed") {
      if (is.null(fix_offset_value)) {
        stop("Fixed / Offset value must be specified for value type = \"Fixed\"")
      }
      rep(fix_offset_value, count)
    } else if (value_type == "LOV") {
      if (is.null(lov)) {
        stop("'List of Values' parameter must be specified for Value Type = \"LOV\"")
      }
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
      stop(paste("Value type <", value_type, "> is not recognized", sep = ""))
    }

  result %>%
    applyCond(condition = eval_cond, data = data) %>%
    castValue(type = attr_type) %>%
    reduceLength(type = attr_type, len = attr_len, num_dec = attr_num_dec)

}
