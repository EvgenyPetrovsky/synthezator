castValue <- function(
  value,
  type
) {
  if (type == "Number") as.double(value)
  else if (type == "Date") as.Date(value, format = "%Y%m%d")
  else if (type == "Varchar") as.character(value)
}

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

random <- function(count, rand_dist_name, rand_dist_mean, rand_dist_sd) {
  if (rand_dist_name == "normal") {
    rnorm(n = count, mean = rand_dist_mean, sd = rand_dist_sd)
  } else if (rand_dist_name == "poisson") {
    rpois(n = count, lambda = rand_dist_mean)
  } else {
    NULL
  }
}

evaluate <- function(expression, data) {
  with(data,{
    eval(parse(text = expression))
  })
}

applyCond <- function(x, condition, data){
  if(is.null(condition) || is.na(condition) || condition == "") {
    x
  } else {
    with(data,{
      ifelse(eval(parse(text = condition)), x, NA)
    })
  }
}

generateAttr <- function(
  count,
  attr_type = c("Number", "Date", "Varchar"),
  eval_seq = NULL,
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
