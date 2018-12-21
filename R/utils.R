#' Vector of set names and vector of set values into list of set values
#'
#' @description Function transforms dataframe columns (Set name and Set value)
#'   into list of set values. Sets may be accessed by list element names. The
#'   output of function can be used as and input for data generation functions
#'   (like \link{processTable} or \link{processAttr}).
#'
#' @param set_names vector of set names
#' @param set_values vector of set values
makeSetsFromDF <- function(set_names, set_values) {

  filterBySetName <- function(set_name) {
    x <- set_names == set_name
    set_values[x]
  }

  sets <- unique(set_names)

  result <- Map(f = function(x) {set_values[set_names == x]}, unique(set_names))

  result
}

#' Break data frasme into list of data-frasmes with data grouped by provided
#' column
#'
#' @param df data frame
#' @param column column name within data frame that should be used for groupping
nest_df <- function(df, column) {
  columns <- names(df)
  rest <- columns[columns != column]
  if (!column %in% columns) {stop(paste("dataframe has no column", column))}
  col_vals <- unique(df[[column]])
  mapfun <- function(x) {
    subset(x = df, subset = df[[column]] == x, select = rest)
  }

  Map(f = mapfun, col_vals)
}

#' Process rules for attribute generation
#'
#' @param rules_for_attr dataframe with rules for attributes
#' @param lovs lists of values
#' @param count number of records to generate
#' @param data data frame with earlier prepared results is useful for evaluation
#'   conditions and evaluation expressions
processAttr <- function(rules_for_attr, lovs, count, data) {
  r <- rules_for_attr
  if(r$Value.Type == "LOV" && (!r$List.Of.Values %in% names(lovs))) {
    stop(paste("Lsit of values <", r$List.Of.Values, "> is not in the list", sep = ""))
  }
  generateAttr(
    count = count, attr_type = r$Attribute.Type,
    eval_cond = eval(r$Evaluation.Condition),
    value_type = r$Value.Type, fix_offset_value = r$Fixed.Value...Offset,
    lov = lovs[[r$List.Of.Values]],
    rand_dist_name = r$Random.Distribution, rand_dist_mean = r$Mean, rand_dist_sd = r$Standard.Dev,
    sign_type = r$Sign,
    expression = r$Evaluation.Expression,
    data = data
  )
}


processTable <- function(rules_for_table, lovs, count) {

  rls <-
    rules_for_table %>%
    dplyr::mutate(Evaluation.Sequence = dplyr::coalesce(Evaluation.Sequence, 0L)) %>%
    dplyr::arrange(Evaluation.Sequence) %>%
    nest_df(df = ., column = "Attribute")

  result <-
    Reduce(
      f = function(z, x) {
        print(paste("process attr", x))
        vals <- processAttr(rules_for_attr = rls[[x]], lovs = lovs, count = count, data = z)
        z[[x]] <- vals
        z
        },
      x = names(rls),
      init = data.frame(n = 1:count, stringsAsFactors = F)
    ) %>%
    subset(select = -n)

  result
}


#' Process table with rules
#'
#' @export
#'
#' @param rules_df data frame with rules for tables and attributes generation
#' @param lovs_df data frame with lists of values (lov name and lov value pairs)
#' @param count number of records to be generated
processRules <- function(rules_df, lovs_df, count) {
  lovs <- makeSetsFromDF(lovs_df$Set, lovs_df$Value)

  nest_df(rules_df, "Table") %>%
    Map(
      f = function(x) processTable(x, lovs, count)
    )
}
