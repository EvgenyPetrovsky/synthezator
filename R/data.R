#' Rules for data generation
#'
#' A dataset contains rules to generate attributes (columns) of tables (datasets)
#'
#' @format A dataframe with following variables
#' \describe{
#'   \item{Table}{
#'     Table (Dataset) Name for the result. Result is returned as list of
#'     dataframes, list names conform to Table values from rules.
#'   }
#'   \item{Attribute}{
#'     Attribute (Column) Name for generated result
#'   }
#'   \item{Attribute.Length}{
#'     Maximum length of attribute; mandatory for Number and optional fore Varchar
#'   }
#'   \item{Attribute.Length}{
#'     Maximum number of digits in decimal part of number; mandatry for Number
#'   }
#'   \item{Attribute.Type}{
#'     Data Type of attribute; can be \code{Number}, \code{Date} or \code{Varchar}.
#'     Date attributes must be in specific format "YYYYMMDD"
#'   }
#'   \item{Evaluation.Sequence}{
#'     Defines order in which attributes are being generated. Attributes without
#'     sequence receive Sequence = 0. In case \code{Evaluation.Condition} or
#'     \code{Evaluation.Expression} refer to other attributes it is important
#'     to define correct sequence.
#'   }
#'   \item{Evaluation.Condition}{
#'     Expresstion that returns either \code{FALSE} or \code{TRUE}; empty
#'     condition is treated as truth.
#'   }
#'   \item{Value.Type}{
#'     Defines the way how value is being generated. Can be of following values:
#'     \code{Empty} (Empty value);
#'     \code{Fixed} (Fixed value, the same for all records of dataset);
#'     \code{Expression} (Calculated value based on formula);
#'     \code{Random} (Randomly generated value);
#'     \code{LOV} (Value picked from list of values).
#'   }
#'   \item{Fixed.Value./.Offset}{
#'     Defines fixed value for \code{Fixed} value type or offset for
#'     \code{Random} value type, offset is being added to random value.
#'   }
#'   \item{List.Of.Values}{
#'     Name of list with values. List of values must be existing one for
#'     successful value generation
#'   }
#'   \item{Random.Distribution}{
#'     Name of random distribution; must be specified for attributes with
#'     \code{Value.Type} = \code{Random}
#'   }
#'   \item{Mean}{
#'     Random distribution mean; must be specified for attributes with
#'     \code{Value.Type} = \code{Random}
#'   }
#'   \item{Standard.Dev}{
#'     Random distribution standard deviation; must be specified for attributes with
#'     \code{Value.Type} = \code{Random}
#'   }
#'   \item{Sign}{
#'     Sign Type for randomly generated value; must be specified for attributes with
#'     \code{Value.Type} = \code{Random}. Sometimes it is important to have
#'     values of only specific sign. Possible values are:
#'     \code{Any} (keep avlue as it is);
#'     \code{Not Negative} (replace negative values with 0);
#'     \code{Not Positive} (replace positive values with 0);
#'     \code{Flip Negative} (replace negative values with positive counterparts);
#'     \code{Flip Positive} (replace positive values with negative counterparts);
#'   }
#'   \item{Evaluation.Expression}{
#'     Expression that returns value; must be specified for attributes with
#'     \code{Value.Type} = \code{Expression}
#'   }
#' }
"demo_rules"

#' Lists of Values for data generation
#'
#' A dataset contains lists of values that are referred by \link{demo_rules}
#'
#' @format A dataframe with following variables
#' \describe{
#'   \item{Set}{
#'     Name of the set as it is reffered by \code{List.Of.Values} in rules for
#'     attribues of \code{Value.Type} = \code{LOV}
#'   }
#'   \item{Value}{
#'     Possible values that belong to set
#'   }
#' }
"demo_sets"
