demo_rules <- utils::read.csv(
  file = "./data-raw/demo-rules.csv",
  header = TRUE, stringsAsFactors = FALSE
)
devtools::use_data(demo_rules, overwrite = TRUE)
