demo_sets <- utils::read.csv(
  file = "./data-raw/demo-sets.csv",
  header = TRUE, stringsAsFactors = FALSE
)
devtools::use_data(demo_sets, overwrite = TRUE)
