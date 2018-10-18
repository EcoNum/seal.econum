growth <- readRDS("data-raw/growth.rds")

# To include it as a dataset inside the package

devtools::use_data(growth, overwrite = TRUE)
