library(flow)
inorga <- readRDS("data-raw/samp_inorga.rds")
inorga %>.%
  dplyr::filter(., project == "mesocosm_monitoring") -> samp_inorga

# To include it as a dataset inside the package
devtools::use_data(samp_inorga, overwrite = TRUE)
