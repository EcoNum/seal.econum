library(flow)
orga <- readRDS("data-raw/samp_orga.rds")
orga %>.%
  dplyr::filter(., project == "mesocosm_monitoring") -> samp_orga

# To include it as a dataset inside the package
devtools::use_data(samp_orga, overwrite = TRUE)
