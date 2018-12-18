SciViews::R

# Use S3 Object
source(file = "R/convert_aa3.R")
source(file = "R/plot_aa3.R")
source(file = "R/calb_aa3.R")

convert_aa3("inst/extra_data/181018E.TXT",
            "inst/extra_data/181018E.xlsx", project = "test") -> aa3

class(aa3)
aa2 <- unclass(aa3)
class(aa2)

plot(aa3, graph_type = "ALL")
plot(aa3, graph_type = "lm")
plot(aa3, graph_type = "NO") # Il va falloir trouver un autre nom pour graph_type =

calb <- function(obj, ...)
UseMethod("calb")

calb <- calb(aa3, filter_list = list(NO2 = 10, Ntot = c(0.1, 0.5, 1, 2), Ptot = c(50, 25, 10)))

class(calb)

plot(calb, graph_type = )
plot(calb, graph_type = "ALL")
plot(calb, graph_type = "lm")

plot(calb, graph_type = ,old_data = TRUE)
plot(calb, graph_type = "ALL", old_data = TRUE)
plot(calb, graph_type = "lm", old_data = TRUE)
