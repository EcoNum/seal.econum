#' plot aa3 file
#'
#' @param obj aa3 object
#'
#' @return Une liste contenant un dataframe avec les donnees CALB,
#' les graphes de controle pour la calibration, un dataframe avec les parametres
#' des regressions et un dataframe avec les donnees SAMP filtrees
#' @export
#' @import stringr
#' @importFrom chart chart
#' @importFrom stats na.omit
#' @importFrom stats lm
#' @importFrom stats as.formula
#' @import ggplot2
#' @importFrom  dplyr select
#' @importFrom broom tidy
#' @importFrom broom glance
#' @importFrom ggrepel geom_text_repel
#' @importFrom chart ggarrange
#' @importFrom ggpubr annotate_figure
#' @importFrom ggpubr text_grob
#'
#'
#' @examples
#' #TODO
#'

plot.aa3 <- function(obj){

(names(obj)[str_detect(names(obj), pattern = "values")] -> a)
(names(obj)[str_detect(names(obj), pattern = "std")] -> b)
attr(x = obj, which = "metadata")$sample -> samp_name

# create a list
graph_aa3 <- list()

for(i in 1:length(a)){
  x <- chart::chart(obj,
             formula = stats::as.formula(paste(a[i], "~",
                                        "date_time%color=%sample_type%group=%1"))) +
    geom_line() +
    geom_point() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    guides(col = guide_legend(title = "Sample",title.position = "top"))

  # Filter data frame
  obj %>.%
    dplyr::select(., a[i], b[i]) %>.%
    stats::na.omit(.) -> x1

  # Equation
  attr(obj, "calb_lm") -> calb_lm
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(calb_lm$intercept[i], digits = 2),
                        b = format(calb_lm$values[i], digits = 2),
                        r2 = format(calb_lm$r_squared[i], digits = 3)))
  eq <- as.character(as.expression(eq))
  x3 <- chart::chart(x1,
              formula = x1[,1] ~ x1[,2]) +
    geom_point() +
    geom_abline(intercept = calb_lm$intercept[i], slope = calb_lm$values[i]) +
    labs( y = names(x1)[1], x = names(x1)[2]) +
    geom_text(x = 2*(diff(range(x1[,2])))/5 , y = max(x1[,1]),
              label = eq, parse = TRUE) +
    ggrepel::geom_text_repel(aes(label = x1[,2]), nudge_y = 1.5,
                             nudge_x = 1.5, direction = "both",
                             segment.size = 0.2)
  x4 <- chart::ggarrange(x, x3)
  x5 <- ggpubr::annotate_figure(x4,
                                top = ggpubr::text_grob(a[i], size =  14, face = "bold"),
                                bottom = ggpubr::text_grob(paste0("Data source: ", samp_name),
                                                           color = "blue", hjust = 1,
                                                           x = 1, face = "italic",
                                                           size = 10))
  graph_aa3[[i]] <- x5
  names(graph_aa3)[i] <- paste(a[i])
}
return(graph_aa3)
}


