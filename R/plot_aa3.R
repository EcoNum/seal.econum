#' plot aa3 file
#'
#' @param aa3_combine aa3 data
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
plot_aa3 <- function(aa3_combine){

  if ( !("aa3" %in% class(aa3_combine)) ){
    stop("class is not aa3")
  }

(names(aa3_combine)[str_detect(names(aa3_combine), pattern = "values")] -> a)
(names(aa3_combine)[str_detect(names(aa3_combine), pattern = "std")] -> b)
attr(x = aa3_combine, which = "metadata")$sample -> samp_name

# create a list
graph_aa3 <- list()

for(i in 1:length(a)){
  x <- chart::chart(aa3_combine,
             formula = stats::as.formula(paste(a[i], "~",
                                        "date_time%color=%sample_type%group=%1"))) +
    geom_line() +
    geom_point() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    guides(col = guide_legend(title = "Sample",title.position = "top"))


  # Filter data frame
  aa3_combine %>.%
    dplyr::select(., a[i], b[i]) %>.%
    stats::na.omit(.) -> x1

  # lm
  stats::lm(x1, formula = stats::as.formula(paste(a[i] ,"~", b[i]))) -> t1
  broom::tidy(t1) -> t2
  broom::glance(t1) -> t3
  t2$n <- length(x1[,1])
  # Equation
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(a = format(t2$estimate[1], digits = 2),
                        b = format(t2$estimate[2], digits = 2),
                        r2 = format(t3$r.squared, digits = 3)))
  eq <- as.character(as.expression(eq))
  x3 <- chart::chart(x1,
              formula = x1[,1] ~ x1[,2]) +
    geom_point() +
    geom_smooth(method ="lm") +
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


