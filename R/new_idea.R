SciViews::R
# Use S3 Object
source(file = "R/convert_aa3.R")
source(file = "R/plot_aa3.R")

convert_aa3("inst/extra_data/181018E.TXT",
            "inst/extra_data/181018E.xlsx", project = "test") -> aa3

class(aa3)
aa2 <- unclass(aa3_combine)

plot.aa3 <- function(aa3_combine){

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

plot_aa3(aa3)
plot(aa3)

plot_aa3(aa2)
plot(aa2)

# add reg lin in metadata of aa3 file with convert function -----

summary(p_lm <- lm(data = aa3_combine, Ptot_values ~ Ptot_std))
p_lm %>.% (function (lm, model = lm[["model"]], vars = names(model))
  chart(model, aes_string(x = vars[2], y = vars[1])) +
    geom_point() +
    stat_smooth(method = "lm", formula = y ~ x))(.)

as.numeric(p_lm$coefficients[1]) -> a

aa3 %>.%
  mutate(., Ptot_conc1 = (Ptot_values - as.numeric(p_lm$coefficients[1]))/as.numeric(p_lm$coefficients[2]),
         Ptot_comp = Ptot_conc /Ptot_conc1) -> aa3


aa3_calb <- calb_aa3(aa3, filter_list = NULL)
