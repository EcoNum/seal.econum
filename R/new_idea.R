SciViews::R
# Use S3 Object
source(file = "R/convert_aa3.R")
source(file = "R/plot_aa3.R")
source(file = "R/calb_aa3.R")

convert_aa3("inst/extra_data/181018E.TXT",
            "inst/extra_data/181018E.xlsx", project = "test") -> aa3

class(aa3)
aa2 <- unclass(aa3_combine)


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


aa3_calb <- calb_aa3(aa3, filter_list = list(Ptot = 50, Ntot = c(0.1, 0.5)))

plot(aa3_calb)


calb.aa3 <- function(aa3_combine, filter_list) {

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(aa3_combine)) ) {
    stop("class is not aa3")
  }

  # Check_2 : filter_list
  if ( !is.null(filter_list) ) {
    if ( sum(!(names(filter_list) %in% attr(aa3_combine,
                                            which = "method")$method)) != 0 ) {
      stop("Attention : pas de noms pour les elements de la liste ou pas de
           correspondance, utiliser un ou plusieurs des noms suivants :
           'Ptot', 'NO2', 'NOx', 'Ntot', 'NH4', 'PO4'")
    }
    }

  # values, std and conc vectors for nutrient
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "values")] -> values
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "std")] -> std
  names(aa3_combine)[stringr::str_detect(names(aa3_combine),
                                         pattern = "conc")] -> conc

  # Output list
  lm_list <- list()

  # attribute_list
  attribute_list <- attributes(aa3_combine)

  for (i in 1:length(values)) {

    # nutrient name
    nutri_name <-  stringr::str_split(values[i], pattern = "_")[[1]][1]

    # IF ... nutrient in filter_list
    if (nutri_name %in% names(filter_list)) {

      # Check conc_list
      if ( sum(!(filter_list[[nutri_name]] %in% aa3_combine[,std[i]])) != 0 ) {
        stop("Attention : concentration non valide")
      }

      # Replace values by NA
      aa3_combine[which(aa3_combine[,std[i]] %in% filter_list[[nutri_name]] &
                          aa3_combine$sample_type == "CALB"),
                  c(std[i], conc[i], values[i]) ] <- NA

      # Calb_data
      aa3_combine[aa3_combine$sample_type == "CALB", c(std[i], values[i])] %>.%
        stats::na.omit(.) -> calb

      # Check n(std)
      if ( length(calb[[std[i]]]) <= 3) {
        stop("Attention seulement 3 points pour le calcul de la regression")
      }

      # lm
      lm_mod <- stats::lm(calb, formula = stats::as.formula(paste(values[i] ,
                                                                  "~", std[i])))

      data.frame(std_name = paste(nutri_name, "new", sep = "_"),
                 intercept = lm_mod$coefficients[[1]],
                 values = lm_mod$coefficients[[2]],
                 r_squared = round(summary(lm_mod)$r.squared,digits = 4),
                 n = length(calb[[std[i]]]),
                 filter_conc = I(filter_list[nutri_name])
      ) ->  lm_list[[i]]

      names(lm_list)[i] <- paste(nutri_name)

      # add new nutrient values
      cnum <- which(names(aa3_combine) == conc[i])
      names(aa3_combine)[cnum] <- paste(conc[i], "old", sep = "_")
      aa3_combine %>.%
        dplyr::mutate(., new = round((aa3_combine[,values[i]] -
                                        lm_mod$coefficients[[1]]) /
                                       lm_mod$coefficients[[2]],3)
        ) -> aa3_combine

      names(aa3_combine)[length(aa3_combine)] <- paste(conc[i])

    } else {

      lm_list[[i]] <- attribute_list$calb_lm[i,]

      names(lm_list)[i] <- paste(nutri_name)

    }

  }

  lm_df <- dplyr::bind_rows(lm_list)

  print(lm_df)

  attr(aa3_combine, "class") <- c("aa3", "data.frame")
  attr(aa3_combine, "method") <- attribute_list$method
  attr(aa3_combine, "calb_lm") <- lm_df
  attr(aa3_combine, "calb_lm_old") <- attribute_list$calb_lm
  attr(aa3_combine, "metadata") <- attribute_list$metadata

  return(aa3_combine)
    }

calb.aa3(aa3, filter_list = NULL)
