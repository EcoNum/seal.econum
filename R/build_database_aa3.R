
#' Build CALB/PRIM/DRIFT database
#'
#' @param obj aa3 object
#'
#' @return un dataframe contenant les donnees CALB, DRIFT et PRIM restructurees
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_locate
#' @importFrom stringr str_sub
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr inner_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom tidyr gather
#' @importFrom lubridate date
#' @importFrom stats na.omit
#'
#' @examples

build_calbdb_aa3 <- function(obj) {

  # Attributes
  attribute_list <- attributes(obj)

  # Creation des vecteurs de noms de colonne pour values, std et conc
  names(obj)[stringr::str_detect(names(obj), pattern = "values$")] %>.%
    sort(.) -> values
  names(obj)[stringr::str_detect(names(obj), pattern = "std$")] %>.%
    sort(.) -> std
  names(obj)[stringr::str_detect(names(obj), pattern = "conc$")] %>.%
    sort(.) -> conc

  #### PRIM, DRIFT ####

  # Filtrer PRIM et DRIF + Ajout d'une colonne filename
  obj %>.%
    dplyr::filter(., sample_type %in% c("PRIM", "DRIF")) %>.%
    dplyr::mutate(., filename = attr(obj,
                                     which = "metadata")$sample) -> prim_drift_all

  # Modification de Sample_id pour retirer les blancs [ex: (1 2 ) --> (1-2)]
  ## Localisation (start end) du pattern
  stringr::str_locate(prim_drift_all$sample_id,
                      pattern = "\\d{1}.\\d{1}") -> locate
  ## Identification des lignes contenant le pattern
  which(stringr::str_detect(prim_drift_all$sample_id,
                            pattern = "\\d{1}.\\d{1}")) -> drift_change
  ## Ajout du "-" dans Sample_id sur base de la ligne
  ## et de la localisation du pattern
  for (i in drift_change){
    stringr::str_sub(prim_drift_all$sample_id[i],
                     start = locate[i,1] + 1, end = locate[i,2] - 1) <- "-"
  }
  ## Suppression des espaces
  stringr::str_remove_all(prim_drift_all$sample_id,
                          pattern = " ") -> prim_drift_all$sample_id

  # PRIM - DRIFT database
  ## Selection des variables d'interets
  ## et transformation du jeu de donnee pour values
  prim_drift_all %>.%
    dplyr::select(., sample_id, peak_number, sample_type,
                  date_time, filename, values) %>.%
    tidyr::gather(., key = "std_type", value = "values", values) -> prim_drift

  ## Ajout de l'id unique, modification de std_type (ex : Ptot_values --> Ptot)
  ## et selection des colonnes
  prim_drift %>.%
    dplyr::mutate(., std_type = as.character(stringr::str_split(prim_drift$std_type,
                                                                pattern = "_") %>.%
                                             lapply(., `[[`, 1)),
                     id = paste(lubridate::date(prim_drift$date_time),
                                std_type,
                                sample_id,
                                peak_number,
                                sep = "_")) %>.%
    dplyr::select(., -sample_id, -peak_number) -> prim_drift

  # Add concentration to PRIM - DRIFT dataset
  ## Selection des variables d'interets pour la creation de l'id unique et
  ## transformation du jeu de donnee pour conc
  prim_drift_all %>.%
    dplyr::select(., sample_id, date_time, peak_number, conc) %>.%
    tidyr::gather(., key = "std_type",
                     value = "concentration", conc) -> prim_drift_conc

  ## Ajout de l'id unique et selection des colonnes
  prim_drift_conc %>.%
    dplyr::mutate(., std_type = as.character(stringr::str_split(prim_drift$std_type,
                                                                 pattern = "_") %>.%
                                             lapply(., `[[`, 1)),
                     id = paste(lubridate::date(prim_drift$date_time),
                                std_type,
                                sample_id,
                                peak_number,
                                sep = "_")) %>.%
    dplyr::select(., id, concentration) -> prim_drift_conc

  # Assemblage des 2 dataframes en fonction de l'id et rearrangement des donnees
  prim_drift %>.%
    dplyr::inner_join(., prim_drift_conc, by = "id")  %>.%
    dplyr::arrange(., id, date_time, sample_type, filename, std_type, values,
                   concentration) -> prim_drift_db

  # Retirer les objets inutiles
  remove(prim_drift_all, prim_drift_conc,
         locate, drift_change, i, prim_drift )

  #### CALB ####

  # CALB_db_list
  calb_db_list <- list()

  # Vecteur avec les noms des nutrients
  stringr::str_split(values, pattern = "_") %>.%
    lapply(., `[[`, 1) %>.%
    as.character(.) -> nutri_names

  for (i in seq_along(nutri_names)){
    # identification de la ligne correspondant au nutrient
    row_num <- which(attr(obj, which = "calb_lm") == nutri_names[i])

    # Verifier de l'existance de la variable "filter_conc" et si un filtre
    # a ete applique au nutrient.
    # Si OUI, creer "filter" contenant les concentrations filtrees et "nutrient",
    # un vecteur contenant les noms des colonnes nutrient_std_old et
    # nutrient_values_old
    # Si NON, NULL est attribue a "filter" et les noms des colonnes nutrient_std
    # et nutrient_values
    if ("filter_conc" %in% names(attr(obj, which = "calb_lm")) &
        !is.null(attr(obj, which = "calb_lm")$filter_conc[[row_num]])) {

      attr(obj, which = "calb_lm")$filter_conc[[row_num]] -> filter

      c(paste(std[i],"old", sep = "_"),
        paste(values[i], "old", sep = "_")) -> nutrient

    } else {

      c(std[i], values[i]) -> nutrient
      filter <- NULL

    }

    # Selectionner les variables sur base du vecteur nutrient ce qui permet d'obtenir
    # toutes les donnees de calibration, y compris les valeurs filtrees. Ces dernières
    # seront annotees plus bas.
    # Supprimer les NA et creer calb_data
    obj[obj$sample_type == "CALB",  ] %>.%
      dplyr::select(., sample_type, date_time, nutrient) %>.%
      stats::na.omit(.) -> calb_data

    # Creer l'id unique et de la variable filename + gather
    calb_data %>.%
      dplyr::mutate(., id = paste(lubridate::date(calb_data$date_time),
                                  nutri_names[i],
                                  calb_data[[nutrient[1]]], sep = "_"),
                    filename = attr(obj,
                                    which = "metadata")$sample)  %>.%
      tidyr::gather(., key = "std_type", value = "concentration", 3) -> calb_db

    # Modifier de std_type (ex : Ptot_values --> Ptot)
    calb_db %>.%
      dplyr::mutate(., std_type = as.character(stringr::str_split(calb_db$std_type,
                                                                  pattern = "_") %>.%
                                               lapply(., `[[`, 1))) -> calb_db
    # Renommer la colonne nutrient_values
    names(calb_db)[3] <- "values"

    # Annotater des valeurs filtrees en ajoutant _old à std_type si filter non-NULL
    calb_db$std_type[calb_db$concentration %in% filter] <- paste(nutri_names[i],
                                                                 "old",
                                                                 sep = "_")
    # Selectionner les variables d'interet pour CALB_db
    calb_db %>.%
      dplyr::select(., id, date_time, sample_type, filename, std_type, values,
                    concentration) -> calb_db

    # Ajouter calb_db a calb_db_list
    calb_db_list[[i]] <- calb_db
  }

  #### CALB_DB ####

  # Fusionner calb_db_list et prim_drir_db
  dplyr::bind_rows(calb_db_list) %>.%
    dplyr::bind_rows(., prim_drift_db) -> calb_db

  # Attributes calb_db
  attr(calb_db, "class") <- attribute_list$class
  attr(calb_db, "method") <- attribute_list$method
  attr(calb_db, "calb_lm") <- attribute_list$calb_lm
  attr(calb_db, "calb_lm_old") <- attribute_list$calb_lm_old
  attr(calb_db, "metadata") <- attribute_list$metadata

  return(calb_db)

}

#' Build SAMP database
#'
#' @param obj aa3 object
#'
#' @return un dataframe contenant les donnees SAMP
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_split
#'
#' @examples

build_sampdb_aa3 <- function(obj) {

  # Stocker les attributes
  attribute_list <- attributes(obj)

  # Filtrer les donnees SAMP + Creation de la variable filename
  obj[obj$sample_type == "SAMP",] %>.%
    dplyr::mutate(., filename = attr(obj,
                                     which = "metadata")$sample) -> samp_db

  # Renommer filename en ajoutant le type de methode (organic ou inorganic)
  paste(stringr::str_split(attr(obj, which = "metadata")$sample,
                           pattern = "-")[[1]][2],
        "filename", sep = "_") -> filename

  names(samp_db)[length(samp_db)] <- filename

  # Creation du vecteur nutrient pour la selection des colonnes (new and old)
  nutrient = sort(c(conc, values))
  nutrient_old = paste(nutrient, "old", sep = "_")

  for (i in nutrient_old) {
    if (i %in% names(samp_db)) {
      nutrient <- c(nutrient, i)
    }
  }

  # Selection des variables qui constituent samp_db
  samp_db %>.%
    dplyr::select(., sample_id, sample, sample_date, nutrient, project,
                  filename, date_time, authors, comment) ->  samp_db

  # Attributes samp_db
  attr(samp_db, "class") <- attribute_list$class
  attr(samp_db, "method") <- attribute_list$method
  attr(samp_db, "calb_lm") <- attribute_list$calb_lm
  attr(samp_db, "calb_lm_old") <- attribute_list$calb_lm_old
  attr(samp_db, "metadata") <- attribute_list$metadata

  return(samp_db)
}

#' Build METADATA database
#'
#' @param obj aa3 object
#'
#' @return un dataframe contenant les metadatas et les methodes d'analyse
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_split
#'
#' @examples

build_metadb_aa3 <- function(obj) {

#### METADATA ####
  # Gestion de la variable "topic"
  if ("metadata" %in% names(attributes(obj))) {
    if (is.null(attr(obj, which = "metadata")$topic)) {
      dplyr::bind_rows(attributes(obj)$metadata[1:6]) %>.%
        dplyr::mutate(., topic = "NULL") %>.%
        as.data.frame(.) -> df
    } else {
      dplyr::bind_rows(attributes(obj)$metadata) %>.%
        as.data.frame(.) -> df
    }
  }

  # Renommer la colonne project (contient le nom du fichier)
  names(df)[2] <- "filename"

  # Creer un identifiant unique
  df %>.%
    dplyr::mutate(., id = paste(date, project, sep = "_")) %>.%
    dplyr::select(., id, project, date, filename, author,
                  comment, topic, -sample_date) -> df

#### AJOUT DES METHODES ####

  # longeur du dataframe
  df_length <- length(df)

  # Creation du vecteur channel-variable
  paste(rep(rownames(attr(obj, which = "method")), 5),
      names(attr(obj, which = "method")), sep = "_") %>.%
    sort(.) -> cnames

  # Creation de la liste pour l'extraction de l'information dans l'attribut method
  stringr::str_split(cnames, pattern = "_") -> elements

  # Ajout des éléments de l'attribut method a df
  for (i in seq_along(cnames)){

    # Identification de la ligne
    which(rownames(attr(obj,
                        which = "method")) == paste(elements[[i]][1],
                                                    elements[[i]][2],
                                                    sep = "_")) -> rownum
    # ajouter l'element cible a df
    cbind(df, attr(obj,which = "method")[rownum, elements[[i]][3]]) -> df

    # Renommer la colonne creee
    names(df)[i + df_length] <- cnames[i]
  }

  return(df)
}


#' Build aa3 DataBase
#'
#' @param obj aa3 object
#' @param conn A DBIConnection object, as returned by dbConnect()
#'
#' @return rentre les donnees aa3 dans les differentes bases de donnees
#' @export
#' @importFrom stringr str_split
#'
#' @examples
build_database_aa3 <- function(obj, con){

  # Check_1 : aa3 class
  if ( !("aa3" %in% class(obj)) ) {
    stop("class is not aa3")
  }

  # calb database
  calb_db <- build_calbdb_aa3(obj)
  print(calb_db)

  # samp database
  samp_db <- build_sampdb_aa3(obj)
  print(samp_db)

  if (stringr::str_split(attr(obj, which = "metadata")$sample,
                         pattern = "-")[[1]][2] == "orga") {
    # DATABASE ORGA
  } else if (stringr::str_split(attr(obj, which = "metadata")$sample,
                                pattern = "-")[[1]][2] == "inorga") {
    # DATABASE INORGA
  }


  # Metadata/Method database
  meta_db <- build_metadb_aa3(obj)
  print(meta_db)


}


