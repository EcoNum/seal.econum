#' Compute the skeleton weight with the buoyant weight
#'
#' @description  The buoyant weight is a simply tools. This method reduces the stress during the manipulation of the coral's nubbins. The buoyant weight is used to calculate the skeleton weight. The evolution of skeleton weight can bu used to monitor the growth of the coral's nubbins.
#'
#' @param buoyant_weight buoyant weight in gramme
#' @param S salinity in PSU
#' @param T temperature in  degree Celsius
#' @param P hydrostatic pressure in bar
#' @param rho_aragonite density of the  aragonite
#'
#' @return skeleton weight
#' @importFrom seacarb rho
#' @export
#'
#' @examples
#'
#' skeleton_weight(buoyant_weight = 25, S = 35, T = 25, P = 0, rho_aragonite = 2930)
#'
#' # second example on a dataset , growth
#' data(growth) # load data
#' growth$skeleton_weight <- skeleton_weight(buoyant_weight = growth$weight,
#' S = growth$salinity, T = growth$temperature, P = 0, rho_aragonite = 2930) # compute new variable
#' growth$skeleton_weight # see the new variable
#' # more information see vignette
skeleton_weight <- function(buoyant_weight, S, T, P = 0, rho_aragonite = 2930) {
  rho_sw <- seacarb::rho(S = S, T = T, P = P)
  y <- buoyant_weight / (1 - (rho_sw / rho_aragonite))
  attributes(y) <- NULL
  y
}
