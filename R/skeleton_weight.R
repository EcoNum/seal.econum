#' Compute the skeleton weight with the buoyant weight
#'
#' @description  The buoyant weight is a simply tools. This method reduces the stress during the manipulation of the coral's nubbins. The buoyant weight is used to calculate the skeleton weight. The evolution of skeleton weight can bu used to monitor the growth of the coral's nubbins.
#'
#' @param buoyant_weight the buoyant weight in gramme
#' @param S salinity in PSU
#' @param T temperature in  degree Celsius
#' @param P hydrostatic pressure in bar
#' @param rho_aragonite densite de l'aragonite
#'
#' @return skeleton weight
#' @importFrom seacarb rho
#' @export
#'
#' @examples
#'
#' skeleton_weight(buoyant_weight = 25, S = 35, T = 25, P = 0, rho_aragonite = 2930)
#'
skeleton_weight <- function(buoyant_weight, S, T, P = 0, rho_aragonite = 2930){
  x <- rho(S = S, T = T, P = P)
  y <- buoyant_weight / (1 - (x / rho_aragonite))
  y
}
