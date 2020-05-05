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
#'
#' @note
#' [rho()] is borrowed from [seacarb::rho()], but its code is copied here
#' because do *not* want to import **seacarb** that itself depends on **oce**
#' and **gsw** that bring a lot of useless additional dependencies!
#'
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
  rho_sw <- rho(S = S, T = T, P = P)
  y <- buoyant_weight / (1 - (rho_sw / rho_aragonite))
  attributes(y) <- NULL
  y
}

#' @export
#' @rdname skeleton_weight
rho <- function(S = 35, T = 25, P = 0) {
  T68 <- (T - 2e-04) / 0.99975
  rhow <- 999.842594 + 0.06793952 * T68 - 0.00909529 * T68^2 +
    0.0001001685 * T68^3 - 1.120083e-06 * T68^4 + 6.536332e-09 *
    T68^5
  A <- 0.824493 - 0.0040899 * T68 + 7.6438e-05 * T68^2 - 8.2467e-07 *
    T68^3 + 5.3875e-09 * T68^4
  B <- -0.00572466 + 0.00010227 * T68 - 1.6546e-06 * T68^2
  C <- 0.00048314
  rho0 <- rhow + A * S + B * S^(3/2) + C * S^2
  Ksbmw <- 19652.21 + 148.4206 * T68 - 2.327105 * T68^2 + 0.01360477 *
    T68^3 - 5.155288e-05 * T68^4
  Ksbm0 <- Ksbmw + S * (54.6746 - 0.603459 * T68 + 0.0109987 *
      T68^2 - 6.167e-05 * T68^3) + S^(3/2) * (0.07944 + 0.016483 *
          T68 - 0.00053009 * T68^2)
  Ksbm <- Ksbm0 + P * (3.239908 + 0.00143713 * T68 + 0.000116092 *
      T68^2 - 5.77905e-07 * T68^3) + P * S * (0.0022838 - 1.0981e-05 *
          T68 - 1.6078e-06 * T68^2) + P * S^(3/2) * 0.000191075 +
    P * P * (8.50935e-05 - 6.12293e-06 * T68 + 5.2787e-08 *
        T68^2) + P^2 * S * (-9.9348e-07 + 2.0816e-08 * T68 +
            9.1697e-10 * T68^2)
  rho <- rho0 / (1 - P/Ksbm)
  attr(rho, "unit") = "(kg/m3)"
  rho
}
