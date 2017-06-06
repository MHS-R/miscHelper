#' Round half up 
#'
#' R's default round() function does banker's rounding. This round2 function rounds a number half up. 
#'
#' @param x number 
#' @param digits numeric; a number specifying the number of digits to round to.
#' @export round2
round2 = function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}