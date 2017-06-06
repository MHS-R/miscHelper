#' Function to draw observations from stratified norm sample
#'
#' This function creates a stratified random sample, given an arbitrary number of
#' factors and levels within those factors.
#'
#' @param ... x, y, z can be matrices or data.frames
#' @references http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill
#' @export cbind.fill
cbind.fill <- function(...){
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
    rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
