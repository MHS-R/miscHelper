#' Calculate mode
#'
#' Calculates the mode, the most frequent value, of a variable x. This makes mostly sense for qualitative data.
#' If there are more than one mode, all of them are returned in a vector.
#' Modified from DescTools::Mode
#'
#' @param x vector containing numeric information. To do: Modify to allow for character information!
#' @param na.rm logical. Defaults to FALSE.
#' @param multiple logical. Defaults to FALSE. If FALSE, return a vector containing ALL possible modes. If TRUE, function returns the 'mean' of the mode values for numeric vectors if the difference between the two modes is 1; if there are multiple differences or if the difference between two modes is > 1, then return NA to indicate that no 'numeric' mode can be imputed.
#' @export Mode
Mode <- function (x, na.rm = FALSE, multiple=FALSE)
{
  if (!is.atomic(x) | is.matrix(x))
    stop("Mode supports only atomic vectors. Use sapply(*, Mode) instead.")
  if (na.rm)
    x <- na.omit(x)
  tab <- table(x)
  res <- names(which(tab == max(tab)))
  res <- as.numeric(res)
  if (multiple & length(res) > 1 ){
      if ( diff(res) > abs(1) | length(res) > 2){
        res<- NA
      } else
        res <- mean(res)
  }
  if (!inherits(x, "factor") & !is.na(res))
    class(res) <- class(x)
  return(as.vector(res))
}
