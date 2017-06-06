#' View the column numbers are associated with the data frame column variable names
#' @param df data.frame
colNum <- function(df){
  y <- rbind(seq(1,ncol(x)))
  colnames(y) <- colnames(x)
  rownames(y) <- "col.number"; return(y)
} 