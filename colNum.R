#' View the column numbers are associated with the data frame column variable names
#' @param df data.frame
colNum <- function(df){
  y <- rbind(seq(1,ncol(x)))
  colnames(y) <- colnames(x)
  rownames(y) <- "col.number"; return(y)
} 

#' colNum2 (fixed)
#' View the column numbers are associated with the data frame column variable names
#' @param df data.frame
#' @param wide A boolean indicating if results should be stored in wide or long format
colNum2 <- function(df, wide = FALSE){
  if (wide == FALSE) {
    ret <- data.frame(variable = colnames(df), 
                      col.number = seq(1,ncol(df)))
  } else {
    ret <- t(data.frame(col.number = seq(1, ncol(df))))
    colnames(ret) <- colnames(df)
  } 
  return(ret)
}

# EXAMPLE  
data(cars)
colNum2(cars)
colNum2(cars, wide = TRUE)
