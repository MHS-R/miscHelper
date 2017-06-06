#' Add names to likert graphs
#'
#' Takes a list of dataframe column names, and long names, and stores the long names in the attribute "fullname". Then, the original names of the original dataframe get renamed with the full names.
#' This is to be used with the 'likert' package. e.g., WHen plotting, item names should appear with item content description if this function is used.
#'Example:
#'db <- add_likert_fullnames(db, c(
#'  "X7"="Do you use sites like Facebook, Twitter, or GPlus?",
#   "X8"="Do you participate in online communities organised around your interests?",
#'  "X10"="Do you know of online communities relevant to your discipline or the courses you are taking now?"))

#'
#' @param rawDf; a data frame with named columns
#' @param levels; a numeric vector indicating the number of levels per item
#' @param fnames; a character vector providing the item content names to be placed in the column names.
#' @export likert_add_fullnames.df

likert_add_fullnames.df <- function(rawDf, levels=1:6, fnames){
  # create likert-usable dataframe.
  db2 <- data.frame(lapply(rawDf, factor, levels=levels))

  # NESTED FUNCTION
  likert_add_fullnames <- function(to, fnames) {
    if(length(fnames) > length(unique(fnames))) {
      stop("All names must be unique")
    }
    for(x in names(fnames)) {
      attr(to[[x]], "fullname") <- fnames[[x]]
    }
    to
  }

  # get the full names
  db2 <- likert_add_fullnames(db2, fnames=fnames)

  #rename the columns
  for (n in names(db2)) {
    if (!is.null(attr(db2[[n]], "fullname"))) {
      names(db2)[names(db2) == n] <- attr(db2[[n]], "fullname")
    }
  }
  return(db2)
}

