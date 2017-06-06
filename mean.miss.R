#' Calculate mean with allowable missingness
#'
#' This function calculates a mean if the missingness does not exceed that maximum allowed.
#'
#' @param rowVector vector; a row vector in, say, a data frame of Cases by Variables
#' @param maxMiss numeric; a number specifying the maximum NAs that are allowed prior to calculation
#' @param items numeric; number of cases to sample from each strata
#'
mean.miss <- function(rowVector, maxMiss){  #mean.miss(dat$ha, 0)
  
  missings <- sum(is.na(rowVector))
  ifelse (missings > maxMiss, 
          meanM<-NA, meanM <- mean(rowVector, na.rm=TRUE))
  # scoreM <- round(meanM*items,0)
  return(meanM)
} 

#' Prorate subscale sum scores
#'
#' This function computes a sum score across items in the event of missingness, but also prorates. 
#'
#' @param rowVector vector; a row vector in, say, a data frame of Cases by Variables. The row vector contains ONLY columns that contribute to the sum score.
#' @param maxMiss numeric; a number specifying the maximum NAs that are allowed prior to calculation

proSumScore <- function(rowVector, maxMiss){ 
  
  missings <- sum(is.na(rowVector))
  numItemsScale <- length(rowVector)
  itemsAnswered <- numItemsScale - missings
  ifelse (missings > maxMiss, 
          sumM<-NA, sumM <- sum(rowVector, na.rm=TRUE))
  proSum <- sumM*numItemsScale/itemsAnswered
  return(proSum)
} 

