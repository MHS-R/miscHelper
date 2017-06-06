
theme_apa <- function(plot.box = FALSE){
 
 RMN <- "Times New Roman"
  out <- theme(
    plot.title=element_text(family=RMN, size=16, face="bold", colour="black"),
    axis.title.x=element_text(family=RMN, size=14, colour="black"),
    axis.title.y=element_text(family=RMN, size=14, angle=90, colour="black"),
    axis.text.x=element_text(family=RMN, size=13, colour="black"),
    axis.text.y=element_text(family=RMN, size=13, colour="black"),
    axis.ticks=element_line(colour="black"))
  if (!plot.box) {
    out <- out + theme(panel.background = element_rect(fill = "white",
                                                       colour = "black"), panel.border = element_rect(fill = NA,
                                                                                                      colour = "white"), axis.line = element_line())
  } else {
    out <- out + theme(panel.background = element_rect(fill = "white",
                                                       colour = "white"), panel.border = element_rect(fill = NA,
                                                                                                      colour = "grey50"))
  }
  out
}

theme_apaOrig <- function(plot.box = FALSE){
  if (Sys.info()["sysname"] != "Windows") {
    windowsFonts <- NULL
  }
  if (Sys.info()["sysname"] == "Windows") {
    windowsFonts(RMN=windowsFont("Times New Roman"))
    RMN <- "RMN"
  } else {
    RMN <- "Times New Roman"
  }
  out <- theme(
    plot.title=element_text(family=RMN, size=14, face="bold", colour="black"),
    axis.title.x=element_text(family=RMN, size=14, colour="black"),
    axis.title.y=element_text(family=RMN, size=14, angle=90, colour="black"),
    axis.text.x=element_text(family=RMN, size=11, colour="black"),
    axis.text.y=element_text(family=RMN, size=11, colour="black"),
    axis.ticks=element_line(colour="black"))
  if (!plot.box) {
    out <- out + theme(panel.background = element_rect(fill = "white",
                                                       colour = "black"), panel.border = element_rect(fill = NA,
                                                                                                      colour = "white"), axis.line = element_line())
  } else {
    out <- out + theme(panel.background = element_rect(fill = "white",
                                                       colour = "white"), panel.border = element_rect(fill = NA,
                                                                                                      colour = "grey50"))
  }
  out
}

#This is  labeller function. The argument will be the expression lables.
#eg.facet_wrap_labeller(myplot, labels = c(expression(paste("A or ", alpha)), expression(beta), expression(gamma), expression(delta)))
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}


#Calculate LRT from model components
LRtest <- function(model){
  c(LRchisq=(model$null.deviance - model$deviance), 
    df=(model$df.null - model$df.residual))
  (LR <- LRtest(icu.full))  
}
