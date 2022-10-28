#' Summary
#'
#' Includes a set of useful estimates for the scales
#'
#' @param object The Psychometric object
#' @param ... mean T: Shown, F: not shown
#' sd  T: Shown, F: not shown
#'  SE  T: Shown, F: not shown
#' skew  T: Shown, F: not shown
#'  kurtosis  T: Shown, F: not shown
#'  min  T: Shown, F: not shown
#'  max  T: Shown, F: not shown
#'  omega  T: Shown, F: not shown
#'  n  T: Shown, F: not shown
#'  plots F: no plots are shown, T: plots from psych::omega are shown
#' @return the summary i a list object
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' summary(object)
#' @export summary.Psychometric
summary.Psychometric<-function(object, ...)
{
  GetExtraArgument <- function(a, default)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  mean.s <- GetExtraArgument("mean", T)
  sd.s <- GetExtraArgument("sd", T)
  SE.s <- GetExtraArgument("SE", T)
  skew.s <- GetExtraArgument("skew", T)
  kurtosis.s <- GetExtraArgument("kurtosis", T)
  min.s <- GetExtraArgument("min", T)
  max.s <- GetExtraArgument("max", T)
  omega.s <- GetExtraArgument("omega", T)
  n.s <- GetExtraArgument("n", T)
  plots.s <- GetExtraArgument("plots", T)
  y <- object$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(mean.s==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(sd.s==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(SE.s==TRUE)
    {sumx$SE[i]<-sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(skew.s==TRUE)
    {sumx$Skew[i]<-psych::skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(kurtosis.s==TRUE)
    {sumx$Kurtosis[i]<-psych::kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(min.s==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(max.s==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(omega.s==TRUE && length(object$ScaleItemFrames[[i]])>5)
    {

       omeg<-psych::omega(object$ScaleItemFrames[[i]], plot = plots.s)
       sumx$Omega[i]<-as.vector(omeg$omega.tot)
    }
    else
    {
      omeg<-psych::alpha(object$ScaleItemFrames[[i]])
      sumx$Omega[i]<-as.vector(omeg$total$raw_alpha)
      warning("Number of items to small for omega (<6), alpha estimated instead")
    }

    if(n.s==TRUE)
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-object$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (kurtosis.s==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars <- ifelse(summaryy$Kurtosis > 2, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 2, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }
    else
    {
      mystars <- ifelse(summaryy$Kurtosis > 4, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 4, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }

  }
  if (skew.s==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars2 <- ifelse(summaryy$Skew > 1, "*", "")
      summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
      k <- ifelse(summaryy$skew > 1, NA, summaryy$skew )
      if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')
    }
    else
      mystars2 <- ifelse(summaryy$Skew > 2, "*", "")
    summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
    k <- ifelse(summaryy$skew > 2, NA, summaryy$skew )
    if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')

  }
  if(omega.s==TRUE)
  {
    mystars3 <- ifelse(summaryy$Omega < 0.75, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega < 0.75, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}

