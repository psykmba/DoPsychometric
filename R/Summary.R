#' summary
#'
#' Includes a set of useful estimates for the scales
#'
#' @param object The Psychometric object
#' @param ... extra arguments
#' mean T: Shown, F: not shown
#' sd  T: Shown, F: not shown
#' SE  T: Shown, F: not shown
#' skew  T: Shown, F: not shown
#' kurtosis  T: Shown, F: not shown
#' min  T: Shown, F: not shown
#' max  T: Shown, F: not shown
#' omega  T: Shown, F: not shown
#' alpha  T: Shown, F: not shown
#' n  T: Shown, F: not shown
#' plots F: no plots are shown, T: plots from psych::omega are shown
#' @return the summary i a list object
#' @export
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
  meanS <- GetExtraArgument("mean", T)
 sdS <- GetExtraArgument("sd", T)
  SES <- GetExtraArgument("SE", T)
  skewS <- GetExtraArgument("skew", T)
  kurtosisS <- GetExtraArgument("kurtosis", T)
  minS <- GetExtraArgument("min", T)
  maxS <- GetExtraArgument("max", T)
  omegaS <- GetExtraArgument("omega", T)
  alphaS <- GetExtraArgument("omega", T)
  nS <- GetExtraArgument("n", T)
  plotsS <- GetExtraArgument("plots", F)
  y <- object$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(isTRUE(meanS) )
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(isTRUE(sdS) )
    {sumx$SD[i]<-stats::sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(isTRUE(SES))
    {sumx$SE[i]<-stats::sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(isTRUE(skewS))
    {sumx$Skew[i]<-psych::skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(isTRUE(kurtosisS))
    {sumx$Kurtosis[i]<-psych::kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(isTRUE(minS))
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(isTRUE(maxS))
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if (isTRUE(alphaS))
    {
      a<-psych::alpha(object$ScaleItemFrames[[i]])
      sumx$Alpha[i]<-as.vector(a$total$raw_alpha)

    }

    if(isTRUE(omegaS))
    {
      if (length(object$ScaleItemFrames[[i]])>=7)
      {

        omeg<-psych::omega(object$ScaleItemFrames[[i]], plot = plotsS)
        sumx$Omega[i]<-as.vector(omeg$omega.tot)
        sumx$OmegaHier[i]<-as.vector(omeg$omega_h)
      }
      else
      {
        omeg<-psych::alpha(object$ScaleItemFrames[[i]])
        sumx$Alpha[i]<-as.vector(omeg$total$raw_alpha)
        warning("Number of items to small for omega (<7), alpha estimated instead")
      }
    }

    if(isTRUE(nS))
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-object$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (isTRUE(kurtosisS))
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
  if (isTRUE(skewS))
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
  if(isTRUE(omegaS))
    if (length(object$ScaleItemFrames[[i]])>=7)
    {

    mystars3 <- ifelse(summaryy$Omega < 0.75, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega < 0.75, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
    }
  else
  {
    mystars3 <- ifelse(summaryy$Alpha < 0.75, "*", "")
    summaryy$Alpha<-paste(summaryy$Alpha, mystars3, sep=" ")
    O <- ifelse(summaryy$Alpha < 0.75, NA, summaryy$Alpha )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')

  }
  print(warnings())
  return(summaryy)
}
