

#' summary
#' Includes a set of useful estimates for the scales
#' @param object The Psychometric object
#' @param ... mean T: Shown
#' F: not shown
#' sd  T: Shown, F: not shown
#' SE  T: Shown, F: not shown
#' skew  T: Shown, F: not shown
#' kurtosis  T: Shown, F: not shown
#' min  T: Shown, F: not shown
#' max  T: Shown, F: not shown
#' omega  T: Shown, F: not shown
#' n  T: Shown, F: not shown
#' plots F: no plots are shown, T: plots from psych::omega are shown
#' @return the summary i a list object
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' summary(object)
#' @export
summary.Psychometric <- function(object, ...)
{
  GetExtraArgument <- function(a)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(T)

  }
  amean = GetExtraArgument("mean")
  asd = GetExtraArgument("sd")
  aSE = GetExtraArgument("SE")
  askew = GetExtraArgument("skew")
  akurtosis = GetExtraArgument("kurtosis")
  amin = GetExtraArgument("min")
  amax = GetExtraArgument("max")
  aomega = GetExtraArgument("omega")
  an = GetExtraArgument("n")
  aplots = GetExtraArgument("plots")

  y <- object$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(amean==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(asd==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(aSE==TRUE)
    {sumx$SE[i]<-sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(askew==TRUE)
    {sumx$Skew[i]<-psych::skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(akurtosis==TRUE)
    {sumx$Kurtosis[i]<-psych::kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(amin==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(amax==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(aomega==TRUE)
    {omeg<-psych::omega(object$ScaleItemFrames[[i]], plot = plots)
    sumx$Omega[i]<-as.vector(omeg$omega.tot)}
    if(an==TRUE)
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-object$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (kurtosis==TRUE)
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
  if (askew==TRUE)
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
  if(aomega==TRUE)
  {
    mystars3 <- ifelse(summaryy$Omega < 0.75, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega > 0.75, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}

