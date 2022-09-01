#' Summary
#'
#' Includes a set of useful estimates for the scales
#'
#' @param object The Psychometric object
#' @param mean T: Shown, F: not shown
#' @param sd  T: Shown, F: not shown
#' @param SE  T: Shown, F: not shown
#' @param skew  T: Shown, F: not shown
#' @param kurtosis  T: Shown, F: not shown
#' @param min  T: Shown, F: not shown
#' @param max  T: Shown, F: not shown
#' @param omega  T: Shown, F: not shown
#' @param n  T: Shown, F: not shown
#' @param plots F: no plots are shown, T: plots from psych::omega are shown
#'
#' @return the summary i a list object
#' @export
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' summary(object)

summary.Psychometric<-function(object, mean = T, sd = T, SE = T, skew = T, kurtosis = T,
                               min = T, max = T, omega = T,
                               #alpha = T,
                               n = T, plots = F)
{
  y <- object$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(mean==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(sd==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(SE==TRUE)
    {sumx$SE[i]<-sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(skew==TRUE)
    {sumx$Skew[i]<-psych::skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(kurtosis==TRUE)
    {sumx$Kurtosis[i]<-psych::kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(min==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(max==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(omega==TRUE)
    {omeg<-psych::omega(object$ScaleItemFrames[[i]], plot = plots)
    sumx$Omega[i]<-as.vector(omeg$omega.tot)}
    if(n==TRUE)
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
  if (skew==TRUE)
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
  if(omega==TRUE)
  {
    mystars3 <- ifelse(summaryy$Omega < 0.75, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega > 0.75, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}

