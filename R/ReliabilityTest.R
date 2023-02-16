#' Getting the Reliability object based on a Psychometric object
#'
#' Makes it simple to do basic psychometrics
#' @param object A Psychometric object
#' @param what what type of analyses: Alpha, Omega or Parallel
#' @param check whether to check keys in alpha
#' @param imp how to impute variables in alpha:
#' @param ... more arguments
#' @return A Reliability object based on a Psychometric object that can be used for analyses
#' @export
GetReliabilityTest <- function(object, what = "Alpha", check = T, imp = "NULL", ...)
{
  reslist2 <- NULL
  printres <- NULL
  GetAlphaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      print(deparse(substitute(object)))
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::alpha(object$OriginalData[cs(", n, ")], check.keys = T,keys=NULL,",
                                    "cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, ",
                                    "n.iter=1,delete=TRUE,use='pairwise',warnings=TRUE,",
                                    "n.obs=NULL)", sep = "")))
    }
    return(res)
  }
  GetOmegaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::omega(object$OriginalData[cs(", n, ")],nfactors=3,fm='minres',n.iter=1,",
                                    "p=.05,poly=FALSE,key=NULL,flip=TRUE,digits=2, title='Omega',",
                                    "sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate='oblimin',",
                                    "Phi=NULL,option='equal',covar=FALSE)", sep = "")))
    }
    return(res)

  }
  GetParallelCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::fa.parallel(object$OriginalData[cs(", n, ")]", sep = "")))
    }
    return(res)

  }
  if (what == "Alpha")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = function(x) {return(psych::alpha(x, check.keys = check))})
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$total$raw_alpha)))
    object$RCommands <- GetAlphaCommands()
    object$Name <- "Alpha"

  }

  else if (what == "Omega")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=FALSE)
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$omega_h)))
    object$RCommands <- GetOmegaCommands()
    object$Name <- "Omega"
  }
  else if (what == "Parallel")
  {
    resList2 <- mapply(object$ScaleItemFrames, FUN =
                         function(x,name)
                         {
                           print(name)
                           list(psych::fa.parallel(x, main = paste("Analysis for ", name), plot = F))},
                       names(object$ScaleItemFrames))

    printres <- as.data.frame(lapply(resList2, FUN = function(x,y) return(x$nfact)))
    object$RCommands <- GetParallelCommands()
    object$Name <- "Parallel"


  }
  else
    return(print("There was no such function"))


  names(resList2) <- object$ScaleNames
  names(printres) <- object$ScaleNames
  rownames(printres) <- what
  object$ResultList <- resList2
  object$PrintRes <- printres

  names(object$RCommands) <- object$ScaleNames
  class(object) <- c( "Reliability", "Psychometric")
  return(object)
}


#' Summary for Reliability
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
summary.Reliability <- function(object, ...)
{
  arg <- list(...)
  if ("scale" %in% names(arg))
    scale <- arg$scale
  else
    scale <- NULL

  if (is.null(scale))
    print(object$ResultList)
  else
  {
    if (scale %in% names(object$ScaleFrame))
      print(object$ResultList[scale])
  }
}

#' Prints the results of the Reliabilty object
#'
#' Makes it simple to do basic psychometrics
#' @param x A Reliability object
#' @param ... if not NULL it only plots for scale
#'
#' @return A Reliability object that can be used for analyses
#' @export
print.Reliability <- function(x, ...)
{
  print(x$PrintRes)
}

#' Summary of plot
#'
#' Makes it simple to do basic psychometrics
#' @param x A Reliability object
#' @param ... scale if not NULL it only plots for scale
#' @return A Reliability object that can be used for analyses
#' @export
plot.Reliability <- function(x, ...)
{
 arg <- list(...)
 if ("scale" %in% names(arg))
   scale <- arg$scale
 else
   scale <- NULL
  if (class(x)[1] == "Reliability")
  {
    if (x$Name == "Alpha")
    {
      if (is.null(scale))
      {
        lapply(x$ScaleItemFrames, FUN = psych::corPlot)
      }
      else
      {
      print(psych::corPlot(x$ScaleItemFrames[[scale]]))
      }
    }
    if (x$Name == "Omega")
    {
      if (is.null(scale))
      {
        lapply(x$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=T)
      }
      else if (!is.null(x$ScaleItemFrames[[scale]]))
        psych::omega.diagram(x$ResultList[[scale]])
      #      omega(object$ScaleItemFrames[[scale]], nfactors = 3, plot = T)
    }
    else if (x$Name == "Parallel")
    {
      if (is.null(scale))
      {
        for (frame in x$ScaleItemFrames)
        {
          print(psych::fa.parallel(frame, main = paste("Analysis for ", names(frame)), plot = T))
        }
      }
      else
        psych::fa.parallel(x$ScaleItemFrames[[scale]], main = paste("Analysis for ", scale), plot = T)
    }

  }
}

