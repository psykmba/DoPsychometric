
#' Summary of imputeMissing
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param handleMissing A Reliability object
#' @param scales do missing on scale level
#' @param ... commands to missing
#' @return A Psychometric object that can be used for analyses
#' @examples
#' dat <- as.data.frame(list(pItem1 = c(2,3,4,4,3,4,NA,4), pItem2 = c(2,3,4,4,2,4,2,3)))
#' myObject <- GetPsychometric(dat, "p", responseScale = list(c(0,4)), itemLength = 1)
#' myObject <- imputeMissing(myObject)
#' @export

imputeMissing <- function(object, handleMissing = "Listwise", scales = T, ...) {
UseMethod("imputeMissing", object)
}

#' @export
imputeMissing.Psychometric <- function(object, handleMissing = "Listwise", scales = F,...)
{
  HandleMissing <- function(dataToHandle)
  {
    if (handleMissing == "Listwise")
    {

      return(dataToHandle[stats::complete.cases(dataToHandle),])

    }
    if (handleMissing == "Impute")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm.predict", printFlag = F)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Mean")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "mean", printFlag = F)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Bayesian")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm", printFlag = F)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Check")
    {
      print(mice::md.pattern(dataToHandle, plot = TRUE))

      return(dataToHandle)
    }
  }
  GetScalesFrame <- function(frames, nameV)
  {
    res <- NULL
    for (index in 1:length(frames))
    {
      res <- cbind(res, rowMeans(as.data.frame(frames[index]), na.rm = F))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    names(res) <- nameV
    return(res)


  }
  if (scales == T)
  {
    object$ScaleFrame <- HandleMissing(object$ScaleFrame)
  }
  else {
    for(index in 1:length(object$ScaleItemFrames))
    {
      object$ScaleItemFrames[[index]] <- HandleMissing(object$ScaleItemFrames[[index]])
    }
    object$ScaleFrame <- GetScalesFrame(object$ScaleItemFrames, object$ScaleNames)
  }

  return(object)
}
