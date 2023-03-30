
#' Summary of imputeMissing
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param handleMissing can be:  Listwise, Mean, Impute, Bayesian, Regression, Pmm, BayesianMean, and check
#' @param scales T = do missing on scale level F = on item level
#' @param ... k = can be used to create BayesianMean based on k imputations
#'            printFlag = T can be used to get information about imputations
#' @return A Psychometric object that can be used for analyses
#' @details Essentially this is a wrapper for the mice package. It takes a
#' Psychometric object and impute (or deletes listwise) data-cells that are missing.
#' One feature is that you can impute also on item level and if you do, the
#' ScaleFrame is automatically re-estimated based on the new item columns. The
#' imputation is in that case based on all items of a scale. Otherwise imputation
#' is based on all scales in the scale frame.
#' The function do not handle stochastic imputation using more than one dataframe,
#' maybe it will be implemented in the future.
#' @export

imputeMissing <- function(object, handleMissing = "Listwise", scales = T, ...) {
  UseMethod("imputeMissing", object)
}

#' @export
imputeMissing.Psychometric <- function(object, handleMissing = "Listwise", scales = T,...)
{
  GetExtraArgument <- function(a, default)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  pf <- GetExtraArgument("printFlag", F)
  k <- GetExtraArgument("k", 10)
  HandleMissing <- function(dataToHandle)
  {
    if (handleMissing == "Listwise")
    {

      return(dataToHandle[stats::complete.cases(dataToHandle),])

    }
    if (handleMissing == "Pmm")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "pmm", printFlag=pf)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Regression")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm.predict", printFlag=pf)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Impute")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm.nob", printFlag=pf)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Mean")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "mean", printFlag=pf)
      return(mice::complete(imputed))

    }
    if (handleMissing == "Bayesian")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm", printFlag=pf)
      return(mice::complete(imputed))

    }
    if (handleMissing == "BayesianMean")
    {
      imputed <- mice::mice(dataToHandle, m = k, method = "norm", printFlag=pf)
      imputed <-  mice::complete(imputed, "all")
      sumStart <- imputed[[1]]
      for(index in 2:k)
      {
        sumStart <- sumStart + imputed[[index]]
      }
      sumStart <- sumStart / k


      return(sumStart)

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

    NewScaleFrame <- HandleMissing(object$ScaleFrame)
    if (isTRUE(pf))
      print(summary(arsenal::comparedf(NewScaleFrame, object$ScaleFrame)))
    object$ScaleFrame <- NewScaleFrame
  }
  else {
     for(index in 1:length(object$ScaleItemFrames))
    {
       object$ScaleItemFrames[[index]] <- HandleMissing(object$ScaleItemFrames[[index]])

     }
    object$ScaleFrame <- GetScalesFrame( object$ScaleItemFrames, object$ScaleNames)

  }

  return(object)
}
