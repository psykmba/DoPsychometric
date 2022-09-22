filter.Psychometric<-function(object, keep)
{
  if (nrow(object$ScaleFrame) != length(keep))
  {
    print("Logical vector not the same length as frames")
    return()
  }
  object$ScaleFrame <- dplyr::filter(object$ScaleFrame, keep)
  object$OtherVariables <- dplyr::filter(object$OtherVariables, keep)
  object$OriginalData <- dplyr::filter(object$OriginalData, keep)
  for(index in 1:length(object$ScaleItemFrames))
    object$ScaleItemFrames[[index]] <-
    dplyr::filter(object$ScaleItemFrames[[index]], keep)
  return(object)
}

MakeMissing <- function(data, miss)
{
  if (is.null(miss))
    return(data)
  data <- naniar::replace_with_na_all(data, ~.x %in% miss)
  return(data)
}



#' Handle outliers methods
#'
#' @param object a Psychometric object to work with
#' @param method there are three ways, "Mahalanobis", "SD" and "Change"
#' @param limit the probability value for handeling an outlier
#' @param missing when "none", missing values are not handled, otherwise the method in missing will be used
#' @param otherVar whether the variables not among the scales shall be included, only for check and SD
#'
#' @return an updated Psychometric object
#'
#' @export
handleOutliers <- function(object, method = "Mahalanobis", limit = .001,
                           missing = "None", otherVar = F) {
  UseMethod("handleOutliers", object)
}

#' Handle outliers methods
#' @param object a Psychometric object to work with
#' @param method there are three ways, "Mahalanobis", "SD" and "Change"
#' @param limit the probability value for handeling an outlier
#' @param missing when "none", missing values are not handled, otherwise the method in missing will be used
#' @param otherVar whether the variables not among the scales shall be included, only for check and SD
#' @return an updated Psychometric object
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' newObject <- handleOutliers(object)
#' @export
handleOutliers.Psychometric <- function(object, method = "Mahalanobis", limit = .001,
                                        missing = "None", otherVar = F)
{
  getInsideRange <- function(s, r)
  {
    return (ifelse(s >= r[1] & s <= r[2], s, ifelse(s < r[1], r[1], r[2])))
  }
  deleteOutsideRange <- function( s, r)
  {
    return(ifelse (s < r[1], NA, ifelse(s > r[2], NA, s) ))
  }
  if (method == "Mahalanobis") {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    scaleCor <- stats::cov(noMissObject$ScaleFrame)
    Outliers <- stats::mahalanobis(noMissObject$ScaleFrame, colMeans(noMissObject$ScaleFrame), scaleCor)
    object <- dplyr::filter(noMissObject, Outliers < stats::qchisq(1-limit, length(object$ScaleNames)))
    return(object)
  }
  if (method == "SD")
  {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    newFrame <- NULL
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, deleteOutsideRange(scale, r))
    }
    noMissObject$ScaleFrame <- newFrame
    if (isTRUE(otherVar))
    {
      newFrame <- NULL
      for(var in noMissObject$OtherVariables)
      {
        if (is.numeric(var))
        {
          m <- mean(var)
          sd <- sd(var) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          newFrame <- cbind(newFrame, deleteOutsideRange(scale, r))
        }
        else
        {
          newFrame <- cbind(newFrame, var)
        }
      }
      noMissObject$OtherVariables <- newFrame
    }

    return(imputeMissing(noMissObject, scales = T))

  }
  if (method == "Winsorizing" || method == "Change")
  {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    newFrame <- NULL
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, getInsideRange(scale, r))
    }
    noMissObject$ScaleFrame <- newFrame
    if (isTRUE(otherVar))
    {
      newFrame <- NULL
      for(v in noMissObject$OtherVariables)
      {
        if (is.numeric(v))
        {
          m <- mean(v)
          sd <- sd(v) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          newFrame <- cbind(newFrame, getInsideRange(v, r))
        }
        else
        {
          newFrame <- cbind(newFrame, v)
        }
      }
      noMissObject$OtherVariables <- newFrame
    }

    return(noMissObject)
  }
}

#' Names
#'
#' @param x a Psychometric object to work with
#' @return the names of Scale and Other variables
#'
#' examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#' names(object)
#' @export names.Psychometric

names.Psychometric <- function(x)
{
  print("Scales")
  print(names(x$ScaleFrame))
  print("OtherVariables")
  print(names(x$OtherVariables))
  return()
}

#' Gets data from Psychometric object
#'
#' @param object the object to get data from
#'
#' @return a dataframe with all scales and other variablesof the Psychometric object
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' data <- getData(object)
#' @export
getData <- function(object, items) {
  UseMethod("getData", object)
}

#' @export
getData.Psychometric <- function(object, items = F)
{
  if (!items)
    return(as.data.frame(cbind(object$ScaleFrame, object$OtherVariables)))
  else
  {
    itemFrames <- data.frame(row.names = 1:nrow(object$ScaleItemFrames[[1]]))
    for(itemFrame in object$ScaleItemFrames)
      itemFrames <- cbind(itemFrames, itemFrame)
    return(cbind(object$ScaleFrame, object$OtherVariable,as.data.frame(itemFrames)))
  }

}

#' Write all data
#'
#' @param object A psychometric object
#' @param fileName A filename with pathinformation
#' @param col.names T if you like to write column names
#' @param row.names T if you like to write row names
#'
#' @return NULL
#' @export write.csv.Psychometric
write.csv.Psychometric <- function(object, fileName, colnames = T, rownames = F)
{
  write.csv(x = getData(object, items = T),file = fileName)
  return(NULL)

}

#' Get commands
#'
#' @param object A reliability object
#' @param scale Either "All" or a scale among those in ScaleNames
#'
#' @return a character string
#' @export getCommand
getCommand <- function(object, ...){
  UseMethod("getCommand", object)
}

#' Get the reliability commands
#'
#' @param object A reliability object
#' @param scale Either "All" or a scale among those in ScaleNames
#'
#' @return a character string
#' @export getCommand.Reliability
getCommand.Reliability <- function(object, ...)
{
  GetExtraArgument <- function(a)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return("All")

  }
  scale = GetExtraArgument("scale")
  res <- ""
  if (scale == "All")
  {
    for (s in object$ScaleNames)
    {
      res <- paste(res, object$RCommands[[s]][1], "\n", rep = "")
    }
    return(cat(paste(res, collapse = "")))
  }
  else
  {
    return(cat(paste(object$RCommands[[scale]], collapse = "")))
  }
}


#' getCommand for Psychometric
#'
#' @param object a Psychometric object
#' @param ...
#'
#' @return text with commands to use
#' @export getCommand.Psychometric
getCommand.Psychometric <- function(object, ...)
{
  res<-""
  for (commands in object$RCommands)
  {
    res <- paste(res, commands[1], sep = "")
  }
  return(cat(paste(res, collapse = "")))
}

#' getCommand TestFacets
#'
#' @param object a TestFacets object created by TestFacets function
#' @param ... model either "All" or a number
#'
#' @return  text with commands to use
#' @export getCommand.TestFacets
getCommand.TestFacets <- function(object, ...)
{
  GetExtraArgument <- function(a)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return("All")

  }
  model = GetExtraArgument("model")

  if (model == "All")
    return(cat(paste(object$RCommands, collapse = "")))
  else
    return(cat(paste(object$RCommands[model], collapse = "")))
}

