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

#' Title
#'
#' @param object a Psychometric object
#' @param scales the scales that should be included in the result
#'
#' @return an Psychometric object with only the scales
select.Psychometric <- function(object, scales)
{
  newObject <- object
  newObject$ScaleFrame <- object$ScaleFrame[scales]
  newObject$ScaleItemFrames <- object$ScaleItemFrames[scales]
  return(newObject)
}

GetItemWithParcels <- function(object, parcel, subscales)
{
  GetItemNum <- function(n, maxItems)
  {
    res <- maxItems
    if ((n - (maxItems*2)) == (maxItems-1))
      return(c(maxItems, maxItems*2, n))
    else
      return(c(maxItems,  maxItems*2-1, n))
  }
  GetRandomItemNum <- function( itemVector, nLength)
  {

    itemNums <- sample(1:nLength, nLength, replace = F)
    return(list(itemNums[1:itemVector[1]],
                itemNums[(itemVector[1]+1):itemVector[2]],
                itemNums[(itemVector[2]+1):itemVector[3]]))

  }
  MakeParcels <- function(data)
  {
    if (isTRUE(parcel))
    {
      s <- ncol(data) / 3
      numItemVector <- 0
      if (trunc(s) == s)
        numItemVector <- c(s,s*2,s*3)
      else
      {
        maxItems <- trunc(s) + 1
        numItemVector <- GetItemNum(ncol(data), maxItems)
      }
      randomParcelNum<- GetRandomItemNum( numItemVector, ncol(data))
      return(list(data[randomParcelNum[[1]]],
                  data[randomParcelNum[[2]]],
                  data[randomParcelNum[[3]]]))
    }
    else
    {
      res <- list()
      for (index in 1:ncol(data))
        res <- append(res, list(data[index]))
      return(res)
    }
  }
  CombineParcels <- function(data)
  {
    res <- data.frame(row.names = 1:nrow(data[[1]]))
    for (d in data)
    {
      if(ncol(d) == 1)
      {
        newColumn <- data.frame(d)
      }
      else
      {
        newColumn <- data.frame(rowMeans(d))
      }
      names(newColumn) <- names(d[1])
      res <- cbind(res, newColumn)
    }
    return(res)
  }
  res <- list()

  object <- select.Psychometric(object, subscales)
  for(subscale in subscales)
  {
    dataFrames <- object$ScaleItemFrames[[subscale]]
    dataFrames <- MakeParcels(dataFrames)
    dataFrames <- list(CombineParcels(dataFrames))

    names(dataFrames) <- subscale
    res <- append(res, dataFrames)
  }
  return(res)
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
#' @export
handleOutliers.Psychometric <- function(object, method = "Mahalanobis", limit = .001,
                                        missing = "None", otherVar = c())
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
    newFrame <-  data.frame(row.names = 1:nrow(object$ScaleFrame))
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, deleteOutsideRange(scale, r))
    }
    noMissObject$ScaleFrame <- newFrame
    if (length(otherVar) > 0)
    {
      newFrame <-  data.frame(row.names = 1:nrow(object$ScaleFrame))
      for(v in otherVar)
      {
        if (is.numeric(noMissObject$OtherVariables[v]))
        {
          m <- mean(noMissObject$OtherVariables[v])
          sd <- sd(noMissObject$OtherVariables[v]) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          newFrame <- cbind(newFrame, deleteOutsideRange(noMissObject$OtherVariables[v], r))
        }
        else
        {
          newFrame <- cbind(newFrame, noMissObject$OtherVariables[v])
        }
      }
      noMissObject$OtherVariables <- newFrame

    }


    return(noMissObject)
  }
  if (method == "Winsorizing" || method == "Change")
  {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    newFrame <- data.frame(row.names = 1:nrow(object$ScaleFrame))
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, getInsideRange(scale, r))
    }
    names(newFrame) <- object$ScaleNames
    noMissObject$ScaleFrame <- newFrame
    if (length(otherVar) > 0)
    {
      newFrame <-  data.frame(row.names = 1:nrow(object$ScaleFrame))

      newFrame <- data.table::data.table(row.names = 1:nrow(object$ScaleFrame))
      otherVar <- lapply(otherVar, function(v) {
        ifelse(is.numeric(noMissObject$OtherVariables[v]), {
          m <- mean(noMissObject$OtherVariables[v], na.rm = TRUE)
          sd <- sd(noMissObject$OtherVariables[v], na.rm = TRUE) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          noMissObject$OtherVariables[v][which(noMissObject$OtherVariables[v] >= r[1] & noMissObject$OtherVariables[v] <= r[2])]
        }, {
          noMissObject$OtherVariables[v]
        })
      })
      newFrame <- cbind(newFrame, otherVar)
      for(v in otherVar)
      {
        if (is.numeric(noMissObject$OtherVariables[v]))
        {
          m <- mean(noMissObject$OtherVariables[v])
          sd <- sd(noMissObject$OtherVariables[v]) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          newFrame <- cbind(newFrame, getInsideRange(noMissObject$OtherVariables[v], r))
        }
        else
        {
          newFrame <- cbind(newFrame, noMissObject$OtherVariables[v])
        }
      }
      noMissObject$OtherVariables <- newFrame




    }
    return(noMissObject)
  }
  return(object)

}
#' Names
#'
#' @param x a Psychometric object to work with
#' @return the names of Scale and Other variables
#'
#' examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#' names(object)
#' @export

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
#' @param scales scales to extract, if null all scales
#' @param itemFrames  if true also items
#' @param scaleFrame  if true also scales
#' @param otherVar  if true also other variables
#' @return a dataframe with all scales and other variables from the Psychometric object
#'
#' @export
getData <- function(object,  scales = NULL, otherVar = T, scaleFrame = T, itemFrames = T) {
  UseMethod("getData", object)
}

#' Gets data from Psychometric object
#'
#' @param object the object to get data from
#' @param scales scales to extract, if null all scales
#' @param itemFrames  if true also items
#' @param scaleFrame  if true also scales
#' @param otherVar  if true also other variables
#' @return a dataframe with all scales and other variables from the Psychometric object
#'
#' @export
getData.Psychometric <- function(object, scales = NULL,  otherVar = T, scaleFrame = T, itemFrames = T)
{
  if (is.null(scales))
  {
    res <- data.frame(row.names = 1:nrow(object$ScaleItemFrames[[1]]))

    if (isTRUE(scaleFrame))
      res <- cbind(res, object$ScaleFrame)
    if (isTRUE(otherVar))
      res <- cbind(res, object$OtherVariables)
    if (isTRUE(itemFrames))
    {
      for(itemFrame in object$ScaleItemFrames)
        res <- cbind(res, itemFrame)
    }

    return(res)
  }
  else
  {
    res <- data.frame(row.names = 1:nrow(object$ScaleItemFrames[[1]]))

    if (isTRUE(scaleFrame))
      res <- cbind(res, object$ScaleFrame[scales])
    if (isTRUE(otherVar))
      res <- cbind(res, object$OtherVariables)
    if (isTRUE(itemFrames))
    {
      for(itemFrame in object$ScaleItemFrames[scales])
        res <- cbind(res, itemFrame)
    }

    return(res)


  }

}


#' Write all data
#'
#' @param object A psychometric object
#' @param fileName A filename with pathinformation
#' @param colnames T if you like to write column names
#' @param rownames T if you like to write row names
#'
#' @return NULL
#' @export
writeP <- function(object, fileName, colnames = T, rownames = F) {
  UseMethod("writeP", object)
}

#' Write all data
#'
#' @param object A psychometric object
#' @param fileName A filename with pathinformation
#' @param colnames T if you like to write column names
#' @param rownames T if you like to write row names
#'
#' @return NULL
#' @export
writeP.Psychometric <- function(object, fileName, colnames = T, rownames = F)
{
  utils::write.csv(x = getData(object, itemFrames = T),file = fileName, col.names = colnames,
            row.names = rownames)
  return(NULL)

}

#' Get commands
#'
#' @param object A reliability object
#' @param ... scale Either "All" or a scale among those in ScaleNames
#'
#' @return a character string
#' @export getCommand
getCommand <- function(object, ...){
  UseMethod("getCommand", object)
}

#' Get the reliability commands
#'
#' @param object A reliability object
#' @param ...  scale Either "All" or a scale among those in ScaleNames
#'
#' @return a character string
#' @export
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
#' @param ... more commands
#'
#' @return text with commands to use
#' @export
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
#' @export
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

#' Title
#'
#' @param object a Psychometric object
#'
#' @return names of all scale and items
#' @export
getSubScaleNames <- function(object)
{
  res <- list()
  for(index in 1:length(object$ScaleNames))
  {
    nam1 <- object$ScaleNames[index]
    nam2 <-  names(object$ScaleItemFrames[[index]])
    res <- append(res, list(c(nam1,nam2)))

  }
  return(res)
}
