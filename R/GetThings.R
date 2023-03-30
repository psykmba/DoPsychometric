

#' Title
#'
#' @param object a Psychometric object
#'
#' @return a list of all names starting with the
#' @details When the ScaleItemFrames is created it changes the variable names of
#' the items. This functions return all the names together with the scale names.
#' The functions can be used to check that everything has been defined correctly.
#' @export
getSubScaleNames <- function(object)
{
  UseMethod("getSubScaleNames", object)
}


#' Title
#'
#' @param object a Psychometric object
#'
#' @return a list of all names starting with the
#' @details When the ScaleItemFrames is created it changes the variable names of
#' the items. This functions return all the names together with the scale names.
#' The functions can be used to check that everything has been defined correctly.
#' @export
getSubScaleNames.Psychometric <- function(object)
{
  res <- list()
  for(index in 1:length(object$ScaleNames))
  {
    nam2 <-  names(object$ScaleItemFrames[[index]])
    res <- append(res, list(c(nam2)))

  }
  names(res) <- object$ScaleNames
  return(res)
}




#' getData
#'
#' @param object the object to get data from
#' @param scales scales to extract, if null all scales
#' @param otherVar  if true also other variables
#' @param itemFrames  if true also items
#' @param scaleFrame  if true also scales
#' @return a dataframe with all scales and other variables from the Psychometric object
#'
#' @export
getData <- function(object,  scales = NULL, otherVar = T, scaleFrame = T, itemFrames = T) {
  UseMethod("getData", object)
}

#' getData
#'
#' @param object the object to get data from
#' @param scales scales to extract, if null all scales
#' @param otherVar  if true also other variables
#' @param itemFrames  if true also items
#' @param scaleFrame  if true also scales
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

#' selectP
#'
#' @param object a Psychometric object
#' @param scales the scales that should be included in the result
#'
#' @return an Psychometric object with only the scales
selectP <- function(object, scales)
{
  UseMethod("selectP", object);
}

#' select
#'
#' @param object a Psychometric object
#' @param scales the scales that should be included in the result
#'
#' @return an Psychometric object with only the scales
selectP.Psychometric <- function(object, scales)
{
  newObject <- object
  newObject$ScaleFrame <- object$ScaleFrame[scales]
  newObject$ScaleItemFrames <- object$ScaleItemFrames[scales]
  return(newObject)
}
