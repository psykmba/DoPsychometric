

#' getSubScaleNames
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


#' getSubScaleNames
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


#' getItemText
#'
#' @param object the object to get data from
#' @param scales scales to get item text, if null all scales
#' @param printName whether it should print the item texts
#' @return a dataframe with all scales and other variables from the Psychometric object
#'
#' @export
getItemText <- function(object,  scales = NULL, printName = F) {
  UseMethod("getItemText", object)
}

#' getItemText
#'
#' @param object the object to get data from
#' @param scales scales to get item text, if null all scales
#' @param printName whether it should print the item texts
#' @return a dataframe with all scales and other variables from the Psychometric object
#'
#' @export
getItemText <- function(object, scales = NULL, printName = F)
{
  getText <- function(name)
  {
    if (isTRUE(printName))
      return (paste(object$ItemDictionary[[name]],name, sep = " | "))
    else
      return (object$ItemDictionary[[name]])

  }
  res <- list()
  if (is.null(scales))
  {

   res <- lapply(object$ScaleItemFrames, FUN = function(x)
      return(sapply(x, FUN = getText)))
  }
  else
  {

    res <- lapply(object$ScaleItemFrames[scales], FUN = function(x)
    {
      print(names(x))
      return(sapply(names(x), FUN = getText))
    })
  }

  return(res)


}



#' namesP
#'
#' @param x a Psychometric object to work with
#' @return the names of Scale and Other variables
#'
#' @examples
#' dataObject <- GetPsychometric(persData,
#' scaleNames = c("Extraversion", "Agreeableness",
#'                "Conscientiousness", "Neuroticism",
#'                "Openness"),
#' responseScale = list(c(0,36)),
#' itemLength = 4)
#' namesP(dataObject)
#' @export
namesP <- function(x)
{
  UseMethod("namesP", x)
}
#' namesP
#'
#' @param x a Psychometric object to work with
#' @return the names of Scale and Other variables
#'
#' @examples
#' dataObject <- GetPsychometric(persData,
#' scaleNames = c("Extraversion", "Agreeableness",
#'                "Conscientiousness", "Neuroticism",
#'                "Openness"),
#' responseScale = list(c(0,36)),
#' itemLength = 4)
#' names(dataObject)
#' @export
namesP.Psychometric <- function(x)
{
  print("Scales")
  print(names(x$ScaleFrame))
  print("OtherVariables")
  print(names(x$OtherVariables))
  return()
}

#' selectP
#'
#' @param object a Psychometric object
#' @param scales the scales that should be included in the result
#'
#' @return an Psychometric object with only the scales
#' @export
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
#' @export
selectP.Psychometric <- function(object, scales)
{
  newObject <- object
  newObject$ScaleFrame <- object$ScaleFrame[scales]
  newObject$ScaleItemFrames <- object$ScaleItemFrames[scales]
  newObject$ResponseScales <- object$ResponseScales[scales]
  res <- list()
  for(com in newObject$RCommands)
  {
    for(scale in scales)
    {
      if (!is.na(stringr::str_match(com,scale)))
        res <- append(res, list(com))
    }
  }
  newObject$RCommands <- res
  newObject$ScaleNames <- scales
  newObject$ResultList <- list()
  return(newObject)
}


#' select'[]'
#'
#' @param x a Psychometric object
#' @param i the index
#'
#' @return an Psychometric object only the indexed rows
#' @export
`[.Psychometric` <- function(x,i)
{
  UseMethod("[", object);
}

#' select'[]'
#'
#' @param x a Psychometric object
#' @param i the index
#'
#' @return an Psychometric object only the indexed rows
#' @export
`[.Psychometric` <- function(x,i)
{
  argnames <- sys.call()
  form <- as.character(argnames[3])
 browser()
   param <- rlang::parse_expr(form)
   return(filterP(x,villkor = form))
   data <- x
  data$ScaleFrame <- x$ScaleFrame[i,]
  data$OtherVariables <- x$OtherVariables[i,]
  data$ScaleItemFrames <- lapply(x$ScaleItemFrames, FUN = function(d)
    return(d[i,]))
  data$OriginalData <- x$OriginalData[i,]
  return(data)
}
