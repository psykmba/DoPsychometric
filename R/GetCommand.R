
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
