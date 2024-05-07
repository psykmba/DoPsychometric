#' Split Psychometric
#'
#' Makes it simple to work with groups
#' @param object A Psychometric object
#' @param f  A function to apply to the groups
#' @param group  A group variables among other variables
#' @param ... more arguments to the f function
#' @return all results from the f function
#' @export
splitP <- function(object, f = NULL, group, ...)
{
  UseMethod("splitP", object)
}
#' Split Psychometric
#'
#' Makes it simple to work with groups
#' @param object A Psychometric object
#' @param f  A function to apply to the groups
#' @param group  A group variables among other variables
#' @param ... more arguments to the f function
#' @return all results from the f function
#' @export
splitP.Psychometric <- function(object, f = NULL, group, ...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  splitDataFrames <- split(object$OriginalData, object$OriginalData[c(group)])
  results <- list()
  lNames <- c()
  for (data in splitDataFrames)
  {
    print(paste("Here are results for group variable ", group,
                "category", data[1, group]))
    Psychometric <- GetPsychometric(data, object$ScaleNames,
                                    responseScale = object$ResponseScales,
                                    itemLength = object$ItemLength,
                                    reverse = F, name = paste(group, data[1, group]))
    if (!is.null(f))
      res <- f(Psychometric, ...)
    else
      res <- Psychometric
    lNames <- c(lNames, paste(group, as.character(data[group][1,1]), sep = ""))
    results <- append(results, list(res))
  }
  names(results) <- lNames
  return(results)

}
