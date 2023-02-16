#' Split Psychometric
#'
#' Makes it simple to work with groups
#' @param object A Psychometric object
#' @param group  A group variables among other variables
#' @param f  A function to apply to the groups
#' @param ... more arguments to the f function
#' @return all results from the f function
#' @export split.Psychometric
split.Psychometric <- function(object, group, f,...)
{
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
    res <- f(Psychometric, ...)
    lNames <- c(lNames, paste(group, as.character(data[group][1,1]), sep = ""))
    results <- append(results, list(res))
  }
  names(results) <- lNames
  return(results)

}
