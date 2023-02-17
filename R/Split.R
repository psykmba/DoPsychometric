#' Split Psychometric
#'
#' Makes it simple to work with groups
#' @param x A Psychometric object
#' @param drop  A group variables among other variables
#' @param f  A function to apply to the groups
#' @param ... group to split for and more arguments to the f function
#' @return all results from the f function
#' @export
split.Psychometric <- function(x, f, drop, ...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  group = GetExtraArgument("group")
  f = GetExtraArgument("f")
  splitDataFrames <- split(x$OriginalData, x$OriginalData[c(group)])
  results <- list()
  lNames <- c()
  for (data in splitDataFrames)
  {
    print(paste("Here are results for group variable ", group,
                "category", data[1, group]))
    Psychometric <- GetPsychometric(data, x$ScaleNames,
                                    responseScale = x$ResponseScales,
                                    itemLength = x$ItemLength,
                                    reverse = F, name = paste(group, data[1, group]))
    res <- f(Psychometric, ...)
    lNames <- c(lNames, paste(group, as.character(data[group][1,1]), sep = ""))
    results <- append(results, list(res))
  }
  names(results) <- lNames
  return(results)

}
