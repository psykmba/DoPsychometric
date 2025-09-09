#' split Psychometric
#'
#' Makes it simple to work with groups
#' @param object A Psychometric object
#' @param f  A function to apply to the groups
#' @param drop logical indicating if levels that do not occur should be dropped (if f is a factor or a list).
#' @param ... more arguments to the f function
#' @return all results from the f function
#' @export
split.Psychometric <- function(x, f, drop = FALSE,  ...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  group <- GetExtraArgument("Grupp", NULL)
  if (is.null(group))
      {
      print("No group argument to the split function")
        return()
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
