#' Summary of plotScale.Psychometric
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale A Reliability object
#' @param external  All, Alpha, Omega
#' @param group  All, Alpha, Omega
#' @param type  function
#' @param main  function
#' @param xlab  function
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
plotScale <- function(object, scale = "All", group = NULL,
                      external = NULL,
                      type = "Histogram", main = "", xlab = "", ...) {
  UseMethod("plotScale", object)
}



#' @export
plotScale.Psychometric <- function(object, scale = "All", group = NULL,
                                   external = NULL,
                                   type = "Histogram", main = "", xlab = "", ...)
{
  if(is.null(group) && is.null(external))
  {
    if (scale %in% object$ScaleNames)
    {
      if (type == "Histogram" || missing(type))
      {
        hist(object$ScaleFrame[[scale]],
             main = ifelse(missing(main),
                           paste("Histogram of", scale, object$Name),
                           main),
             xlab = ifelse(missing(xlab), scale, xlab))
      }
      if (type == "Boxplot")
      {
        ggplot2::ggplot(data = object$ScaleFrame[scale], aes_string(y = scale))+
          ggplot2::geom_boxplot() +
          ggplot2::ggtitle(paste("Distribution of ", object$Name))
      }
    }
    else
      print("Wrong scale name")
  }
  else
  {
    if (scale != "All" && is.null(external))
    {
      if(group %in% names(object$OtherVariables))

        if (type == "Boxplot")
        {
          f <- formula(paste(scale, "~", group))
          d <- cbind(object$ScaleFrame[scale], factor(object$OtherVariables[group]))


          boxplot(f, data = d)
        }
    }
    else
    {
      if (!is.null(external))
      {
        if (type == "Scatter" && scale %in% names(object$ScaleFrame) &&
            external %in% names(object$OtherVariables))
        {
          d <- cbind(object$ScaleFrame[scale], object$OtherVariables[external])
          ggplot2::ggplot(data = d, aes_string(x = scale, y = external)) +
            ggplot2::geom_point(col = "gray") +
            ggplot2::ggtitle(paste("Distribution of ", object$Name))
        }

      }
    }

  }


}

#' Plot scales rom Psychometric
#'
#' @param object a Psychometric object
#' @param ... extra argument is: scale = scaleName
#'
#' @return nothing
#' @export
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#'  plot(object, scale = "Achievement")
plot.Psychometric <- function(object, ...)
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
  if (scale == "All")
  {

    for(data in object$ScaleItemFrames )
    {
      corr <- cor(data, use = "pairwise.complete.obs");

      print(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                                   lab = TRUE))
    }
  }
  if (scale != "All")
  {
    if (any(scale == object$ScaleNames))
    {
      data <- object$ScaleItemFrames[[scale]]
      corr <- cor(data, use = "pairwise.complete.obs")
      print(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                             lab = TRUE))
    }

  }

}

