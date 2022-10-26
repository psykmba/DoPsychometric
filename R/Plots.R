#' Summary of plotScale
#'
#' Makes it simple to do basic psychometrics
#' @param object A Psychometric object
#' @param scale A Psychometric object
#' @param group  All, Alpha, Omega
#' @param external  All, Alpha, Omega
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


#' Summary of plotScale.Psychometric
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale A Reliability object
#' @param group  All, Alpha, Omega
#' @param external  All, Alpha, Omega
#' @param type either Histogram, Boxplot or Scatter
#' @param main  main title
#' @param xlab  title for x-axis
#' @param ... which scale
#' @return A Reliability object that can be used for analyses

#' @export
plotScale.Psychometric <- function(object, scale = "All", group = NULL,
                                   external = NULL,
                                   type = "Histogram", main = "", xlab = "", ...)
{
  if (scale == "All")
    scale = object$ScaleNames
  for (s in scale)
  {
    if(is.null(group) && is.null(external))
    {
      if (s %in% object$ScaleNames)
      {
        if (type == "Histogram")
        {
          hist(object$ScaleFrame[[s]],
               main = ifelse(missing(main),
                             paste("Histogram of", s, object$Name),
                             main),
               xlab = ifelse(missing(xlab), s, xlab))
        }
        if (type == "Boxplot")
        {
          p <- ggplot2::ggplot(object$ScaleFrame[s], ggplot2::aes_string(y = s)) +
            ggplot2::geom_boxplot()
          return(p)

        }
      }
    }

    else
    {
      if (!is.null(external))
      {
        if (type == "Scatter" && s %in% names(object$ScaleFrame) &&
            external %in% names(object$OtherVariables))
        {
          d <- cbind(object$ScaleFrame[[s]], object$OtherVariables[external])
          ggplot2::ggplot(data = d, ggplot2::aes_string(x = s, y = external)) +
            ggplot2::geom_point(col = "gray") +
            ggplot2::ggtitle(paste("Distribution of ", object$Name))
        }

      }
    }

  }


}


#' Plot scales from Psychometric
#'
#' @param x a Psychometric object
#' @param y optional not used
#' @param ... extra argument is: scale = scaleName
#'
#' @return nothing
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#'  plot(object, scale = "Achievement")
#' @export
plot.Psychometric <- function(x, y, ...)
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

    for(data in x$ScaleItemFrames )
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
      data <- x$ScaleItemFrames[[scale]]
      corr <- cor(data, use = "pairwise.complete.obs")
      print(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                                   lab = TRUE))
    }

  }

}

