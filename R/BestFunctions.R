#' Title
#'
#' @param object Psychometric object
#' @param scale, scale to find best items for
#' @param nItems, number of items in new scale
#'
#' @return best item object
#' @export
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' res <- bestItems(object, "Achievement", 3)
#'
bestItems <- function(object,  scale, nItems) {
  UseMethod("bestItems", object)
}

#' @export
bestItems.Psychometric <- function(object, scale, nItems)
{


  res <- psych::bestScales(cbind(object$ScaleItemFrames[[scale]], object$ScaleFrame[scale]),
                    criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary)
  print(res)
  return(res)
}

#' BestScales
#'
#' @param object Psychometric object
#' @param nItems Number of items in new scales
#'
#' @return Psychometric object with scale means using best items
#' @export
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#' res <- bestScales(object,  5)

bestScales <- function(object, nItems) {
  UseMethod("bestScales",object)
}


#' @export
bestScales.Psychometric <- function(object, nItems)
{
  for (scale in object$ScaleNames)
  {
    res <-bestItems(object, scale, nItems)
    items <- object$ScaleItemFrames[[scale]]
    items <- items[res$best.keys[[1]]]
    object$ScaleFrame[scale] <- rowMeans(as.data.frame(items), na.rm = F)
  }
  return(object)
}
