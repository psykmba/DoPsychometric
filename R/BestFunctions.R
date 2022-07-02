bestItems <- function(x, ...) {
  UseMethod("bestItems", x)
}

bestItems.Psychometric <- function(object, scale, nItems)
{


  res <- psych::bestScales(cbind(object$ScaleItemFrames[[scale]], object$ScaleFrame[scale]),
                    criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary)
  print(res)
  return(res)
}

#' BestScales
#'
#' @param object
#' @param nItems
#'
#' @return
#' @export
#'
#' @examples
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
    items < items[res$best.keys[[1]]]
    object$ScaleFrame[scale] <- rowMeans(as.data.frame(items), na.rm = F)
  }
  return(object)
}
