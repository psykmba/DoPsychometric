#' Title
#'
#' @param object Psychometric object
#' @param scale, scale to find best items for
#' @param nItems, number of items in new scale
#'
#' @return best item object
#' @export
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
#' @param deleteItems F = allitems are left in the object. T: items not selected are deleted
#'
#' @return Psychometric object with scale means using the best nItems items
#' @export
bestScales <- function(object, nItems, deleteItems = F) {
  UseMethod("bestScales",object)
}


#' BestScales
#'
#' @param object Psychometric object
#' @param nItems Number of items in new scales
#' @param deleteItems F = allitems are left in the object. T: items not selected are deleted
#'
#' @return Psychometric object with scale means using the best nItems items
#' @export
bestScales.Psychometric <- function(object, nItems, deleteItems = F)
{
  for (scale in object$ScaleNames)
  {
    res <-bestItems(object, scale, nItems)
    items <- object$ScaleItemFrames[[scale]]
    items <- items[res$best.keys[[1]]]
    object$ScaleFrame[scale] <- rowMeans(as.data.frame(items), na.rm = F)
    if (deleteItems)
    {
      object$ScaleItemFrames[[scale]] <- items
    }
  }
   return(object)
}


#' bestScaleItems
#'
#' @param object Psychometric object
#' @export
bestScaleItems <- function(object) {
  UseMethod("bestScales",object)
}

bestScaleItems <- function(object)
{
  FindBestScaleForItem <- function(object)
  {
    index <- 0
    tScaleFrame <- object$ScaleFrame
    res <- data.frame()
    out <- data.frame()
    for (itemScale in object$ScaleItemFrames)
    {
      index <- index + 1
      itemIndex <- 0
      r <- list(0)
      names(r) <- names(tScaleFrame[index])
      res <- append(res, r)
      out <- append(out, r)
      for (item in itemScale)
      {
        itemIndex <- itemIndex + 1
        tScaleFrame[index] <- rowMeans(itemScale[-itemIndex], na.rm = T)
        corItem <- cor(item, tScaleFrame, use = "pairwise.complete.obs")
        corItem <- as.data.frame(cbind(1:ncol(tScaleFrame), t(corItem)))
        names(corItem) <- c("ItemNum", names(tScaleFrame[index]))
        corItem <- dplyr::arrange(corItem, desc(corItem[,2]))
        if (corItem[1,1] == index)
        {
          res[[index]] <- c(res[[index]], itemIndex)
          names(res[index]) <- names(tScaleFrame[index])
          #        print(c("Correct", corItem[1,2], names(tScaleFrame[index])))
        }
        else
        {
          out[[index]] <- c(out[[index]], itemIndex)
          names(out[index]) <- names(tScaleFrame[index])
          #        print(c("Fawlty", corItem[1,2], names(tScaleFrame[index])))
        }

      }

      tScaleFrame[index] <- object$ScaleFrame[index]
    }
    return(list(res,out))
  }
}
