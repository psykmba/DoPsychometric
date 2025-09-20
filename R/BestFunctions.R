#' Title
#'
#' @param object Psychometric object
#' @param scale, scale to find best items for
#' @param nItems, number of items in new scale
#' @param splitReversed, whether item should be splitted on reversed items
#' @param disc, take away items with bad discrimination
#' @param ..., parameters to discriminate function
#'
#' @return best item object
#' @export
bestItems <- function(object,  scale, nItems, splitReversed, disc,...) {
  UseMethod("bestItems", object)
}

#' @export
bestItems.Psychometric <- function(object, scale, nItems, splitReversed = F, disc = 0, ...)
{
  GetExtraArgument <- function(a, default)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  GetBestSplitted <- function()
    # Items that are reversed ends with "R" so first all items that do not end with R
    # should be selected and half of the nItems should be taken from these, then the
    # items with a name that ends with "R" should be selected and half of the nItems
  {
    items <- object$ScaleItemFrames[[scale]]
    nItems2 <- round(nItems/2)
    nItems1 <- nItems - nItems2
    namesV <- names(items)
    normalItems <- c()
    reversedItems <- c()
    for (name in namesV)
    {
      if (stringr::str_ends(name, "R"))
        reversedItems <- c(reversedItems, name)
      else
        normalItems <- c(normalItems, name)
    }
    if (length(normalItems) < nItems1)
      nItems1 <- length(normalItems)
    if (length(reversedItems) < nItems2)
      nItems2 <- length(reversedItems)
    res1 <- invisible(psych::bestScales(cbind(items[normalItems], object$ScaleFrame[scale]),
                                        criteria = scale, n.item = nItems1-1, dictionary = object$ItemDictionary))
    res2 <- invisible(psych::bestScales(cbind(items[reversedItems], object$ScaleFrame[scale]),
                                        criteria = scale, n.item = nItems2-1, dictionary = object$ItemDictionary))
    return(c(res1$best.keys[[1]], res2$best.keys[[1]]))

  }
  if (disc == 0 )
    if (isTRUE(splitReversed))
      res <- GetBestSplitted()
    else
   res <- invisible(psych::bestScales(cbind(object$ScaleItemFrames[[scale]], object$ScaleFrame[scale]),
                    criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary))
  else
  {
    if (isTRUE(splitReversed))
      # In this case only half of the items in disc should be taken from each group
    {
     disc <- round(disc/2)
      nItems2 <- round(nItems/2)
      nItems1 <- nItems - nItems2
      namesV <- names(object$ScaleItemFrames[[scale]])
      normalItems <- c()
      reversedItems <- c()
      for (name in namesV)
      {
        if (stringr::str_ends(name, "R"))
          reversedItems <- c(reversedItems, name)
        else
          normalItems <- c(normalItems, name)
      }
      if (length(normalItems) < nItems1)
        nItems1 <- length(normalItems)
      if (length(reversedItems) < nItems2)
        nItems2 <- length(reversedItems)
      disc1 <- round(disc/2)
      disc2 <- disc - disc1
      discScales <- GetExtraArgument("discrminantScales", NULL)
      if (is.null(discScales))
      {
        print("No discriminant scales given")
        return(NULL)
      }
      useItems <- bestDiscriminantItem(object, scale, object$ScaleFrame[discScales])
      useItems1 <- intersect(names(useItems),reversedItems)
      useItems2 <- intersect(names(useItems),normalItems)
      useItems1 <- useItems1[1:(length(useItems1)-disc1)]
      useItems2 <- useItems2[1:(length(useItems2)-disc2)]
      if (nItems > length(useItems)-disc)
        return(c(useItems1, useItems2))
      res1 <- invisible(psych::bestScales(cbind(object$ScaleItemFrames[[scale]][,useItems1], object$ScaleFrame[scale]),
                                         criteria = scale, n.item = nItems1-1, dictionary = object$ItemDictionary))
      res2 <- invisible(psych::bestScales(cbind(object$ScaleItemFrames[[scale]][,useItems2], object$ScaleFrame[scale]),
                                         criteria = scale, n.item = nItems2-1, dictionary = object$ItemDictionary))
      return(c(res1$best.keys[[1]], res2$best.keys[[1]]))
    }
   discScales <- GetExtraArgument("discrminantScales", NULL)
    if (is.null(discScales))
    {
      print("No discriminant scales given")
      return(NULL)
    }
    useItems <- bestDiscriminantItem(object, scale, object$ScaleFrame[discScales])
    useItems <- names(useItems[1:(length(useItems)-disc)])
    if (nItems < length(useItems)-disc)
      return(useItems)
    res <- invisible(psych::bestScales(cbind(object$ScaleItemFrames[[scale]][,useItems], object$ScaleFrame[scale]),
                                       criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary))

  }
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
    if (nItems < length( object$ScaleItemFrames[[scale]]))
    {
     res <-bestItems(object, scale, nItems)
    items <- object$ScaleItemFrames[[scale]]
    items <- items[res$best.keys[[1]]]
    object$ScaleFrame[scale] <- rowMeans(as.data.frame(items), na.rm = T)
    if (deleteItems)
    {
      object$ScaleItemFrames[[scale]] <- items
    }
    }
  }
   return(object)
}


#' BestItemsForScale
#'
#' @param object Psychometric object
#' @param targetObject Psychometric object which should be used as facet targets
#' @param method which method to create the scales, mean default
#' @return Psychometric object with items that correlate most strongly with their scale
#' @export
BestItemsForScale <- function(object, targetObject = NULL, method = "mean") {
  UseMethod("bestScales",object)
}



BestItemsForScale <- function(object, targetObject = NULL, method = "mean")
{
  GetColNames <- function(dFrame)
  {
    res <- ""
    for (name in names(dFrame))
    {
      res <- paste(res, '"', name, '",', sep = "")
    }
    return(substr(res, 1, stringr::str_length(res)-1))
  }

  FindBestScaleForItem <- function(object, targetObject)
  {
    FindBest <- function()
    {
      index <- 0
      tScaleFrame <- object$ScaleFrame #Take all scales for comparison with items
      res <- data.frame() #results
      out <- data.frame() #new data frame
      for (itemScale in object$ScaleItemFrames) # go through all item frames
      {
        #Set up each round, for each scale item frame
        index <- index + 1
        itemIndex <- 0
        r <- list(0)
        names(r) <- names(tScaleFrame[index])
        res <- append(res, r)
        out <- append(out, r)
        for (item in itemScale)
        {
          # Go through all item
          itemIndex <- itemIndex + 1 # update the index since we have have item content in 'item'
          tScaleFrame[index] <- rowMeans(itemScale[-itemIndex], na.rm = T) #scale without item excluded
          corItem <- stats::cor(item, tScaleFrame, use = "pairwise.complete.obs") #cor item with all scales
          corItem <- as.data.frame(cbind(1:ncol(tScaleFrame), t(corItem))) #add cor to
          names(corItem) <- c("ItemNum", names(tScaleFrame[index])) # the goal is to sort the cor frame
          corItem <- dplyr::arrange(corItem, dplyr::desc(corItem[,2])) # here it is sorted from largest to smallest
          if (corItem[1,1] == index) # if the items has the highest correlation with its own
            # scale it is selected, otherwise it is deselected
          {
            res[[index]] <- c(res[[index]], itemIndex)
            names(res[index]) <- names(tScaleFrame[index])
            #        print(c("Correct", corItem[1,2], names(tScaleFrame[index])))
          }
          else
          {
            out[[index]] <- c(out[[index]], itemIndex)
            names(out[index]) <- names(tScaleFrame[index])
            print("Fawlty")
          }

        }

        tScaleFrame[index] <- object$ScaleFrame[index]
      }
      return(list(res,out))

    }
    FindBestTarget <- function()
    {
        index <- 0
      tScaleFrame <- targetObject$ScaleFrame #Take all scales for comparison with items
      res <- data.frame() #results
      out <- data.frame() #new data frame
      for (itemScale in object$ScaleItemFrames) # go through all item frames
      {
        #Set up each round, for each scale item frame
        index <- index + 1
        itemIndex <- 0
        r <- list(0)
        names(r) <- names(tScaleFrame[index])
        res <- append(res, r)
        out <- append(out, r)
        for (item in itemScale)
        {
          # Go through all item
          itemIndex <- itemIndex + 1 # update the index since we have have item content in 'item'
          corItem <- stats::cor(item, tScaleFrame, use = "pairwise.complete.obs") #cor item with all scales
          corItem <- as.data.frame(cbind(1:ncol(tScaleFrame), t(corItem))) #add cor to
          names(corItem) <- c("ItemNum", names(tScaleFrame[index])) # the goal is to sort the cor frame
          corItem <- dplyr::arrange(corItem, dplyr::desc(corItem[,2])) # here it is sorted from largest to smallest
          if (corItem[1,1] == index) # if the items has the highest correlation with its own
            # scale it is selected, otherwise it is deselected
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

        tScaleFrame[index] <- targetObject$ScaleFrame[index]
      }
      return(list(res,out))

    }
    if (object$Name == targetObject$Name)
      return(FindBest())
    else
      return(FindBestTarget())
  }
  GetScalesFrame <- function(frames, nameV, N)
  {
    res <- NULL
    object$RCommands <<- list()
    for (index in 1:length(frames))
    {
      if (is.null(frames[[index]]))
        res <- cbind(res, as.data.frame(rep(NA, N)))
      else
        res <- cbind(res, rowMeans(as.data.frame(frames[[index]]), na.rm = T))
      object$RCommands <<- append(object$RCommands, list(paste("Data$",nameV[index], "<- rowMeans(Data[c(", GetColNames(as.data.frame(frames[[index]])), ")],na.rm = TRUE)\n", sep = "")))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    names(res) <- nameV
    return(res)


  }
  scaleItemFrames <- object$ScaleItemFrames
  if (is.null(targetObject))
    targetObject <- object

  for (index in 1:length(scaleItemFrames))
  {
    object$ScaleItemFrames[[index]] <- data.frame(row.names = 1:nrow(object$ScaleFrame))
    startIndex <- 1
    for (x in 1:length(scaleItemFrames[[index]]))
    {

      if(sum(!is.na( scaleItemFrames[[index]][x]))>50)
      {
        object$ScaleItemFrames[[index]][startIndex] <- scaleItemFrames[[index]][x]
        startIndex <- startIndex + 1
      }

    }

  }


  myRes <- FindBestScaleForItem(object, targetObject)
  # print(myRes)
  index <- 1
  scaleItemFrames <- object$ScaleItemFrames
  for (index in 1:length(object$ScaleItemFrames))
  {
     if (length(myRes[[1]][[index]]) > 2)
    {
      scaleItemFrames[[index]] <- object$ScaleItemFrames[[index]][,myRes[[1]][[index]]]
    }
  }

  object$ScaleFrame <- GetScalesFrame(scaleItemFrames,
                                      object$ScaleNames, nrow(object$ScaleFrame[1]))
  object$ScaleItemFrames <- scaleItemFrames
  return(object)
  #   tMice <- mice::mice(object$ScaleFrame, method = "mean")
  #  object$ScaleFrame <- complete(tMice,1)

}


#' bestDiscriminantItem
#'
#' @param object Psychometric object
#' @param mainScale Scale to test which items are best
#' @param discriminantScales which scales to discriminate from
#' @return sorted vector of items with highest discrimination first
#' @export
bestDiscriminantItem <- function(object, mainScale, discriminantScales) {
  UseMethod("bestDiscriminantItem",object)
}


#' bestDiscriminantItem
#'
#' @param object Psychometric object
#' @param mainScale Scale to test which items are best
#' @param discriminantScales which scales to discriminate from
#' @return sorted vector of items with highest discrimination first
#' @export
bestDiscriminantItem <- function(object, mainScale, discriminantScales)
{
  res <- c()
  getSubScaleNames <- function(subScales)
  {
    res <- ""
    for (subScale in names(subScales))
    {
      res <- paste(res, subScale, " + ")
    }
    return(substr(res, 1, stringr::str_length(res)-2))
  }

  items <- object$ScaleItemFrames[[mainScale]]
  data <- cbind(discriminantScales, items)
  n <- names(items)
  for (item in n)
  {
    pred <- summary(lm(as.formula(paste(item,"~", paste(getSubScaleNames(discriminantScales)),
                                collapse = "")), data = data))
    res <-c(res, pred$r.squared)
  }
  names(res) <- n
  return(sort(res))
}

