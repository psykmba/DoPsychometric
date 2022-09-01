
#' Title
#'
#' @param object  a Psychometric object
#' @param scale a Scale name
#' @param subscales a vector of facet names
#'
#' @return results
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'    responseScale = list(c(0,4)), itemLength = 4)
#' TestFacets(object, "Cons", c("Achievement", "Dutifulness", "Orderly"))
#' @export
TestFacets <- function(object,scale, subscales) {
  UseMethod("TestFacets", object)
}

#' @export
TestFacets.Psychometric <- function(object, scales, subscales)
{
  GetItemWithParcels <- function()
  {
    GetItemNum <- function(n, maxItems)
    {
      res <- maxItems
      if ((n - (maxItems*2)) == (maxItems-1))
        return(c(maxItems, maxItems*2, n))
      else
        return(c(maxItems,  maxItems*2-1, n))
    }
    GetRandomItemNum <- function( itemVector, nLength)
    {

      itemNums <- sample(1:nLength, nLength, replace = F)
      return(list(itemNums[1:itemVector[1]],
               itemNums[(itemVector[1]+1):itemVector[2]],
               itemNums[(itemVector[2]+1):itemVector[3]]))

    }
    MakeParcels <- function(data)
    {
      s <- ncol(data) / 3
      numItemVector <- 0
      if (trunc(s) == s)
        numItemVector <- c(s,s*2,s*3)
      else
      {
        maxItems <- s + 1
        numItemVector <- GetItemNum(ncol(data), maxItems)
      }
      randomParcelNum<- GetRandomItemNum( numItemVector, ncol(data))
      return(list(data[randomParcelNum[[1]]],
                  data[randomParcelNum[[2]]],
                  data[randomParcelNum[[3]]]))
    }
    CombineParcels <- function(data)
    {
      res <- data.frame(row.names = 1:nrow(data[[1]]))
      for (d in data)
      {
        newColumn <- data.frame(rowMeans(d))
        names(newColumn) <- names(d[1])
        res <- cbind(res, newColumn)
      }
      return(res)
    }
    res <- list()
    for(subscale in subscales)
    {
       dataFrames <- object$ScaleItemFrames[[subscale]]
       dataFrames <- MakeParcels(dataFrames)
       dataFrames <- list(CombineParcels(dataFrames))

       names(dataFrames) <- subscale
       res <- append(res, dataFrames)
    }
    return(res)
  }
  GetHierarchicalModel <- function(scales, subscaleData)
  {
    getSubScaleNames <- function(subScaleData)
    {
      res <- ""
      for (subScale in names(subScaleData))
      {
        res <- paste(res, subScale, " + ")
      }
      return(substr(res, 1, stringr::str_length(res)-2))
    }

    getScaleNames <- function(scaleNames)
    {
      res <- ""

      for (scale in scaleNames)
      {
        res <- paste(res, scale, " + ")
      }
      return(substr(res, 1, stringr::str_length(res)-2))
    }
    getDataFrameSubScale <-function(subScaleData)
    {
      dataFrame <- data.frame(row.names = 1:nrow(subScaleData[[1]]))
       for(subScale in subScaleData)
      {
        dataFrame <- cbind(dataFrame, subScale)
      }
      return(dataFrame)
    }


    lines <- c()
    for(scale in names(subScaleData))
    {

      ind <-getSubScaleNames(subscaleData[[scale]])
      line <- paste(scale, "=~" , ind, "\n", sep=" ")
      lines <- paste(lines, " ", line,"\n", sep=" ")
    }
    dep <-  getScaleNames(names(subscaleData))
    lines <- paste(lines, scales, "=~", dep,"\n", sep = "")
    dataFrame <- getDataFrameSubScale(subScaleData)

    print(lines)

    print(cfa(model = lines, data = dataFrame))
    return(lines)
  }

  subScaleData <- GetItemWithParcels()
  hierModel <- GetHierarchicalModel(scales, subScaleData)
  return()
  result <- list()
  for (subscale in subScaleData)
  {
    result <- append(result,GetTestSubscale(subscale))
  }
  return(append(hierModel, result))
}
