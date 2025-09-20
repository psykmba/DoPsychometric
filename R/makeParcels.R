#' makeThreeParcels
#'
#' @param object Psychometric object
#' @param scales, scale to find best items for
#' @param random, random parcels
#' @param nParc,  number of parcels to make 2 or 3
#'
#' @return best item object
#' @export
makeThreeParcels <- function(object,  scales, random = FALSE, nParc = 3) {
  UseMethod("makeThreeParcels", object)
}

#' @export
makeThreeParcels.Psychometric <- function(object,  scales,  random = FALSE, nParc = 3)
{
  if (isTRUE(random))
  {
    scaleItemFrames <- object$ScaleItemFrames[scales]
    if (nParc == 3)
    {
      for (scale in scales)
      {
        s <- object$ScaleItemFrames[[scale]]
        sampleR <- sample(1:ncol(s), ncol(s))
        P1 <- rowMeans(s[,sampleR[1:trunc(ncol(s)/3)]], na.rm = T)
        P2 <- rowMeans(s[,sampleR[(trunc(ncol(s)/3)+1):trunc(ncol(s)/3)*2]], na.rm = T)
        P3 <- rowMeans(s[,sampleR[(trunc(ncol(s)/3)*2+1):ncol(s)]], na.rm = T)




        s <- as.data.frame(list(P1,P2,P3))
        names(s) <- sapply(c("P1", "P2", "P3"), FUN = function(x) return(paste(scale, x, sep = "")))
        object$ScaleItemFrames[[scale]] <- s
      }
    }
    else if (nParc == 2)
    {
      for (scale in scales)
      {
        s <- object$ScaleItemFrames[[scale]]
        sampleR <- sample(1:ncol(s), ncol(s))
        P1 <- rowMeans(s[,sampleR[1:trunc(ncol(s)/2)]], na.rm = T)
        P2 <- rowMeans(s[,sampleR[(trunc(ncol(s)/2)+1):ncol(s)]], na.rm = T)
        s <- as.data.frame(list(P1,P2))
        names(s) <- sapply(c("P1", "P2"), FUN = function(x) return(paste(scale, x, sep = "")))
        object$ScaleItemFrames[[scale]] <- s
      }
    }
    return(object)

  }
  scaleItemFrames <- object$ScaleItemFrames[scales]
  for (scale in scales)
  {
    s <- object$ScaleItemFrames[[scale]]
    if (ncol(s) >=9 && nParc == 3)
      s <- psych::scoreItems(psych::parcels(s, size = 3),s)$scores[,1:3]
    else
      s <-  psych::scoreItems(psych::parcels(s, size = 2),s)$scores[,1:3]




    s <- as.data.frame(s)
    names(s) <- sapply(names(s), FUN = function(x) return(paste(scale, x, sep = "")))
    object$ScaleItemFrames[[scale]] <- s
  }
  return(object)
}

meanMatrix <- function(dataList)
{
  w <- which(sapply(dataList[[1]]$pe, is.numeric))
  first <- as.matrix(dataList[[1]]$pe[,w])
  for (index in 2:length(dataList))
  {
    second <- as.matrix(dataList[[index]]$pe[,w])
    first <- first + second
  }
  first <- round( first / length(dataList),3)
  returnMatrix <- dataList[[1]]$pe
  returnMatrix[w] <- first
  return(returnMatrix)
  return(first)
}
sdMatrix <- function(dataList)
{
  w <- which(sapply(dataList[[1]]$pe, is.numeric))
  meanM <- meanMatrix(dataList)[,w]
  first <- (as.matrix(dataList[[1]]$pe[,w])-meanM)**2
  for (index in 2:length(dataList))
  {
    second <- as.matrix(dataList[[index]]$pe[,w])
    first <- first + (second-meanM)**2
  }
  first <- round(first / length(dataList),3)
  returnMatrix <- dataList[[1]]$pe
  returnMatrix[w] <- sqrt(first)
  return(returnMatrix)
}

#' testRandomParcels
#'
#' @param object Psychometric object
#' @param model Should be the model that test for
#' @param scales, scale to find best items for
#' @param k, number of items in new scale
#' @param random, random parcels
#'
#' @return best item object
#' @export
testRandomParcels <- function(object, model, scales, k = 31, random = FALSE) {
  UseMethod("testRandomParcels", object)
}

#' @export
testRandomParcels.Psychometric <- function(object, model,  scales, k = 31, random = FALSE) {

  # sumData <- list()
  # for (index in 1:k)
  # {
  #   objectParcels <- makeParcels(object, scales, random)
  #   data <- getData(objectParcels)
  #   fit <- sem(model = modelStruct4, data = data, missing = 'fiml', orthogonal = TRUE)
  #   fit <- eval(model)
  #   sumData <- append(sumData, list(summary(fit,  fit.measures = TRUE, standardized = TRUE)))
  #   lavaan::modificationindices(fit, maximum.number = 20, sort = T)
  # }

  #debugonce(sumMatrix)
#  sumMatrix(sumData)
  #debugonce(sdMatrix)
#  sdMatrix(sumData)

}

