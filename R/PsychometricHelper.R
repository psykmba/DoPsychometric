filterP<-function(object, ...)
{
  GetExtraArgument <- function(a, default)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }

  argnames <- sys.call()
   form <- GetExtraArgument("villkor", "")
   allFrame <- cbind(object$ScaleFrame, object$OtherVariables)

  if (length(form) != nrow(allFrame) && typeof(argnames[3]) == "language")
  {
    allFrame$tmyFilter <- 0
    form <- as.character(form)

    allFrame <-  allFrame %>% dplyr::mutate(tmyFilter := !!rlang::parse_expr(form))
    if (nrow(object$ScaleFrame) != length(allFrame$tmyFilter))
    {
      print("Logical vector not the same length as frames")
      return()
    }

  }
  else
    allFrame$tmyFilter <- form
  allFrame <- allFrame %>% tidyr::replace_na(list(tmyFilter = FALSE))
  object$ScaleFrame <- dplyr::filter(object$ScaleFrame, allFrame$tmyFilter)
  object$OtherVariables <- dplyr::filter(object$OtherVariables, allFrame$tmyFilter)
  object$OriginalData <- dplyr::filter(object$OriginalData, allFrame$tmyFilter)
  for(index in 1:length(object$ScaleItemFrames))
    object$ScaleItemFrames[[index]] <-
    dplyr::filter(object$ScaleItemFrames[[index]], allFrame$tmyFilter)
  return(object)
}

MakeMissing <- function(data, miss)
{
  if (is.null(miss))
    return(data)
  data <- naniar::replace_with_na_all(data, ~.x %in% miss)
  return(data)
}



GetItemWithParcels <- function(object, parcel, subscales)
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
    if (isTRUE(parcel))
    {
      s <- ncol(data) / 3
      numItemVector <- 0
      if (trunc(s) == s)
        numItemVector <- c(s,s*2,s*3)
      else
      {
        maxItems <- trunc(s) + 1
        numItemVector <- GetItemNum(ncol(data), maxItems)
      }
      randomParcelNum<- GetRandomItemNum( numItemVector, ncol(data))
      return(list(data[randomParcelNum[[1]]],
                  data[randomParcelNum[[2]]],
                  data[randomParcelNum[[3]]]))
    }
    else
    {
      res <- list()
      for (index in 1:ncol(data))
        res <- append(res, list(data[index]))
      return(res)
    }
  }
  CombineParcels <- function(data)
  {
    res <- data.frame(row.names = 1:nrow(data[[1]]))
    for (d in data)
    {
      if(ncol(d) == 1)
      {
        newColumn <- data.frame(d)
      }
      else
      {
        newColumn <- data.frame(rowMeans(d))
      }
      names(newColumn) <- names(d[1])
      res <- cbind(res, newColumn)
    }
    return(res)
  }
  res <- list()

  object <- selectP.Psychometric(object, subscales)
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

#' Handle outliers methods
#' @param object a Psychometric object to work with
#' @param method there are four ways, "Mahalanobis", "Quartile", SD" and "Change"
#' @param limit the probability value for handeling an outlier
#' @param missing when "none", missing values are not handled, otherwise the method in missing will be used
#' @param otherVar whether the variables not among the scales shall be included, only for check and SD
#' @return an updated Psychometric object
#' @export
handleOutliers <- function(object, method = "Mahalanobis", limit = .001,
                           missing = "None", otherVar = F) {
  UseMethod("handleOutliers", object)
}

#' Handle outliers methods
#' @param object a Psychometric object to work with
#' @param method there are four ways, "Mahalanobis", "Quartile", SD" and "Change"
#' @param limit the probability value for handeling an outlier
#' @param missing when "none", missing values are not handled, otherwise the method in missing will be used
#' @param otherVar whether the variables not among the scales shall be included, only for check and SD
#' @return an updated Psychometric object
#' @details Essentially this function takes a Psychometric object and checks for
#' outliers. The resulting object is sometimes changed, for example Mahalanobis and
#' SD and Quartile filters out the outliers and return an updated Psychometric object.
#' 'Change' winsorizes the scales, e.g., changes the data values to be inside the
#' distribution.
#' The limit argument defines an outlier in terms of probabilty, either based on
#' chi2 value or SD.
#' The missing argument impute (or deletes cases). See the ImputeMissing function.
#' Using the 'OtherVariables' flag makes it possible to also handle other variables
#' not included in some of the scales.
#' @export
handleOutliers.Psychometric <- function(object, method = "Mahalanobis", limit = .001,
                                        missing = "None", otherVar = c())
{
  getInsideRange <- function(s, r)
  {
    return (ifelse(s >= r[1] & s <= r[2], s, ifelse(s < r[1], r[1], r[2])))
  }
  deleteOutsideRange <- function( s, r)
  {
    return(ifelse (s < r[1], NA, ifelse(s > r[2], NA, s) ))
  }
  if (missing != "None")
    noMissObject <- imputeMissing(object, handleMissing = missing)
  else
    noMissObject <- object

  if (method == "Mahalanobis") {
    scaleCor <- stats::cov(noMissObject$ScaleFrame)
    Outliers <- stats::mahalanobis(noMissObject$ScaleFrame, colMeans(noMissObject$ScaleFrame), scaleCor)
    object <- filterP(noMissObject, villkor = Outliers < stats::qchisq(1-limit, length(object$ScaleNames)))
    return(object)
  }
  if (method == "SD")
  {
    newFrame <-  noMissObject$ScaleFrame
    newFrame <- as.data.frame(sapply(newFrame, FUN = function(scale)
    {
      m <- mean(scale, na.rm = T)
      sd <- sd(scale, na.rm = T) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      return(deleteOutsideRange(scale, r))
    }))
    noMissObject$ScaleFrame <- newFrame
    if (length(otherVar) > 0)
    {
      oFrame <-  noMissObject$OtherVariables
      oFrame[,c(otherVar)] <-  sapply(oFrame[otherVar], FUN = function(scale)
      {
        m <- mean(scale, na.rm = T)
        sd <- sd(scale, na.rm = T) * stats::qnorm(1 - limit)
        r <- range(m+sd, m-sd)
        d <-deleteOutsideRange(scale, r)
        return(d)
      })

    }
    else
    {
      oFrame <- noMissObject$OtherVariables
    }
    noMissObject$OtherVariables <- oFrame



    return(noMissObject)
  }
  if (method == "Quartile")
  {
    x <- noMissObject
    IQR_list<-list()
    Outlier__rowNum <- c()
    for(i in 1:ncol(x$ScaleFrame))
    {
      #first and third quartile
      qnt <- stats::quantile(x$ScaleFrame[,i], probs = c(0.25, 0.75), na.rm = TRUE)
      #identifying outliers above and below iQR * 1.5
      outliers_above <-  x$ScaleFrame[i][x$ScaleFrame[,i]  > (qnt[2] + 1.5*stats::IQR(x$ScaleFrame[,i], na.rm = T)),]
      outliers_below <- x$ScaleFrame[i][x$ScaleFrame[,i] < (qnt[1] - 1.5*stats::IQR(x$ScaleFrame[,i], na.rm = T)),]
      #adding outliers as well as outlier boundaries into a dataframe
      outliers<-data.frame(outliers=c(outliers_below, outliers_above),
                           fence=c(rep(qnt[1] - 1.5*stats::IQR(x$ScaleFrame[,i], na.rm = T), length(outliers_below)), rep(qnt[2] + 1.5*stats::IQR(x$ScaleFrame[,i], na.rm = T), length(outliers_above))))
      #remove NA:s if added
      outliers<-stats::na.omit(outliers)
      #order the row names
      if(nrow(outliers) == 0){
        outliers
      }else{
        rownames(outliers)<-c(1:nrow(outliers))
      }
      #identifying the row numbers and then the ID of the outliers
      rows_with_numbers <- c(which(x$ScaleFrame[,i] %in% outliers_above),which(x$ScaleFrame[,i] %in% outliers_below)) # the row indices where the numbers are found
      Outlier__id<-x$OtherVariables$ID[c(rows_with_numbers)]
      Outlier__rowNum<- c(Outlier__rowNum, rows_with_numbers)

      #combine the dataframes and add it to the outlier list
      result_df <- data.frame(Outlier__id,outliers)
      colnames(result_df)[2] <- colnames(x$ScaleFrame[i])
      IQR_list[[i]]<-result_df
      # delete outliers for new object
    }


    Outlier__rowNum <- unique(Outlier__rowNum)
    noMissObject$ScaleFrame <- x$ScaleFrame[-Outlier__rowNum,]
    noMissObject$OtherVariables <- x$OtherVariables[-Outlier__rowNum,]
    for (index in  seq_along(noMissObject$ScaleItemFrames))
    {
      noMissObject$ItemScaleFrames[[index]] <- noMissObject$ItemScaleFrames[[index]][-Outlier__rowNum,]
    }

    print(IQR_list)
    return(noMissObject)

  }
  if (method == "Winsorizing" || method == "Change")
  {
    newFrame <- data.frame(row.names = 1:nrow(object$ScaleFrame))
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * stats::qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, getInsideRange(scale, r))
    }
    names(newFrame) <- object$ScaleNames
    noMissObject$ScaleFrame <- newFrame
    if (length(otherVar) > 0)
    {
      newFrame <-  data.frame(row.names = 1:nrow(object$ScaleFrame))

      newFrame <- data.table::data.table(row.names = 1:nrow(object$ScaleFrame))
      otherVar <- lapply(otherVar, function(v) {
        ifelse(is.numeric(noMissObject$OtherVariables[v]), {
          m <- mean(noMissObject$OtherVariables[v], na.rm = TRUE)
          sd <- sd(noMissObject$OtherVariables[v], na.rm = TRUE) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          noMissObject$OtherVariables[v][which(noMissObject$OtherVariables[v] >= r[1] & noMissObject$OtherVariables[v] <= r[2])]
        }, {
          noMissObject$OtherVariables[v]
        })
      })
      newFrame <- cbind(newFrame, otherVar)
      for(v in otherVar)
      {
        if (is.numeric(noMissObject$OtherVariables[v]))
        {
          m <- mean(noMissObject$OtherVariables[v])
          sd <- sd(noMissObject$OtherVariables[v]) * stats::qnorm(1 - limit)
          r <- range(m+sd, m-sd)
          newFrame <- cbind(newFrame, getInsideRange(noMissObject$OtherVariables[v], r))
        }
        else
        {
          newFrame <- cbind(newFrame, noMissObject$OtherVariables[v])
        }
      }
      noMissObject$OtherVariables <- newFrame




    }
    return(noMissObject)
  }
  return(object)

}




library(dplyr)
library(lavaan)
library(utils)


#' writeP
#'
#' @param object A psychometric object
#' @param File A filename with pathinformation
#' @param colnames T if you like to write column names
#' @param rownames T if you like to write row names
#'
#' @return NULL
#' @export
writeP <- function(object, File, colnames = T, rownames = F) {
  UseMethod("writeP", object)
}

#' Write all data
#'
#' @param object A psychometric object
#' @param File A filename with pathinformation
#' @param colnames T if you like to write column names
#' @param rownames T if you like to write row names
#'
#' @return NULL
#' @export
writeP.Psychometric <- function(object, File, colnames = T, rownames = F)
{
  write.table(x = getData(object),file = File, col.names = colnames,
              row.names = rownames)
  return(NULL)

}


#' getSubScaleNames
#'
#' @param object a Psychometric object
#'
#' @return a list of all names starting with the
#' @details When the ScaleItemFrames is created it changes the variable names of
#' the items. This functions return all the names together with the scale names.
#' The functions can be used to check that everything has been defined correctly.
#' @export
getSubScaleNames <- function(object)
  writeP <- function(object, File, colnames = T, rownames = F) {
    UseMethod("getSubScaleNames", object)
  }



#' getSubScaleNames
#'
#' @param object a Psychometric object
#'
#' @return a list of all names starting with the
#' @details When the ScaleItemFrames is created it changes the variable names of
#' the items. This functions return all the names together with the scale names.
#' The functions can be used to check that everything has been defined correctly.
#' @export
getSubScaleNames <- function(object)
{
  res <- list()
  for(index in 1:length(object$ScaleNames))
  {
    nam2 <-  names(object$ScaleItemFrames[[index]])
    res <- append(res, list(c(nam2)))

  }
  names(res) <- object$ScaleNames
  return(res)
}
