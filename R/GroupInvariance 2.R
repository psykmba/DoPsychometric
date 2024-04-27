#' Group invariance test with CFA
#'
#' @param object  a Psychometric object
#' @param scale a Scale name
#' @param group a category variable included in "Other variables"
#' @param ordered whether the variables are ordinal or binary
#' @param fixed whether the bi-factor should be fixed to 1
#' @param fixedScales whether the scales should be fixed to 1
#' @param fixedSubScales fix these subscales to 1
#' @param tries how many attempt to estimate model
#' @param parcel whether the observed variables should be in parcels
#' @param estimator any estimator that is admissible for cfa
#' @param zeroVar var set to zero error variance
#' @param delVar delete some variables in a previous model
#'
#' @return TestFacet model
#' @export
GroupInvariance <- function(object,scale, group,ordered = F,fixed = F,
                       fixedScales = F,parcel = F,fixedSubScales = c(),
                       tries = 1, estimator = "ML", zeroVar = c(),
                       delVar = c()) {
  UseMethod("TestFacets", object)
}

#' Facet test with CFA
#'
#' @param object  a Psychometric object
#' @param scale a Scale name
#' @param group a category variable included in "Other variables"
#' @param ordered whether the variables are ordinal or binary
#' @param fixed whether the bi-factor should be fixed to 1
#' @param fixedScales whether the scales should be fixed to 1
#' @param fixedSubScales fix these subscales to 1
#' @param tries how many attempt to estimate model
#' @param parcel whether the observed variables should be in parcels
#' @param estimator any estimator that is admissible for cfa
#' @param zeroVar var set to zero error variance
#' @param delVar delete some variables in a previous model
#'
#' @return a Invariance model
#' @export
GroupInvariance.Psychometric <- function(object, scale, group, ordered = F, fixed = F,
                                    fixedScales = F,parcel = F,fixedSubScales = c(),
                                    tries = 1, estimator = "ML", zeroVar = c(),
                                    delVar = c())
{
  commands <- list()
  GetItemWithParcels <- function(subscales,ScaleItemFrames)
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

    for(scale in scale)
    {
      dataFrames <- ScaleItemFrames[[scale]]
      dataFrames <- MakeParcels(dataFrames)
      dataFrames <- list(CombineParcels(dataFrames))

      names(dataFrames) <- names(ScaleItemFrames[scale])
      res <- append(res, dataFrames)
    }
    return(res)
  }
  getSubScaleNames <- function(subScaleData)
  {
    res <- ""
    for (subScale in names(subScaleData))
    {
      res <- paste(res, subScale, " + ")
    }
    return(substr(res, 1, stringr::str_length(res)-2))
  }
  getSubScaleNamesZero <- function(subScaleData)
  {
    res <- ""
    for (subScale in names(subScaleData))
    {
      res <- paste(res, "0*",subScale, " + ", sep="")
    }
    return(substr(res, 1, stringr::str_length(res)-2))
  }
  getSubScaleNamesOne <- function(subScaleData)
  {
    res <- ""
    for (subScale in names(subScaleData))
    {
      res <- paste(res, "1*",subScale, " + ", sep="")
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
  getScaleNamesOne <- function(scaleNames)
  {
    res <- ""

    for (scale in scaleNames)
    {
      res <- paste(res, "1*",scale, " + ", sep = "")
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

  getZeroCovariance <- function(subscales)
  {
    lines <- ""
    for (x in 1:length(subscales))
      for(y in (x+1):length(subscales))
      {
        if (y<=length(subscales) && y != x)
          lines <- paste(lines, subscales[x], "~~0*",subscales[y], "\n ", sep="")
      }
    return(lines)
  }
  GetZeroVarList <- function()
  {
    lines <- ""
    for (z in zeroVar)
    {
      lines <- paste(lines, z, "~~0*",z, "\n ", sep="")

    }
    return (lines)
  }
  CollapseNoLevelInGroup <- function(data,group)
  {
    res <- list()
    for(index in 1:(length(data)-1))
    {
      levelsMat <- table(data[[index]], data[[group]])
      d <- data[[index]]
      repeat {
        CollapseBottom <- function(dCol, level)
        {
          ch <- dCol + 1
          return(ifelse(dCol==as.numeric(level), ch, dCol))
        }
        CollapseTop <- function(dCol,level)
        {
          ch<-dCol-1
          return(ifelse(dCol==as.numeric(level), ch, dCol))
        }

        flag  <- F
        if (0 %in% levelsMat[1,])
        {
          d <- CollapseBottom(d, rownames(levelsMat)[1])
          flag <- T
        }
        if (0 %in% levelsMat[nrow(levelsMat),])
        {
          rn <- rownames(levelsMat)
          d <- CollapseTop(d, rn[length(rn)])
          flag <- T
        }
        if (!isTRUE(flag) && !(0 %in% table(d, data[[group]])))
          break
        else
        {
          if (0 %in% table(d, data[[group]]))
          {
             inZero <- 2
            while(0 %in% table(d, data[[group]]))
            {
              rn <- rownames(levelsMat)
              d = CollapseBottom(d, rn[inZero])
              inZero <- inZero + 1
            }

          }
        }
        levelsMat <- table(d, data[[group]])
      }
      data[[index]] <- d
    }
    return(data)
  }
  GetSimpleModel <- function(scales, subScaleData, ordered, corr = F,
                             group = NULL, invariance = NULL)
  {

    lines <- ""
    for(scale in names(subScaleData))
    {

      if (isTRUE(fixedScales))
      {
        ind <-getSubScaleNamesOne(subScaleData[[scale]])
      }
      else if (scale %in% fixedSubScales)
      {
        ind <-getSubScaleNamesOne(subScaleData[[scale]])

      }
      else
      {
        ind <-getSubScaleNames(subScaleData[[scale]])
      }
      line <- paste(scale, "=~" , ind, "\n", sep=" ")
      lines <- paste(lines, " ", line, sep=" ")
    }
    dep <-  getScaleNames(names(subScaleData))
    if (!isTRUE(corr))
    {
      covariance <- getZeroCovariance(names(subScaleData))
      lines <- paste(lines, covariance, sep = "\n")
    }

    #    lines <- paste(lines, GetZeroVarList(),"\n ", sep = "")


    if (!is.null(group))
      dataFrame <- cbind(getDataFrameSubScale(subScaleData), group)
    else
      dataFrame <- getDataFrameSubScale(subScaleData)

    commands <<- append(commands, list(lines))
    if (length(ordered > 0))
    {
      estimator = "DWLS"
      dataFrame <- CollapseNoLevelInGroup(dataFrame,length(dataFrame))
    }

    return(lavaan::cfa(model = lines, data = dataFrame, ordered = ordered,
                       estimator=estimator, group = names(group), group.equal = invariance))
  }
  # object <- DeleteItems(object, delVar)
  subScaleData <- GetItemWithParcels(scales, object$ScaleItemFrames)
  ordCommand <- c()
  if (isTRUE(ordered))
    ordCommand <- names(getDataFrameSubScale(subScaleData))
  else if(length(ordered) > 0)
    ordCommand <- ordered
  simpModel1 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                   group = object$OtherVariables[group],
                                   corr = T)
  simpModel2 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                   group = object$OtherVariables[group],
                                   invariance =  c("loadings"),
                                   corr = T)
  if (length(ordered) > 0)
  {
    simpModel3 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                 group = object$OtherVariables[group],
                                 invariance =  c("loadings", "thresholds"),
                                 corr = T)
    simpModel4 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                 group = object$OtherVariables[group],
                                 invariance =  c("loadings", "thresholds", "residuals", "means" ),
                                 corr = T)

  }
  else
  {
    browser()
    models <- list(fit.configural = simpModel1, fit.loadings = simpModel2)
    print(partialInvariance(models,  type = "metric"))
  simpModel3 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                   group = object$OtherVariables[group],
                                   invariance =  c("loadings", "intercepts"),
                                   corr = T)
  simpModel4 <- GetSimpleModel(scale, subScaleData, ordCommand,
                                   group = object$OtherVariables[group],
                                   invariance =  c("loadings", "intercepts", "residuals" ),
                                   corr = T)
  }
  print(anova(simpModel1,simpModel2,simpModel3,simpModel4))
  object$RCommands <- commands
  class(object) <- c("GroupInvariance", "Psychometric")
  object$ResultList <- list(simpModel1, simpModel2,simpModel3,simpModel4)
  names(object$ResultList) <- cs(simpModel1, simpModel2,simpModel3,simpModel4)

  return(object)


}
