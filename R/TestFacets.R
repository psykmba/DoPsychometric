
#' Facet test with CFA
#'
#' @param object  a Psychometric object
#' @param scale a Scale name
#' @param subscales a vector of facet names
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
TestFacets <- function(object,scale, subscales, fixed = F,
                       fixedScales = F,parcel = F, fixedSubScales = c(),
                       tries = 1, estimator = "ML", zeroVar = c(),
                       delVar = c()) {
  UseMethod("TestFacets", object)
}

#' Facet test with CFA
#'
#' @param object  a Psychometric object
#' @param scale a Scale name
#' @param subscales a vector of facet names
#' @param fixed whether the bi-factor should be fixed to 1
#' @param fixedScales whether the scales should be fixed to 1
#' @param fixedSubScales fix these subscales to 1
#' @param tries how many attempt to estimate model
#' @param parcel whether the observed variables should be in parcels
#' @param estimator any estimator that is admissible for cfa
#' @param zeroVar var set to zero error variance
#' @param delVar delete some variables in a previous model
#'
#' @return a TestFacet model
#' @export
TestFacets.Psychometric <- function(object, scale, subscales, fixed = F,
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

    for(subscale in subscales)
    {
      dataFrames <- ScaleItemFrames[[subscale]]
      dataFrames <- MakeParcels(dataFrames)
      dataFrames <- list(CombineParcels(dataFrames))

      names(dataFrames) <- subscale
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

  GetHierarchicalModel <- function(scales, subScaleData)
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
    if (isTRUE(fixed))
      dep <-  getScaleNamesOne(names(subScaleData))
    else
      dep <-  getScaleNames(names(subScaleData))

      covariance <- getZeroCovariance(names(subScaleData))

      lines <- paste(lines, scales, "=~", dep,"\n ", covariance,"\n ", sep = "")
    lines <- paste(lines, GetZeroVarList(),"\n ", sep = "")

    dataFrame <- getDataFrameSubScale(subScaleData)


    commands <<- append(commands, list(lines))

    return(lavaan::cfa(model = lines, data = dataFrame, estimator=estimator))
  }
  GetSimpleModel <- function(scales, subScaleData, corr = F)
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

    dataFrame <- getDataFrameSubScale(subScaleData)

    commands <<- append(commands, list(lines))

    return(lavaan::cfa(model = lines, data = dataFrame, estimator=estimator))
  }
  GetTestSubscaleAll <-function(dependent, subScaleData)
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
      #        ind1 <-getSubScaleNamesOne(subscaleData[[scale]])
      line <- paste(scale, "=~" , ind, "\n", sep=" ")

      if (isTRUE(fixed))
      {
        ind1 <-getSubScaleNamesOne(subScaleData[[scale]])
        lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
      }
      else
      {
        ind1 <-getSubScaleNames(subScaleData[[scale]])
        lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
      }
      lines <- paste(lines, " ", line,lineBiv, sep=" ")
    }
    scaleNames <- names(subScaleData)
    covariance <- getZeroCovariance(c(scaleNames, dependent))
    lines <- paste(lines, covariance,"\n ", sep = "")

    dataFrame <- getDataFrameSubScale(subScaleData)



    commands <<- append(commands, list(lines))

    return(lavaan::cfa(model = lines, data = dataFrame, estimator=estimator))

  }

  GetTestSubscale <-function(dependent, zeroScale, subScaleData)
  {
    lines <- ""
    for(scale in names(subScaleData))
    {

      if(scale == zeroScale)
      {
        #        indZero <-getSubScaleNamesZero(subscaleData[[scale]])
        #        ind1 <-getSubScaleNamesOne(subscaleData[[scale]])
        if (isTRUE(fixed))
        {
          ind1 <-getSubScaleNamesOne(subScaleData[[scale]])
          lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
        }
        else
        {
          ind1 <-getSubScaleNames(subScaleData[[scale]])
          lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
        }
        lines <- paste(lines, " ", lineBiv, sep=" ")

      }
      else
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
        if (isTRUE(fixed))
        {
          ind1 <-getSubScaleNamesOne(subScaleData[[scale]])
          lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
        }
        else
        {
          ind1 <-getSubScaleNames(subScaleData[[scale]])
          lineBiv <- paste(dependent, "=~" , ind1, "\n", sep=" ")
        }
        lines <- paste(lines, " ", line,lineBiv, sep=" ")
      }
    }
    scaleNames <- names(subScaleData)
    scaleNames <- scaleNames[-which(scaleNames == zeroScale)]
    covariance <- getZeroCovariance(c(scaleNames, dependent))
    lines <- paste(lines, covariance,"\n ", sep = "")
    lines <- paste(lines, GetZeroVarList(),"\n ", sep = "")

    dataFrame <- getDataFrameSubScale(subScaleData)

    commands <<- append(commands, list(lines))

    return(lavaan::cfa(model = lines, data = dataFrame, estimator=estimator))

  }

  DeleteItems <- function(object, delVar)
  {
   for (x in 1:length(object$ScaleItemFrames))
    {
      for (v in delVar)
      {
        if (v %in% names(object$ScaleItemFrames[[x]]))
        {
          object$ScaleItemFrames[[x]] <- object$ScaleItemFrames[[x]][ , -which(names(object$ScaleItemFrames[[x]]) %in% v)]
         }
      }
   }
    return(object)
  }
  MainCall <- function()
  {
    object <- DeleteItems(object, delVar)
   subScaleData <- GetItemWithParcels(subscales, object$ScaleItemFrames)
    hierModel <- list(GetHierarchicalModel(scale, subScaleData))
    names(hierModel) <- "Hierarchical Model"
    result <- list(GetTestSubscaleAll(scale, subScaleData))
    names(result) <- "Bifactor Total"
    for (subscale in names(subScaleData))
    {
      tillf <- list(GetTestSubscale(scale, subscale,subScaleData))
      names(tillf) <- subscale
      result <- append(result,tillf)
    }
    simpModel <- list(GetSimpleModel(scale, subScaleData))
    simpModel2 <- list(GetSimpleModel(scale, subScaleData, corr = T))
    names(simpModel) <- "Factor model"
    names(simpModel2) <- "Factor model with cor"

    object$ResultList <- append(append(append(hierModel, result), simpModel), simpModel2)
    object$RCommands <- commands
    class(object) <- c("TestFacets", "Psychometric")

     return(object)
  }
  if (tries == 1)
    return(MainCall())
  rounds <- 1
  success <- F
  ret <- NULL
  while (rounds <= tries)
  {
    assign("last.warning", NULL, envir = baseenv())
    message(paste("Attempt", rounds, sep = " "))


    ret <- MainCall()
    if(is.null(warnings()))
      break
    rounds <- rounds + 1
  }
  class(ret) <- c("TestFacets", "Psychometric")

  return(ret)
}

#' A Psychometric CFA estimator
#'
#' @param object an of class TestFacets
#' @param model a text string with an cfa model
#' @param what can be "SetNegativeVar"
#' @param exclude a list of items to be deleted
#'
#' @return a new Psychometric model with a new CFA for model
#' @export
RunCFA <- function(object, model, what = NULL, exclude = NULL) {
  UseMethod("RunCFA", object)
}

#' A Psychometric CFA estimator
#'
#' @param object  an of class TestFacets
#' @param model  a text string with an cfa model
#' @param what can be "SetNegativeVar"
#' @param exclude a list of items to be deleted
#'
#' @return a new Psychometric model with a new CFA for model
#' @export
RunCFA.Psychometric <- function(object, model, what = NULL, exclude = c())
{
  GetAllScaleItemFrames <-function()
  {
    data <- data.frame(row.names = 1:nrow(object$ScaleFrame))
    for(frames in object$ScaleItemFrames)
      data <- cbind(data, frames)
    return(data)
  }

  GetVarNegative <- function(rsquare)
  {
    Names <- c()
    for (index in 1:length(rsquare))
    {
      if (is.na(rsquare[index]) || rsquare[index] < 0 )
        Names <- c(Names, names(rsquare[index]))

    }
    return(Names)
  }

  DeleteVar <- function(com)
  {
    if (is.null(exclude))
      return(com)
    stringVect <- stringr::str_split(com, "\n")
    res <- ""
    for(s in stringVect[[1]])
    {
      if (!(exclude %in% s))
      {
        res <- paste(res, s, "\n", sep = "")
      }
    }
    return(res)
  }

  if (is.numeric(model))
  {
    if (is.null(what))
    {
      object$ResultList[model] <- list(lavaan::cfa(data = GetAllScaleItemFrames(), model = object$RCommands[[model]]))
      return(object)
    }
    if (what == "SetNegativeVar")
    {
      rsquare <- lavaan::lavInspect(object$ResultList[[model]], what = "rsquare")
      NegVar <- GetVarNegative(rsquare)
      com <-  object$RCommands[[model]]
      if (length(NegVar) > 0)
        for (index in 1:length(NegVar))
        {
          com <- paste(com, "\n", NegVar, "~~0*", NegVar )
        }
      com <- DeleteVar(com)
      object$RCommands[[model]] <- list(com)
      object$ResultList[[model]] <-lavaan::cfa(data = GetAllScaleItemFrames(), model = com)
      return(object)
    }
  }
  object$RCommands[model] <- model
  object$ResultList[model] <- list(lavaan::cfa(data = GetAllScaleItemFrames(), model = model))

  return(object)
}


#' A Psychometric FA estimator
#'
#' @param object an of class TestFacets
#' @param subscales the subscales that are included
#' @param ... extra argument to the fa
#' @return a psych::fa result based on the selected subscales
#' @export
RunFA <- function(object, subscales, ...) {
  UseMethod("RunFA", object)
}

#' A Psychometric FA estimator
#'
#' @param object an of class TestFacets
#' @param subscales the subscales that are included
#' @return a psych::fa result based on the selected subscales
#' @export
RunFA <- function(object, subscales,...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  nfactors = GetExtraArgument("nfactors", NULL)
  rotate = GetExtraArgument("rotate", "promax")
  dataF <- data.frame(row.names = 1:nrow(object$ScaleFrame))
  for (itemF in object$ScaleItemFrames[subscales])
  {
    dataF <- cbind(dataF, itemF)
  }
  if (is.null(nfactors))
    return (psych::fa.sort(psych::fa(dataF, nfactors = length(subscales), rotate = rotate)))
  else
    return (psych::fa.sort(psych::fa(dataF, nfactors = nfactors, rotate = rotate)))

}
#' A Psychometric FA estimator
#'
#' @param object an of class TestFacets
#' @param subscales the subscales that are included
#' @param ... extra argument to the fa
#' @return a psych::fa result based on the selected subscales
#' @export
RunPCA <- function(object, subscales, ...) {
  UseMethod("RunPCA", object)
}

RunPCA <- function(object, subscales,...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  nfactors = GetExtraArgument("nfactors", NULL)
  rotate = GetExtraArgument("rotate", "promax")
  dataF <- data.frame(row.names = 1:nrow(object$ScaleFrame))
  for (itemF in object$ScaleItemFrames[subscales])
  {
    dataF <- cbind(dataF, itemF)
  }
  if (is.null(nfactors))
    return (psych::fa.sort(psych::pca(dataF, nfactors = length(subscales), rotate = rotate)))
  else
    return (psych::fa.sort(psych::pca(dataF, nfactors = nfactors, rotate = rotate)))

}

#' summary for TestFacets
#'
#' @param object an object of class TestFacets
#' @param ... model and standardized and more which model to print amd whether standardized solution should be printed
#'
#' @return summary of the lavaan results in the object
#' @export
summary.TestFacets <- function(object, ...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  model = GetExtraArgument("model")
  standardized = GetExtraArgument("standardized", F)
  if (is.numeric(model))
  {
    if (isTRUE(standardized))
    {
      print(lavaan::standardizedSolution(object$ResultList[[model]],type = "std.all"))
    }
    else
    {
      print(lavaan::summary(object$ResultList[[model]]), header=F, estimates=T,
            fit.measures=TRUE)
    }

  }
  else
  {
    if (isTRUE(standardized))
    {
      return(sapply(object$ResultList, FUN = function(x) {
        lavaan::standardizedSolution(x, type = "std.all")}))



    }
    else
    {
      return (sapply(object$ResultList, FUN = lavaan::summary,header=F, estimates=T,
                     fit.measures=TRUE))

    }


  }
}



#' Title
#'
#' @param x an object of class TestFacets
#' @param ... model and standardized and more which model to print amd whether standardized solution should be printed
#'
#' @return summary of the lavaan results in the object
#' @export
print.TestFacets <- function(x, ...)
{
  GetExtraArgument <- function(a, default = NULL)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(default)

  }
  model = GetExtraArgument("model", "all")
  standardized = GetExtraArgument("standardized", F)

  if (is.numeric(model))
  {
    return(lavaan::summary(x$ResultList[[model]]))

  }
  else
  {

    return (x$ResultList)
  }
}

#
#' anova TestFacets
#'
#' @param object an object of class TestFacets
#' @param ... type which model to print and more extra
#'
#' @return summary of the lavaan results in the object
#' @export
anova.TestFacets <- function(object, ...)
{
  GetExtraArgument <- function(a)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return("All")

  }
  type = GetExtraArgument("type")

  if (type == "Hiearchical" || type == "H")
    compModel <- 1
  else
    compModel <-2
  if (!isTRUE(lavaan::lavInspect(object$ResultList[[compModel]], "post.check")))
    return("Comparison model has not converged")
  if (type == "H") print("Comparison modell hierarchical")
  else
    print("Comparison modell bifactor")
  print(lavaan::fitmeasures(object$ResultList[[compModel]],c("cfi", "rmsea" ,"srmr_mplus")))

  if (length(object$ResultList)>2)
  {
    if (compModel == 1)
    {
      print(names(object$ResultList[2]))
      print(lavaan::fitmeasures(object$ResultList[[2]],c("cfi", "rmsea" ,"srmr_mplus")))
      print(lavaan::lavTestLRT(object$ResultList[[2]],
                               object$ResultList[[compModel]]))
      print(names(object$ResultList[length(object$ResultList)]))
      print(lavaan::fitmeasures(object$ResultList[[length(object$ResultList)-1]],c("cfi", "rmsea" ,"srmr_mplus")))
      print(lavaan::lavTestLRT(object$ResultList[[length(object$ResultList)-1]],
                               object$ResultList[[compModel]]))

    }
    else

      for(index in 3:length(object$ResultList))
      {
        print(names(object$ResultList[index]))
        fRes <- lavaan::fitmeasures(object$ResultList[[index]],c("cfi", "rmsea" ,"srmr"))
        print(fRes)
        lRes <- lavaan::lavTestLRT(object$ResultList[[index]],
                                   object$ResultList[[compModel]], type = "Chisq")
        row.names(lRes) <- c(names(object$ResultList[compModel]),names(object$ResultList[index]))
        print(lRes)
      }
  }
}


#' Get factor scores
#'
#' @param object a TestFacet object
#' @param model which model to return
#'
#' @return a dataframe with factor scores
#' @export
getPredict <- function(object, model) {
  UseMethod("getPredict.TestFacets", object)
}

#' Get factor scores
#'
#' @param object a TestFacet object
#' @param model which model to return
#'
#' @return a dataframe with factor scores
#' @export
getPredict.TestFacets <- function(object, model)
{
  res <- data.frame(lavaan::predict(object$ResultList[[model]]))
  return(res)
}

