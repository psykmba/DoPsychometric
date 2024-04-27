#' TestCongruence
#'
#' @param objectList  Psychometric objects to compare
#' @param fms Which method from psych::fa to extract factors
#' @param rotates Vector with psych::fa rotation names to be compared
#' @param itemNums Vector of integers used to compare scales based on fewer items. Items selected with psych::BestScales
#' @param factors Vector of integer of number of factors to test in each model
#' @param printFlag Bool for whether the congruence tables should be printed
#' @param printLoadings Bool for whether the loadings should be printed
#' @param printAll Bool for whether the factor model should be printed
#'
#' @return A list with all congruent tested models with their respective congruence value (mean of best match)
#' @export
TestCongruence <- function(objectList, fms = "ml", rotates = "oblimin",
                           itemNums = 50, factors = 4, printFlag = F,
                           printLoadings = F, printAll = F) {
  UseMethod("TestCongruence", object)
}


#' TestCongruence
#'
#' @param objectList Psychometric objects to compare
#' @param fms Which method from psych::fa to extract factors
#' @param rotates Vector with psych::fa rotation names to be compared
#' @param itemNums Vector of integers used to compare scales based on fewer items. Items selected with psych::BestScales
#' @param factors Vector of integer of number of factors to test in each model
#' @param printFlag Bool for whether the congruence tables should be printed
#' @param printLoadings Bool for whether the loadings should be printed
#'
#' @return  A list with all congruent tested models
#' @export
TestCongruence <- function(objectList, fms = "ml", rotates = "oblimin",
                           itemNums = 50, factors = 4, printFlag = F,
                            printLoadings = F, printAll = F)
{
  listRes <- list()
  namesV <- c()
  GetFM <- function(mod)
  {
    if (mod$fn == "principal")
      return ("pca")
    else
      return(mod$fm)
  }

  Congr <- function(mod1, mod2, out)
  {
    cRes <- psych::fa.congruence(mod1$loadings,mod2$loadings)
    if (isTRUE(printFlag) || isTRUE(printLoadings || isTRUE(printAll)))
    {
      cat("\n\n\n")
      print("Next model:")
      print(out)
      cat("\n")
    }
    if (isTRUE(printLoadings))
    {
      names(mod1$loadings) <- stringr::str_c("M1_", names(mod1$loadings))
      names(mod2$loadings) <- stringr::str_c("M2_", names(mod2$loadings))
      print(round(cbind(mod1$loadings, mod2$loadings),2))
    }
    if (isTRUE(printAll))
    {
      cat("\nModel 1\n")
      print(mod1)
      cat("\nModel 2\n")
      print(mod2)
    }
    cMax <- apply(as.data.frame(cRes), FUN = function(x) {return(max(abs(x)))}, MARGIN = 1)
    if (isTRUE(printFlag) || isTRUE(printLoadings) || isTRUE(printAll))
    {
      cat("\n\n\n")
      print("Congruence statistics")
      print(cRes)
      cat("\n")
      print(cMax)
      cat("\n\n\n")
    }

   return (mean(cMax))
  }
  GetModel <- function(object, fm, rotate, itemNum, factor)
  {

    bestObject <- bestScales.Psychometric(object, itemNum, deleteItems = T)
    if (fm == "pca")
      faResFull <- psych::pca(bestObject$ScaleFrame, nfactors = factor, rotate = rotate)

    else
      faResFull <- psych::fa(bestObject$ScaleFrame, nfactors = factor, fm = fm, rotate = rotate)
    faResFull$itemNum = itemNum
    faResFull$Name <- object$Name
    return(faResFull)


  }
   CompareObjects <- function(x,y, listRes)
  {
    Res <- NULL
    namesV <- NULL
    for(fm in fms)
      for (rotate in rotates)
        for(numItem in itemNums)
          for(fact in factors)
          {
            mod1 <- GetModel(x,fm, rotate, numItem, fact)
            mod2 <- GetModel(y,fm, rotate, numItem, fact)
            out <- paste("Object: ", x$Name, y$Name,
                         "Compare:",GetFM(mod1),
                         "Rotate:",mod1$rotation,
                         "NumItem:", mod1$itemNum,
                         "Factors:", mod1$factors, sep = " ")
            Res <- c(Res, Congr(mod1, mod2, out))
            namesV <- c(namesV,out)
          }
     names(Res) <- namesV
    return(c(listRes, Res))
  }

  CompareFMS <- function(x,y, listRes)
  {
    Res <- NULL
    namesV <- NULL
    for(object in objectList)
    for (rotate in rotates)
      for(numItem in itemNums)
        for(fact in factors)
        {
          mod1 <- GetModel(object,x, rotate, numItem, fact)
          mod2 <- GetModel(object,y, rotate, numItem, fact)
          out <- paste("Object: ", object$Name,
                       "Compare:",GetFM(mod1),GetFM(mod2),
                       "Rotate:",mod1$rotation,
                       "NumItem:", mod1$itemNum,
                       "Factors:", mod1$factors, sep = " ")
          Res <- c(Res, Congr(mod1, mod2,out))
          namesV <- c(namesV, out)
        }
    names(Res) <- namesV
    return(c(listRes, Res))
  }
  CompareRotates <- function(x,y, listRes)
  {
    Res <- NULL
    namesV <- NULL
    for(object in objectList)
      for (fm in fms)
      for(numItem in itemNums)
        for(fact in factors)
        {
           mod1 <- GetModel(object,fm, x, numItem, fact)
          mod2 <- GetModel(object,fm, y, numItem, fact)
          out <-paste("Object: ", object$Name,
                      "Compare:",GetFM(mod1),
                      "Rotate:",mod1$rotation,mod2$rotation,
                      "NumItem:", mod1$itemNum,
                      "Factors:", mod1$factors, sep = " ")
          Res <- c(Res,  Congr(mod1, mod2, out))
          namesV <- c(namesV,out)
        }
    names(Res) <- namesV
    return(c(listRes, Res))

  }

  CompareItemNum <- function(x,y, listRes)
  {
    Res <- NULL
    namesV <- NULL
     for(object in objectList)
      for(fm in fms)
        for (rotate in rotates)
          for(fact in factors)
          {
            mod1 <- GetModel(object,fm, rotate, x, fact)
            mod2 <- GetModel(object,fm, rotate, y, fact)
            out <- paste("Object: ",object$Name,
                         "Compare:",GetFM(mod1),
                         "Rotate:",mod1$rotation,
                         "NumItem:", mod1$itemNum,mod2$itemNum,
                         "Factors:", mod1$factors, sep = " ")
            Res <- c(Res,  Congr(mod1, mod2, out))
            namesV <- c(namesV,out)
          }
     names(Res) <- namesV
    return(c(listRes, Res))

  }
  CompareFactors <- function(x,y, listRes)
  {
    Res <- NULL
    namesV <- NULL
    for(object in objectList)
      for(fm in fms)
      for (rotate in rotates)
        for(itemNum in itemNums)
        {
          mod1 <- GetModel(object,fm, rotate, itemNum, x)
          mod2 <- GetModel(object, fm, rotate, itemNum, y)
          out <- paste("Object: ", names(object),
                       "Compare:",GetFM(mod1),
                       "Rotate:",mod1$rotation,
                       "NumItem:", mod1$items,
                       "Factors:", mod1$factors,mod2$factors, sep = " ")
          Res <- c(Res, Congr(mod1, mod2, out))
          namesV <- c(namesV,out)
        }
    names(Res) <- namesV
    return(c(listRes, Res))
  }

   if (length(objectList) > 1)
  {
    for(X in 1:(length(objectList)-1))
      for(Y in (X+1):length(objectList))
      {
        listRes <- CompareObjects(objectList[[X]], objectList[[Y]], listRes)
      }
  }
  if (length(fms) > 1)
  {
    for(X in 1:(length(fms)-1))
      for(Y in (X+1):length(fms))
        listRes <- CompareFMS(fms[X], fms[Y], listRes)
  }
  if (length(rotates) > 1)
  {
    for(X in 1:(length(rotates)-1))
      for(Y in (X+1):length(rotates))
        listRes <- CompareRotates(rotates[X], rotates[Y], listRes)
  }
  if (length(itemNums) > 1)
  {
    for(X in 1:(length(itemNums)-1))
      for(Y in (X+1):length(itemNums))
        listRes <- CompareItemNum(itemNums[X], itemNums[Y], listRes)

  }
  # if (length(factors) > 1)
  # {
  #   for(X in 1:(length(factors)-1))
  #     for(Y in (X+1):length(factors))
  #       listRes <- CompareFactors(factors[X], factors[Y], listRes)
  # }
  names(listRes) < namesV

  return(listRes)

}

TestTwoFactorScales <- function(object)
{
  for(scale in object$ScaleItemFrames)
  {
    res <- fa(scale, nfactors = 2)
  }
}
