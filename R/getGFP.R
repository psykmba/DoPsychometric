#' Get All GFPs
#'
#' @param object a TestFacet object
#' @param scales a sublist of scales
#' @param parceling should facets be parceled
#' @param fixed should general factors be fixed to 1
#' @param fixedScales should scales be fixed to 1
#' @param fixedSubScales should subScales be fixed to 1
#'
#' @return an extended TestFacet object
#' @export
getGFP <- function(object, scales = NULL,
                   parceling = F, fixed = F, fixedScales = F,
                   fixedSubScales = c())
  {
  UseMethod("getGFP", object)
}


#' Get All GFPs
#'
#' @param object a TestFacet object
#' @param scales a sublist of scales
#' @param parceling should facets be parceled
#' @param fixed should general factors be fixed to 1
#' @param fixedScales should scales be fixed to 1
#' @param fixedSubScales should subScales be fixed to 1
#'
#' @return an extended TestFacet object
#' @export
getGFP.Psychometric <- function(object, scales = NULL,
                                parceling = F, fixed = F, fixedScales = F,
                                fixedSubScales = c())
{
   if (is.null(scales))
  {
      gPCA <-psych::pca(object$ScaleFrame, nfactors = 1)
#      PCA <-psych::pca(object$OriginalData, nfactors = 1)

      testF <- TestFacets(object, scales = "GFP", subscales = object$ScaleNames,
                          parcel = parceling, fixed = fixed, fixedScales = fixedScales,
                          fixedSubScales = fixedSubScales)
       om <- psych::omega(object$OriginalData, nfactors = length(object$ScaleNames))
  }
  else
  {
    object <- select.Psychometric(object, scales)
    if (isTRUE(parceling))
      object$ScaleItemFrames <- GetItemWithParcels(object, parceling, scales)
    gPCA <-psych::pca(object$ScaleFrame, nfactors = 1)
    data <- getData.Psychometric(object, scales, otherVar = F, scaleFrame = F, itemFrames = T)

    PCA <-psych::pca(data, nfactors = 1)
    testF <- TestFacets(object, scale = "GFP", subscales = scales,
                        parcel = parceling, fixed = fixed, fixedScales = fixedScales,
                        fixedSubScales = fixedSubScales)
    om <- psych::omega(data, nfactors = length(scales))

  }
    r1 <- list(gPCA)
  names(r1) <- "gPCA"
  r2 <- list(PCA)
  names(r2) <- "PCA"
  r3 <- list(om)
  names(r3) <- "Omega"

  testF$ResultList <- append(testF$ResultList,r1)
  testF$ResultList <- append( testF$ResultList,r2)
  testF$ResultList <- append( testF$ResultList,r3)


  return(testF)

}


#' Get correlation GFPs
#'
#' @param object a Psychometric object
#'
#' @return crrrelation matrix
#' @export
corGFP <- function(object)
{
  UseMethod("corGFP", object)
}



#' Get correlation GFPs
#'
#' @param object a Psychometric object
#'
#' @return crrrelation matrix
#' @export
corGFP.Psychometric <- function(object)
{
  v1 <- data.frame(object$ResultList[[length(object$ResultList)-2]]$scores)
  v2 <- data.frame(object$ResultList[[length(object$ResultList)-1]]$scores)
  v3 <- data.frame(object$ResultList[[length(object$ResultList)]]$scores[,1])
  v4 <- data.frame(getPredict.TestFacets(object, 1)$GFP)
  v5 <- data.frame(getPredict.TestFacets(object, 2)$GFP)
  d <- cbind(v1, v2, v3, v4, v5)
  names(d) <- c("gCFA", "CFA", "Omega", "Hiearch", "Bifactor")
  d <- cbind(d, object$ScaleFrame)
  print("Correlation with original variables")
  print(round(cor(d),2))

  v5 <- data.frame(getPredict.TestFacets(object, 2))
  d <- cbind(v1, v2, v3, v4)
  names(d) <- c("gCFA", "CFA", "Omega", "Hiearch")
  d <- cbind(d, v5)
  print("Correlation with bifactor variables")
  print(round(cor(d),2))

}


#' Get commands for models
#'
#' @param object a TestFacet object
#' @param model a model number
#'
#' @return a model
#' @export
getCommand2.Psychometric <- function(object, model)
{
  return(getCommand.TestFacets(object, model = model))
}
