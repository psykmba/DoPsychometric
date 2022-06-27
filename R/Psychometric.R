#' @export
GetPsychometric <- function(data, scaleNames, responseScale = list(c(1,5)),
                            typeSum = "Mean", itemLength = 6,
                            reverse = T, idVar = "ID", name = "Psychometric",
                            itemDictionary = NULL)
{
  # if there is a variable called "ID" this variable is added to all the data
  # frames
  IDVar <- NULL
  if (!is.null(data[[idVar]]))
  {
    IDVar <- data[[idVar]]
  }
  if (FALSE %in% sapply(scaleNames, FUN = function(x) return(stringr::str_length(x) >= itemLength)))
  {
    print(paste("Error: itemLength = ", itemLength, "is larger than the string length of the shortes scale name"))
    return()
  }

  expandResponsScale <- function(respons, scales)
  {
    l <- length(respons) # Number of respons types
    s <- length(scales) # Number of scales
    if (l > 1 && l != s)
      warning("Respons scale is not 1 and not the same as the number of scales using the first for all",
              immediate. = T)
    if (l == 1 || l != s)
      ret <- rep(respons[1], s)
    else 
      ret <- respons
    names(ret) <- scales
    return(ret)
  }
  
  GetNonItemVar <- function()
  {
    scaleNamesItemLength <- unlist(sapply(scaleNames, FUN = function(x)
      return(substr(x, 1, itemLength))), use.names = F)
    res <- NULL
    for (index in ncol(data):1)
    {
      name <- names(data[index])
      if (!(substr(name, 1, itemLength) %in% scaleNamesItemLength))
      {
        res <- c(res, data[index])
        data <- data[-index]
      }
    }
    return(data.frame(res))
    
  }


  getSignItemName <- function(x, scales, itemLength)
  {
    signPart <- strtrim(scales, itemLength)
    
    if ( stringr::str_length(scales) > itemLength)
    {
      newNames <- NULL
      for (item in 1:ncol(x))
      {
        c <- ""
        iName <- names(x[item])
        for (s in itemLength+1: stringr::str_length(scales))
        {
          
          if (substring(scales, s, s) == substring(iName, s,s))
          {
            c <- paste(c, substring(scales, s, s), sep = "")
          }
          else
            break
        }
        newNames <-c(newNames,  stringr::str_remove(names(x[item]),c))
      }
      names(x) <- newNames
    }
    return(x)
  }
  
 
  
  ChangeOriginalDataNames <- function(data)
  {
    signPart <- strtrim(scaleNames, itemLength)
    for(v in names(data))
    {
      if (str_length(v) > itemLength)
      {
        if (substr(v, 1, itemLength) %in% signPart)
        {
          iName <- scaleNames[match(substr(v, 1, itemLength),signPart)]
          if (str_length(iName) > itemLength)
          {
            c <- ""
            for (s in itemLength+1:str_length(v))
            {
              
              if (substring(v, s, s) == substring(iName, s,s))
              {
                c <- paste(c, substring(v, s, s), sep = "")
              }
              else
                break
            }
            names(data)[names(data) == v] <- str_remove(v,c)
          }
        }
      }
      
    }
    return(data)
  }
  GetScaleItemFrames <- function(d, responses)
  {
    
    GetData <- function(name)
    {
      frame <- dplyr::select(d, dplyr::starts_with(substr(name, 1, itemLength)))
      return(frame)
    }
    Reverse <- function(col, resp)
    {
      if (reverse == T)
        return((resp[1]+resp[2]) - col)
      else
        return(col)
    }
    GetReverse <- function(frame, resp)
    {
      #      print(resp[[1]])
      for(index in 1:length(frame))
      {
        name <- names(frame[index])
        frame[index] <- ifelse(substr(name, nchar(name), nchar(name)) == "R", 
                               Reverse(frame[index], resp[[1]]),
                               frame[index])
      }
      return(frame)
    }
    resFrames <- NULL
    for(index in 1:length(scaleNames))
    {
      interm <- GetData(scaleNames[index])
      interm <- getSignItemName(interm,scaleNames[index], itemLength )
      resFrames <- append(resFrames, list(as.data.frame(GetReverse(interm, responses[index]))))
    }
    names(resFrames) <- scaleNames
    return(resFrames)
  }
  
  GetScalesFrame <- function(frames, nameV)
  {
    res <- IDVar
    names(res) <- "ID"
    for (index in 1:length(frames))
    {
      res <- cbind(res, rowMeans(as.data.frame(frames[index]), na.rm = F))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    if (!is.null(IDVar))
      names(res) <- c("ID", nameV)
    else
      names(res) <- nameV
    return(res)
    
    
  }
  GetDictionary <- function()
  {
    d <- NULL
    if (file.exists(itemDictionary))
      d <- read.delim(itemDictionary, comment.char="#")
    else
      print("Dictionary file does not exist")
    rowNames <- NULL
    for(index in 1:nrow(d))
      rowNames <- c(rowNames, paste(substr(d[index, 4],1,itemLength), d[index,1], sep = ""))
    return(data.frame(cbind(d[2], d[3], d[4], row.names = rowNames)))
  }
  otherVariables <- GetNonItemVar()
  responseScale <- expandResponsScale(responseScale, scaleNames)
  scaleItemFrames <- GetScaleItemFrames(data, responseScale)
  scaleFrames <- GetScalesFrame(scaleItemFrames, scaleNames)
  data <- ChangeOriginalDataNames(data)
  names(scaleItemFrames) <- scaleNames
  if (!is.null(itemDictionary)) 
    itemDictionary <- GetDictionary()
  else
    itemDictionary <- list();
  MyObject <- list(ResponseScales = responseScale, ScaleItemFrames = scaleItemFrames, ScaleFrame = scaleFrames, 
                   ScaleNames = scaleNames, OtherVariables = otherVariables, OriginalData = data, 
                   Name = name, ItemDictionary = itemDictionary,
                   ItemLength = itemLength, ResultList = list(), RCommands = list())
  
  class(MyObject) <- "Psychometric"
  return(MyObject)
  
}



#' @export
reliabilityTest <- function(x, ...) {
  UseMethod("reliabilityTest", x)
}

#' @export
reliabilityTest.Psychometric <- function(object, what = "Alpha")
{
  reslist2 <- NULL
  printres <- NULL
  GetAlphaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("alpha(object$OriginalData[cs(", n, ")], check.keys = T,keys=NULL,",
                                    "cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, ",
                                    "n.iter=1,delete=TRUE,use='pairwise',warnings=TRUE,",
                                    "n.obs=NULL,impute=NULL)", sep = "")))
    }
    return(res)
  }
  
  GetOmegaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("omega(object$OriginalData[cs(", n, ")],nfactors=3,fm='minres',n.iter=1,",
                                    "p=.05,poly=FALSE,key=NULL,flip=TRUE,digits=2, title='Omega',",
                                    "sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate='oblimin',",
                                    "Phi=NULL,option='equal',covar=FALSE)", sep = "")))
    }
    return(res)
    
  }
  GetParallelCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("fa.parallel(object$OriginalData[cs(", n, ")]", sep = "")))
    }
    return(res)
    
  }
  
  if (what == "Alpha")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = function(x) {return(psych::alpha(x, check.keys = T))})
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$total$raw_alpha)))
    object$RCommands <- GetAlphaCommands()
    
  }
  if (what == "Omega")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=FALSE)
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$omega_h)))
    object$RCommands <- GetOmegaCommands()
    
  }
  if (what == "Parallel")
  {
    resList2 <- mapply(object$ScaleItemFrames, FUN = 
                         function(x,name) 
                         {
                           list(psych::fa.parallel(x, main = paste("Analysis for ", name), plot = F))},
                       names(object$ScaleItemFrames))
    
    printres <- as.data.frame(lapply(resList2, FUN = function(x,y) return(x$nfact)))
    object$RCommands <- GetParallelCommands()
    
    
  }
  
  
  names(resList2) <- object$ScaleNames
  names(printres) <- object$ScaleNames
  rownames(printres) <- what
  object$ResultList <- resList2
  object$PrintRes <- printres
  names(object$RCommands) <- object$ScaleNames
  class(object) <- c( "Reliability", "Psychometric")
  return(object)
}



#' @export

summary.Reliability <- function(object, scale = NULL)
{
  if (is.null(scale))
    print(object$ResultList)
  else
  {
    if (scale %in% names(object$ScaleFrame))
      print(object$ResultList[scale])
  }
}
#' @export
print.Reliability <- function(object, plot = F)
{
  print(object$PrintRes)
}
#' @export
plot.Reliability <- function(object, main = "Relibility", sub = "", 
                             xlab = "X-axis", ylag = "Y-axis", what = "",
                             scale = NULL)
{
  if (class(object)[1] == "Reliability")
  {
    if (is.null(scale))
    {
      lapply(object$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=T)
    }
    else if (!is.null(object$ScaleItemFrames[[scale]]))
      psych::omega.diagram(object$ResultList[[scale]])
    #      omega(object$ScaleItemFrames[[scale]], nfactors = 3, plot = T)
    else if (what == "Parallel")
    {
      if (is.null(scale))
        sapply(object$ResultList, FUN = plot)
      else 
        plot(object$ResultList[[scale]])
    }
    
  }
}

#' @export
getMissing <- function(x, ...) {
  UseMethod("getMissing", x)
}

#' @export
getMissing.Psychometric <- function(object, handleMissing = Listwise)
{
  HandleMissing <- function(dataToHandle)
  {
    if (handleMissing == "Impute")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm.predict")
      return(mice::complete(imputed))
      
    }    
    if (handleMissing == "Mean")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "mean")
      return(mice::complete(imputed))
      
    }
    if (handleMissing == "Bayesian")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm")
      return(mice::complete(imputed))
      
    }
    if (handleMissing == "Listwise")
    {
      
      return(dataToHandle[complete.cases(dataToHandle),])
      
    }
  }
  for(index in 1:length(object$ScaleItemFrames))
  {
    object$ScaleItemFrames[[index]] <- HandleMissing(object$ScaleItemFrames[[index]])
  }
  
  # Here we change the object to a Missing.Psychometric object. But remember
  # that it can still be used as a Psychometric object
  class(object) <- c("PsychometricMissing", "Psychometric")
  return(object)
}

#' @export
getCommand <- function(x, ...) {
  UseMethod("getCommand", x)
}

#' @export
getCommand.Psychometric <- function(object, scale = "All", command = "Alpha")
{
  getAlpha <- function(scale)
  {
    n <- paste(names(object$ScaleItemFrames[[scale]]), collapse = ",")
    res <- list(paste("alpha(object$OriginalData[cs(", n, ")], check.keys = T,keys=NULL,",
                      "cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, ",
                      "n.iter=1,delete=TRUE,use='pairwise',warnings=TRUE,",
                      "n.obs=NULL,impute=NULL)", sep = ""))
    return(res)
    
  }
  
  if (command == "Alpha" && scale == "All")
  {
    res <- NULL
    
    for(data in object$ScaleItemFrames )
    {
      res <- append(res, getAlpha(scale))
    }
    return(res)
  }
  if (command == "Alpha" && scale != "All")
  {
    if (any(scale == object$ScaleNames))
    {
      return(getAlpha(scale))
    }
    
  }
  
}



#' @export
split.Psychometric <- function(object, group, f,...)
{
  splitDataFrames <- split(object$OriginalData, object$OriginalData[group])
  results <- list()
  for (data in splitDataFrames)
  {
    print(paste("Here are results for group variable ", group,
                "category", data$Group[1]))
    psychometric <- GetPsychometric(data, object$ScaleNames,
                                    responseScale = object$ResponseScales,
                                    itemLength = object$ItemLength, 
                                    reverse = F, name = paste("Group ", data$Group[1]))
    results <- append(results, list(f(psychometric, ...)))
  }
  class(results) <- "PsychometricSplit"
  return(results)
  
}

#' @export
print.PsychometricSplit <- function(object)
{
  for(obj in object)
    print(obj)
}

#' @export
bestItems <- function(x, ...) {
  UseMethod("bestItems", x)
}

#' @export
bestItems.Psychometric <- function(object, scale, nItems)
{
  psych::bestScales(cbind(object$ScaleItemFrames[[scale]], object$ScaleFrame[scale]),
             criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary)
}

#' @export
plotScale <- function(x, ...) {
  UseMethod("plotScale", x)
}



#' @export
plotScale.Psychometric <- function(object, scale = "All", group = NULL,
                                   external = NULL,
                                   type = "Histogram", main = "", xlab = "", ...)
{
  if(is.null(group) && is.null(external))
  {
    if (scale != "All")
    {
      
      if (type == "Histogram" || missing(type))
      {
        hist(object$ScaleFrame[[scale]], 
             main = ifelse(missing(main),
                           paste("Histogram of", scale),
                           main),
             xlab = ifelse(missing(xlab), scale, xlab))
      }
      if (type == "Boxplot")
      {
        #        boxplot(object$ScaleFrame[[scale]], 
        #                xlab = ifelse(missing(xlab), scale, xlab))
        ggplot2::ggplot(data = object$ScaleFrame[scale], aes_string(y = scale))+
          ggplot2::boxplot()
      }
    }
  }
  else
  {
    if (scale != "All" && is.null(external))
    {
      if(group %in% names(object$OtherVariables))
        
        if (type == "Boxplot")
        {
          f <- formula(paste(scale, "~", group))
          d <- cbind(object$ScaleFrame[scale], factor(object$OtherVariables[group]))
          
          
          boxplot(f, data = d)
        }
    }
    else
    {
      if (!is.null(external))
      {
        if (type == "Scatter" && scale %in% names(object$ScaleFrame) && 
            external %in% names(object$OtherVariables))
        {
          d <- cbind(object$ScaleFrame[scale], object$OtherVariables[external])
          ggplot2::ggplot(data = d, aes_string(x = scale, y = external)) +
            ggplot2::geom_point(col = "gray")
        }
        
      }
    }
    
  }
  
  
}

#' @export
plot.Psychometric <- function(object, scale = "All")
{
  if (scale == "All")
  {
    
    for(data in object$ScaleItemFrames )
    {
      corr <- cor(data, use = "pairwise.complete.obs");
      
      print(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                       lab = TRUE))
    }
  }
  if (scale != "All")
  {
    if (any(scale == object$ScaleNames))
    {
      data <- object$ScaleItemFrames[[scale]]
      corr <- cor(data, use = "pairwise.complete.obs")
      ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                 lab = TRUE)
    }
    
  }
  
}


#Summary table of mean, sd, n, omega, alpha, skew, kurtosis and range
#' @export
summary.Psychometric<-function(x, mean = T, sd = T, SE = T, skew = T, kurtosis = T,
                               min = T, max = T, omega = T,
                               #alpha = T, 
                               n = T) 
{
  ifelse(x$ScaleFrame[,1]==x$ScaleFrame$ID, y<-x$ScaleFrame[,-1], y<-x) # remove ID column only if it's there
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in seq_along(y))
  {
    if(mean==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(sd==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(SE==TRUE) 
    {sumx$SE[i]<-std.error(y[i],na.rm = TRUE)} # need library(plotrix))
    if(skew==TRUE)
    {sumx$Skew[i]<-skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(kurtosis==TRUE)
    {sumx$Kurtosis[i]<-kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(min==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(max==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(omega==TRUE)
    {omeg<-psych::omega(x$ScaleItemFrames[[i]]) 
    sumx$Omega[i]<-as.vector(omeg$omega.tot)}
    #sumx$Alpha[i]<-omeg$alpha
    #  if(alpha==TRUE)
    #  {sumx$Alpha[i]<-omeg$alpha}
    if(n==TRUE)
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-x$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (kurtosis==TRUE)
  {
    mystars <- ifelse(summaryy$Kurtosis > 1, "*", "") # adding stars to kurtosis and skew value above a certain number
    summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
    p <- ifelse(summaryy$Kurtosis > 1, NA, summaryy$Kurtosis )
    if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
  }
  if (skew==TRUE)
  {
    mystars2 <- ifelse(summaryy$Skew > 1, "*", "")
    summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
    k <- ifelse(summaryy$skew > 1, NA, summaryy$skew )
    if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')
  }
  if(omega==TRUE)
  {
    mystars3 <- ifelse(summaryy$Omega < 0.82, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega > 0.82, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}