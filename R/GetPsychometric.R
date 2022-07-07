

#' Getting the Psychometric class
#'
#' Makes it simple to do basic psychometrics of scales
#' @param data A dataframe with the items and eventual extra variables
#' @param scaleNames A vector with the names of the scales
#' @param responseScale A list with vectors of response intervalls, either one (if all are the same) or one for each scale
#' @param typeSum A functions used for summarizing the items to scales
#' @param itemLength The number of significant characters in items
#' @param reverse A boolean that sets whether items ending with 'R' should be reversed
#' @param idVar The name of the case identifier (if none a case number variable called ID will be created)
#' @param name The name of the object (optional)
#' @param itemDictionary a textfile with information about all scales, items and whether they should be reversed
#' @param itemList items do not conform to above they can be addressed by number instead, new item names will be created based on scale names
#' @param reverseList a vector with numbers for all items that should be reversed, if any
#' @param missings a vector with numbers to be converted to NA, e.g., -99 should be converted to NA
#' @return A Psychometric object that can be used for analyses
#' @export
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'  responseScale = list(c(0,4)), itemLength = 4)
#'
GetPsychometric <- function(data, scaleNames, responseScale = list(c(1,5)),
                            typeSum = "Mean", itemLength = 6,  #item skall ha samma namn som skalan plus tecken
                            reverse = T, idVar = "ID", name = "Psychometric",
                            itemDictionary = NULL, itemList=NULL, reverseList=c(),
                            missings = NULL)
{
  # if there is a variable called "ID" this variable is added to all the data
  # frame
  IDVar <- NULL
  if (!is.null(data[[idVar]]))
  {
    IDVar <- data[[idVar]]
    data <- dplyr::select(data, -idVar)
  }
  else
  {
    IDVar <- as.data.frame(1:nrow(data))

  }
  if (FALSE %in% sapply(scaleNames, FUN = function(x) return(nchar(x) >= itemLength)))
  {
    print(paste("Error: itemLength = ", itemLength, "is larger than the string length of the shortes scale name"))
    return()
  }
  MakeMissing <- function()
  {
    if (is.null(missings))
      return()
    for (mis in missings)
    {
      data <- naniar::replace_with_na_all(data, condition = ~.x == mis)

    }
  }
  CreateItemNames <- function()
  {
    index <- 1
    for (scale in scaleNames)
    {
      for (item in itemList[[index]])
      {
        if (!is.character(item))
        {
          if (item %in% reverseList)
            names(data)[item] <- paste(substr(scale, 1, itemLength),
                                       names(data[item]), "R", sep = "")
          else
            names(data)[item] <- paste(substr(scale, 1, itemLength),
                                       names(data[item]), sep = "")

        }
        else
        {
          if (item %in% reverseList)
          {
            cn <- which( colnames(data)==item )
            names(data)[cn] <- paste(substr(scale, 1, itemLength),
                                     item, "R", sep = "")
          }
          else
          {
            cn <- which( colnames(data)==item )

            names(data)[cn] <- paste(substr(scale, 1, itemLength),
                                     item, sep = "")
          }

        }
      }
      index <- index + 1

    }
    return(data)
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
    res <- NULL
    if (!is.null(IDVar))
    {
      res <- IDVar

    }
    else
    {
      res <- 1:nrow(data)
    }
    res <- as.data.frame(res)
    names(res) <- "ID"

    scaleNamesItemLength <- unlist(sapply(scaleNames, FUN = function(x)
      return(substr(x, 1, itemLength))), use.names = F)
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

    if (nchar(scales) > itemLength)
    {
      newNames <- NULL
      for (item in 1:ncol(x))
      {
        c <- ""
        iName <- names(x[item])
        for (s in itemLength+1:nchar(scales))
        {

          if (substring(scales, s, s) == substring(iName, s,s))
          {
            c <- paste(c, substring(scales, s, s), sep = "")
          }
          else
            break
        }
        if (c != "")
          newNames <-c(newNames, gsub(c, '', names(x[item])))
        else
          newNames <- c(newNames, names(x[item]))
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
      if (nchar(v) > itemLength)
      {
        if (substr(v, 1, itemLength) %in% signPart)
        {
          iName <- scaleNames[match(substr(v, 1, itemLength),signPart)]
          if (nchar(iName) > itemLength)
          {
            c <- ""
            for (s in itemLength+1:nchar(v))
            {

              if (substring(v, s, s) == substring(iName, s,s))
              {
                c <- paste(c, substring(v, s, s), sep = "")
              }
              else
                break
            }
            if (c != "")
              names(data)[names(data) == v] <- gsub(c, '', v)
            else
              names(data)[names(data) == v] <- v

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
    res <- NULL
    for (index in 1:length(frames))
    {
      res <- cbind(res, rowMeans(as.data.frame(frames[index]), na.rm = F))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    names(res) <- nameV
    return(res)


  }
  GetDictionary <- function()
  {
    d <- NULL
    if (file.exists(itemDictionary))
      d <- utils::read.delim(itemDictionary, comment.char="#")
    else
    {
      print("Dictionary file does not exist")
      return(NULL)
    }
    rowNames <- NULL
    for(index in 1:nrow(d))
      rowNames <- c(rowNames, paste(substr(d[index, 4],1,itemLength), d[index,1], sep = ""))
    return(data.frame(cbind(d[2], d[3], d[4], row.names = rowNames)))
  }
  if (!is.null(itemList))
    data <- CreateItemNames()
  otherVariables <- GetNonItemVar()
  responseScale <- expandResponsScale(responseScale, scaleNames)
  scaleItemFrames <- GetScaleItemFrames(data, responseScale)
  scaleFrames <- GetScalesFrame(scaleItemFrames, scaleNames)
  data <- ChangeOriginalDataNames(data)
  names(scaleItemFrames) <- scaleNames
  if (!is.null(itemDictionary))
    itemDictionary <- GetDictionary()
  else
    itemDictionary <- NULL;
  MyObject <- list(ResponseScales = responseScale, ScaleItemFrames = scaleItemFrames, ScaleFrame = scaleFrames,
                   ScaleNames = scaleNames, OtherVariables = otherVariables, OriginalData = data,
                   Name = name, ItemDictionary = itemDictionary,
                   ItemLength = itemLength, ResultList = list(), PrintRes = list(), RCommands = list())

  class(MyObject) <- "Psychometric"
  return(MyObject)

}


#' summary for Psychometric
#'
#' @param object Psychometric object
#' @param ... There are several extra arguments to delete from the output
#'  mean: should the mean be displayed T/F
#'  sd:  should the sd be displayed T/F
#'  SE:  should the SE be displayed T/F
#'  skew:  should the skew be displayed T/F
#'  kurtosis:  should the kurtosis be displayed T/F
#'  min:  should the min be displayed T/F
#'  max:  should the max be displayed T/F
#'  alpha:  should the alpha be displayed T/F. Warnings at Level
#'  n:  should the n be displayed T/F
#'  plots:  should the plots be displayed T/F
#'
#' @return Nothing
#'
#' @examples
#' object <- GetPsychometric(persData, c("Achievement", "Dutifulness", "Orderly"),
#'    responseScale = list(c(0,4)), itemLength = 4)
#' summary(object)
#' @export
summary.Psychometric<-function(object, ...)
{
  GetExtraArgument <- function(a)
  {
    arg <- list(...)
    if (a %in% names(arg))
      return(arg[[a]])
    else
      return(T)

  }

  mean <- GetExtraArgument("mean")
  sd <- GetExtraArgument("sd")
  SE <- GetExtraArgument("SE")
  skew <- GetExtraArgument("skew")
  kurtosis <- GetExtraArgument("kurtosis")
  min <- GetExtraArgument("min")
  max <- GetExtraArgument("max")
  alpha <- GetExtraArgument("alpha")
  n <- GetExtraArgument("n")
  plots <- GetExtraArgument("plots")

  y <- object$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(mean==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(sd==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(SE==TRUE)
    {sumx$SE[i]<-sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(skew==TRUE)
    {sumx$Skew[i]<-psych::skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(kurtosis==TRUE)
    {sumx$Kurtosis[i]<-psych::kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(min==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(max==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(alpha==TRUE)
    {myalpha<-psych::alpha(object$ScaleItemFrames[[i]])
    sumx$Alpha[i]<-as.vector(myalpha$total$raw_alpha)}
    if(n==TRUE)
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-object$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (kurtosis==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars <- ifelse(summaryy$Kurtosis > 2, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 2, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }
    else
    {
      mystars <- ifelse(summaryy$Kurtosis > 4, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 4, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }

  }
  if (skew==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars2 <- ifelse(summaryy$Skew > 1, "*", "")
      summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
      k <- ifelse(summaryy$skew > 1, NA, summaryy$skew )
      if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')
    }
    else
      mystars2 <- ifelse(summaryy$Skew > 2, "*", "")
    summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
    k <- ifelse(summaryy$skew > 2, NA, summaryy$skew )
    if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')

  }
  if(alpha==TRUE)
  {
    mystars3 <- ifelse(summaryy$Alpha < 0.70, "*", "")
    summaryy$Alpha<-paste(summaryy$Alpha, mystars3, sep=" ")
    O <- ifelse(summaryy$Alpha > 0.75, NA, summaryy$Alpha )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}

