
#' Getting the Psychometric class
#'
#' Makes it simple to do basic psychometrics of scales
#' @param data A dataframe with the items and eventual extra variables
#' @param scaleNames A vector with the names of the scales, e.g., scaleNames = c("Extravertion", "Agreeableness", "Conscientiousnsess", "Neuroticism", "Openness")
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
#' @details The GetPsychometric functions create a class that can be used to simplify
#' psychometric evaluation of a scale or an inventory. The function takes a dataframe
#' with items that are automatically aggregated to scales based on one of two different
#' methods. When you have the Psychometric object you can do a number of
#' psychometric analyses, and when you are satisfied you can save the file and
#' use the scales (and items and other variables) in R or another program.
#'
#' 1. Either you name the item columns in a consistent way based on the scale names
#' you like to have, e.g., all items from the extraversion scale starts with 'extr'
#' 2. You send an list with vectors that have the column numbers of the items for
#' each scale
#'
#' scaleNames
#' A vector with the names of the scales, e.g., scaleNames = c("Extravertion",
#'                                                             "Agreeableness", "Conscientiousnsess", "Neuroticism", "Openness")
#'
#' responseScale
#' A list with vectors of response intervals, either one (if all are the same) or
#' one interval for each scale, e.g., responseScale = list(c(1,5)), when all items
#' have a response scale from 1 to 5 (Likert style). Alternatively, when we have
#' five scales, responseScale = list(c(1,5),c(1,7), c(1,4), c(1,5), c(1,7)). The
#' first and forth scale have response scales from 1 to 5, the second and fifth, from 1
#' to 7, and the third from 1 to 5
#'
#' typeSum
#' A functions used for summarizing the items to scales, the default is Mean
#'
#' itemLength
#' The number of significant characters in the item names. Scale name and item names
#' should start with the same "significant" number of characters, e.g., scale name=
#'   "Extravert" and item names are "ExtrItem1", "ExtrItem2"....</br>
#'   In this case itemLength = 4. But it can be any number of characters, e.g., this
#' would be itemLength = 1, scale name = "A" with item names "AFirstItem", "ASecondItem",
#' "AThirdItem",.....
#'
#' reverse
#' A boolean that sets whether items ending with 'R' should be reversed
#'
#' idVar
#' The name of the case identifier (if none a case number variable called ID will be created)
#'
#' name
#' The name of the object (optional)
#'
#' itemDictionary
#' a textfile with information about all scales, items and whether they should be reversed
#'
#' itemList
#' this is an alternative to have scale name and item names conforming to the definition
#' under itemLength. Instead you can have item scale(s) name(s) and then define the
#' the scales by the item's place in the dataframe, e.g., scaleNames = c("E", "A")
#' and itemList = list(c(1,2,3,4,5), c(6,7,8,9,10)). The items for the "E" scale
#' are in the first five columns of the dataframe, and the item of the "A" scale are
#' in the columns 6 to 10.
#'
#' reverseList
#' This is an alternative way of defining which items that should be reversed that
#' goes together with the 'itemList' argument. It should be
#' a vector with numbers for all columns having an item that should be reversed.
#'
#'
#'
#' You always always provide a vector of scale names, e.g., c("Achievement", "Dutifulness", "Orderly"),
#' to the argument scaleNames and you also provide a itemLength argument, that define the number of characters
#' that define you items, e.g., itemLength = 4 suggesting that the first four characters
#' defines the items belonging to the scale. If an item should be reversed you should
#' add an 'R' to the end of the name and also set the reverse flag to TRUE
#' When you are Using alternative 1 above and has Achievement as scale name, the items should
#' start with column name "Achi", like "Achive10" or "Achive12".
#' Using alternative 2 above, you add an itemList with column numbers like this:
#' list(c(4,5,6,7), c(8,9,10,11), c(12,13,14,15)) representing three scales. Item than
#' need to be reversed should be defined in a vector and sent to reverseList
#' If there is an identity variable you can define it with the idVar argument.
#' If there are missing values use define a vector with column numbers
#' and send it with the missings argument.
#'
#' The object include a number of useful dataframes:
#' 1. A dataframe called 'ScaleFrame' that include the value of the scale
#' 2. A list of dataframes calles 'ScaleItemFrames' that include the items included
#' in each of the scales.
#' 3. All scale names in the 'ScaleNames' vector
#' 4. Original data are in the 'OriginalData' frame
#' 5. The variables not included in the scales are in the "OtherVariables" dataframe
#' together with the ID variable
#' In addition there are other objects that are used together with some of the
#' methods/functions that can be called with the object
#'
#' @return A Psychometric object that can be used for analyses.
#' @examples
#' dataObject <- GetPsychometric(persData,
#' scaleNames = c("Extraversion", "Agreeableness",
#'                "Conscientiousness", "Neuroticism",
#'                "Openness"),
#' responseScale = list(c(0,36)),
#' itemLength = 4)
#'
#' dataObject <- GetPsychometric(persData,
#'                 scaleNames = c("Extraversion", "Agreeableness",
#'                               "Conscientiousness", "Neuroticism",
#'                              "Openness"),
#'                 responseScale = list(c(0,36)),
#'                 itemList = list(c(3,9,11,19), c(7,17,18, 21),
#'                 c(5, 12,13), c(4,8,14,15), c(6,10, 16,20)),
#'                 itemLength = 4)
#'
#' @export
GetPsychometric <- function(data, scaleNames, responseScale = list(c(1,5)),
                            typeSum = "Mean", itemLength = 6,  #item skall ha samma namn som skalan plus tecken
                            reverse = T, idVar = "ID", name = "Psychometric",
                            itemDictionary = NULL, itemList=NULL, reverseList=c(),
                            missings = NULL)
{
  # if there is a variable called "ID" this variable is added to all the data
  # frame

  if (FALSE %in% sapply(scaleNames, FUN = function(x) return(nchar(x) >= itemLength)))
  {
    print(paste("Error: itemLength = ", itemLength, "is larger than the string length of the shortes scale name"))
    return()
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
            names(data)[item] <- paste(substr(scale, 1, itemLength),"_",
                                       names(data[item]), "R", sep = "")
          else
            names(data)[item] <- paste(substr(scale, 1, itemLength),"_",
                                       names(data[item]), sep = "")

        }
        else
        {
          if (item %in% reverseList)
          {
            cn <- which( colnames(data)==item )
            names(data)[cn] <- paste(substr(scale, 1, itemLength),"_",
                                     item, "R", sep = "")
          }
          else
          {
            cn <- which( colnames(data)==item )

            names(data)[cn] <- paste(substr(scale, 1, itemLength),"_",
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
  getSignItemName <- function(x, scales , itemLength)
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
    for(index in 1:length(data))
    {
      n <- names(data[index])
       if (itemLength < stringr::str_length(n)-3)
      {
        signPart <- strtrim(n, itemLength)
        l <- stringr::str_length(n)
        names(data)[index] <-
          paste(signPart, stringr::str_sub(n, l-3, l), sep = "")
      }

    }
    return(data)
  }
  GetColNames <- function(dFrame)
  {
    return(paste(grep(stringr::str_sub(dFrame, 1,itemLength), names(data), value = T), collapse = ","))
    # for (name in names(dFrame))
    # {
    #   res <- paste(res, '"', name, '",', sep = "")
    # }
    # return(substr(res, 1, stringr::str_length(res)-1))
  }

  GetScaleItemFrames <- function(d, responses)
  {

    GetData <- function(name)
    {
       frame <- dplyr::select(d, dplyr::starts_with(substr(name, 1, itemLength)))
       frame2 <- as.data.frame(sapply(frame, FUN = function(x) ifelse(x %in% missings, NA, x)))
       names(frame2) <- names(frame)
      return(frame2)
    }
    Reverse <- function(col, resp,oldname)
    {
      if (reverse == T | reverse == "Find")
      {
        RCommands <<- append(RCommands, paste("Data$",col, " <- (", resp[1], "+", resp[2], ") - ", "Data$",oldname,"\n", sep = ""))
        return((resp[1]+resp[2]) - col)
      }
      else
        return(col)
    }
    GetReverse <- function(frame, resp, oldNames)
    {
       if (reverse == "Find")
      {
         comp <- psych::pca(frame, nfactors = 1)
         load <- as.vector(comp$loadings)
         for(index in 1:length(frame))
         {
           name <- names(frame[index])
            frame[index] <- ifelse(load[index] < 0,
                                  Reverse(frame[index], resp[[1]], oldNames[index]))
         }
          return(frame)
      }
      else {
      for(index in 1:length(frame))
      {
        name <- names(frame[index])

        frame[index] <- ifelse(substr(name, nchar(name), nchar(name)) %in% c("r", "R"),
                               Reverse(frame[index], resp[[1]], oldNames[index]),frame[index])
      }
      }
      return(frame)
    }
    resFrames <- NULL
    for(index in 1:length(scaleNames))
    {
       interm <- GetData(scaleNames[index])
       oldNames <- names(interm)
       # interm <- ChangeOriginalDataNames(interm)
       # interm <- getSignItemName(interm,scaleNames[index], itemLength )
      resFrames <- append(resFrames, list(as.data.frame(GetReverse(interm, responses[index], oldNames))))
     }
    names(resFrames) <- scaleNames
    return(resFrames)
  }


  GetScalesFrame <- function(frames, nameV)
  {
    res <- NULL
     for (index in 1:length(frames))
    {

      res <- cbind(res, rowMeans(as.data.frame(frames[[index]]), na.rm = T))
        RCommands <<- append(RCommands, list(paste("Data$",nameV[index], "<- rowMeans(Data[psych::cs(", GetColNames(nameV[index]), ")],na.rm = TRUE)\n", sep = "")))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    names(res) <- nameV
    return(res)


  }
  GetDictionary <- function()
  {
    GetName <- function(n)
    {
     if (itemLength < stringr::str_length(n)-3)
      {
        signPart <- strtrim(n, itemLength)
        l <- stringr::str_length(n)
        return(paste(signPart, stringr::str_sub(n, l-3, l), sep = ""))
      }

    }
    d <- hash::hash()
    data <- NULL
    if (file.exists(itemDictionary))
      data <- utils::read.delim(itemDictionary, sep = "#")
    else
    {
      print("Dictionary file does not exist")
      return(NULL)
    }
    for(index in 1:nrow(data))
    {
      d[[data[index,1]]] <- paste(data[index, 2], data[index,3], sep = " | ")

    }

    return(d)
  }
  GetExplainText <- function(commands)
  {
    return(append(RCommands, list("To use the commands change Data in the script above to the name of your dataframe")))
  }
  if (!is.null(itemList))
  {
    data <- CreateItemNames()
   }
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
  RCommands <- list()
  otherVariables <- GetNonItemVar()
  responseScale <- expandResponsScale(responseScale, scaleNames)
  scaleItemFrames <- GetScaleItemFrames(data, responseScale)
   scaleFrames <- GetScalesFrame(scaleItemFrames, scaleNames)
  # if (is.null(itemList))
  #    data <- ChangeOriginalDataNames(data)
  names(scaleItemFrames) <- scaleNames
  if (!is.null(itemDictionary))
    itemDictionary <- GetDictionary()
  else
    itemDictionary <- NULL;
  MyObject <- list(ResponseScales = responseScale, ScaleItemFrames = scaleItemFrames, ScaleFrame = scaleFrames,
                   ScaleNames = scaleNames, OtherVariables = otherVariables, OriginalData = data,
                   Name = name, ItemDictionary = itemDictionary,
                   ItemLength = itemLength, ResultList = list(), PrintRes = list(),
                   RCommands = GetExplainText(RCommands))

  class(MyObject) <- "Psychometric"
  return(MyObject)

}

