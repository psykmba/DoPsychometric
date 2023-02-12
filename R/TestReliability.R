library(readr)
library(naniar)
library(psych)
library(CCA)
library(CCP)
devtools::install_github("psykmba/DoPsychometric", force = T)
library(DoPsychometric)
library(haven)
NB5I_1_1_20_22212 <- read.csv("~/Dropbox/Forskning/SPSS filer/Personlighetstest/NB5I/NB5I_1.1_202212.txt")
View(NB5I_1_1_20_22212)
NB5I_1_1_20_22212 <- NB5I_1_1_20_22212 %>%
  replace_with_na_all(condition = ~.x == -99)
NB5I_1_1_20_22212 <- NB5I_1_1_20_22212[complete.cases(NB5I_1_1_20_22212[,2:116]),]

TestObject1 <- GetPsychometric(NB5I_1_1_20_22212,
                              scaleNames = psych::cs(Extra, Agree, Conscient),
                              itemLength = 4,
                              responseScale = list(c(0,4)),
                              itemList = list(c(2:4, 24:26, 45:47, 87:89),
                                              c(8:10, 51:53, 81:83, 93:95),
                                              c(20:21, 63:65, 69:71, 75:77)),
                              missing = -99)


TestObject2 <- GetPsychometric(NB5I_1_1_20_22212,
                              scaleNames = psych::cs(Extra, Agree, Conscient),
                              itemLength = 4,
                              responseScale = list(c(0,4)),
                              itemList = list(c(5:7, 27:28, 48:50, 90:92),
                                              c(11:13, 54:56, 84:86, 96:98),
                                              c(22:23, 66:68, 72:74, 78:80)),
                              missing = -99)


# This function test how well MFC items reproduce the traits of a personality
# inventory based on single-items
# object = A Psychometric object created by the GetPsychometric function
# itemNum = number of item sets in the MFC inventory
# invert = if T then mixed items are used, if F only positive items

GetReliabilityMFC3 <- function(object, itemNum, testObject = NULL, invert = T) {
  # to gather result in a vector
  matrixRes <- c()
  # replications are 100
  for (index in 1:10) {
    # create the inventory and retrun the binary outcomes
    dataInvert <- CreateIpsative3(object,itemNum, invert = invert, tri = NULL)
    # get the scales from the personality inventory
    if (is.null(testObject))
       pdataInvert <- as.data.frame(getData(object, otherVar = F,
                                            itemFrames = F, scaleFrame = T))
    else
      pdataInvert <- as.data.frame(getData(testObject, otherVar = F,
                                           itemFrames = F, scaleFrame = T))

    # sum the binary outcomes to scales
    if (is.null(invert))
      mRes <- as.data.frame(CreateTraitFromTriplar(dataInvert$dataFram,
                                                 length(object$ScaleFrame),
                                                 dataInvert$Triplar,dataInvert$Reverse))
    else
      mRes <- as.data.frame(CreateTraitFromTriplar(dataInvert$dataFram,
                                                   length(object$ScaleFrame),
                                                   dataInvert$Triplar))

    # gather the mean of the diagonal having the trait correlations
    matrixRes <- c(matrixRes, diag(cor(mRes, as.data.frame(pdataInvert))))

  }
  # return the mean of means of all replications
  return(mean(matrixRes))
}


# This uses the GetReliabilityMFC3 with items from 2 to 30 (every second)
# This is for mixed items
mRes <- c()
mInd <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  mInd <- c(mInd, index)
  mRes <- c(mRes, mean(GetReliabilityMFC3(TestObject, index, invert = T)))
}
plot(mInd, mRes)

# This is for non-mixed items
fRes <- c()
fInd <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  fInd <- c(fInd, index)
  fRes <- c(fRes, mean(GetReliabilityMFC3(TestObject, index, invert = F)))
}
plot(fInd, fRes)


# This is a function that randomly select numItems from the scales of the
# inventory.
GetRandomScaleItems <- function(object, numItems = 1)
{
  # to gather results
  sampleRes <- NULL
  itemFrames <- NULL
  # Create new item frames, one for each factor scale
  for (j in 1:length(object$ScaleFrame))
  {
    itemFrames <- append(itemFrames, list(data.frame(row.names = 1:nrow(object$ScaleFrame))))
  }
  # Sample the single-items from teh dataframe
  for(i in 1:numItems)
  {
    for (j in 1:length(object$ScaleFrame))
    {
      frame <-  itemFrames[[j]]
      r <- sample(1:ncol(object$ScaleItemFrames[[j]]),1)
      itemFrames[[j]] <- cbind(frame, object$ScaleItemFrames[[j]][r])
    }
  }
  newFrame <- data.frame(row.names = 1:nrow(object$ScaleFrame))
  # bind together the frames
  for(i in 1:length(object$ScaleFrame))
    newFrame <- cbind(newFrame, itemFrames[[i]])
  # sets unique names for all selected items, there is a chance that an item
  # is selected twice.
 names(newFrame) <- make.unique(names(newFrame))
  return(as.data.frame(newFrame))
}


# This function test how well a few single-items reproduce the traits of a personality
# inventory based on single-items
# object = A Psychometric object created by the GetPsychometric function
# itemNum = number of item sets in the MFC inventory

GetReliabilitySingle3 <- function(object,testObject = NULL, itemNum)
{
  matrixRes <- c()
  for (index in 1:100)
  {
      myFrame <- GetRandomScaleItems(object,numItems = itemNum)
    # This is used to create the scales based on the selected items in myFrame
    # It can handle ay number of items
        test <- GetPsychometric(myFrame, scaleNames = c("Extr", "Agree", "Cons"),
                            itemLength = 4,
                            itemList = list(c(1:itemNum),c((itemNum+1):(itemNum*2)), c((itemNum*2+1):(itemNum*3))),
                            responseScale = list(c(0,4)))
    # This extract the scale values,
        testData1 <-  as.data.frame(getData(test, otherVar = F,
                                           itemFrames = F, scaleFrame = T))
        testData2 <-  as.data.frame(getData(testObject, otherVar = F,
                                           itemFrames = F, scaleFrame = T))
        # Gather the mean of the diagonal that have the trait correlations
        if (is.null(testObject))
          matrixRes <- c(matrixRes, diag(cor(testData1, as.data.frame(pdataInvert))))
        else
          matrixRes <- c(matrixRes, diag(cor(testData1, testData2)))

  }
  return(matrixRes)
}

# This uses the GetReliabilityMFC3 with items from 2 to 30 (every second)
# This is for mixed items

sRes <- c()
sInd <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  sInd <- c(sInd, index)
  sRes <- c(sRes, mean(GetReliabilitySingle3(TestObject, index)))
}
plot(sInd, sRes)



# Creates data for plot
plotData <- as.data.frame(list(numItem = c(mind, sInd, fInd), cor = c(mRes, sRes, fRes),
                               group = as.character(c(rep.int(1,15), rep.int(2,15),rep.int(3,15)))))


library(ggplot2)


plotData$group <- ifelse(plotData$group == '1',
                         "MFCMix", ifelse(plotData$group == '2',"Single", "MFC"))
ggplot(data = plotData, aes(x = numItem, y = cor, group = factor(group))) +
  geom_point() +
  geom_line(aes(linetype=group, color=group))


# Test with replication test

TestObject1 <- GetPsychometric(NB5I_1_1_20_22212,
                               scaleNames = psych::cs(Extra, Agree, Conscient),
                               itemLength = 4,
                               responseScale = list(c(0,4)),
                               itemList = list(c(2:4, 24:26, 45:47, 87:89),
                                               c(8:10, 51:53, 81:83, 93:95),
                                               c(20:21, 63:65, 69:71, 75:77)),
                               missing = -99)


TestObject2 <- GetPsychometric(NB5I_1_1_20_22212,
                               scaleNames = psych::cs(Extra, Agree, Conscient),
                               itemLength = 4,
                               responseScale = list(c(0,4)),
                               itemList = list(c(5:7, 27:28, 48:50, 90:92),
                                               c(11:13, 54:56, 84:86, 96:98),
                                               c(22:23, 66:68, 72:74, 78:80)),
                               missing = -99)


mResR <- c()
mIndR <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  mIndR <- c(mIndR, index)
  mResR <- c(mResR, mean(GetReliabilityMFC3(TestObject1, TestObject2, index, T)))
}
plot(mIndR, mResR)

# This is for non-mixed items
fResR <- c()
fIndR <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  fIndR <- c(fIndR, index)
  fResR <- c(fResR, mean(GetReliabilityMFC3(TestObject1, TestObject2, index, F)))
}
plot(fIndR, fResR)



# This uses the GetReliabilityMFC3 with items from 2 to 30 (every second)
# This is for mixed items

sResR <- c()
sIndR <- c()
for (index in seq(from = 2, to = 30, by = 2))
{
  sIndR <- c(sIndR, index)
  sResR <- c(sResR, mean(GetReliabilitySingle3(TestObject1,TestObject2, index)))
}
plot(sIndR, sResR)



# Creates data for plot
plotData <- as.data.frame(list(numItem = c(mindR, sIndR, fIndR), cor = c(mResR, sResR, fResR),
                               group = as.character(c(rep.int(1,15), rep.int(2,15),rep.int(3,15)))))


library(ggplot2)


plotData$group <- ifelse(plotData$group == '1',
                         "MFCMix", ifelse(plotData$group == '2',"Single", "MFC"))
ggplot(data = plotData, aes(x = numItem, y = cor, group = factor(group))) +
  geom_point() +
  geom_line(aes(linetype=group, color=group))


