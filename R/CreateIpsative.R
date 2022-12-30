
CreateIpsative3 <- function(object, numTriplar, invert = F, tri = NULL,
                            useRandom = NULL)
{
  MakeTriplar <- function()
  {
    res <- list()
    GetOrder <- function(x,y,z)
    {
      ord <- c(x,y,z)
      s <- sample(1:3, 3)
      return(c(ord[s[1]],ord[s[2]],ord[s[3]]))
    }
    for (index in 1:numTriplar)
    {
      for (scalesX in 1:(length(object$ScaleFrame)-2))
      {
        for(scalesY in (scalesX+1):(length(object$ScaleFrame)-1))
        {
          for (scalesZ in (scalesY+1):length(object$ScaleFrame))
          {
 #           print(c(scalesX, scalesY, scalesZ))
            res <- append(res, list(GetOrder(scalesX, scalesY, scalesZ)))
          }
        }
      }
    }
    return(res)
  }
  MakeTriplarSys <- function()
  {
    GetOrder <- function(x,y,z,triples)
    {
      getEx <- function(rt, ct,n)
      {
        m <- matrix(nrow = 3, ncol = 3)
        for (i in 1:3)
          for (j in 1:3)
            m[i,j] <- (rt[i]*ct[j]) / n
        return (m)
      }
       if (is.null(triples)  )
      {
        ord <- c(x,y,z)
        s <- sample(1:3, 3)
        return(c(ord[s[1]],ord[s[2]],ord[s[3]]))

      }
      ord <- c(x,y,z)
      s <- sample(1:3, 3)
      ord <- c(ord[s[1]],ord[s[2]],ord[s[3]])
      mat <- matrix(unlist(triples), ncol = 3, byrow = T)
      mat[!mat %in% ord] <- 0
      expMat <- matrix(c(sum(mat[,1] == ord[1]),
                         sum(mat[,2] == ord[1]),
                         sum(mat[,3] == ord[1]),
                         sum(mat[,1] == ord[2]),
                         sum(mat[,2] == ord[2]),
                         sum(mat[,3] == ord[2]),
                         sum(mat[,1] == ord[3]),
                         sum(mat[,2] == ord[3]),
                         sum(mat[,3] == ord[3])), ncol = 3)
      expMat[which(is.na(expMat))] <- 0
      expMat <- t(expMat)
      # Calculate the row and column totals
      row_totals <- rowSums(expMat)
      col_totals <- colSums(expMat)

      # Calculate the expected values manually
      expected <- getEx(row_totals, col_totals, sum(expMat))
    expMatDiff <- expMat - expected
#    browser()
    minTab <- which(expMatDiff == min(expMatDiff), arr.ind = TRUE)
    min1 <- ord[minTab[1,1]]
    minpl1 <- minTab[1,2]
#    minTab <- which(expMatDiff == min(expMatDiff[, -min1]), arr.ind = TRUE)
    expMatDiff[,minpl1] <- 99
    expMatDiff[minTab[1,1],] <- 99
    minTab <- which(expMatDiff == min(expMatDiff), arr.ind = TRUE)
    min2 <- ord[minTab[1,1]]
    minpl2 <- minTab[1,2]
    expMatDiff[,minpl2] <- 99
    expMatDiff[minTab[1,1],] <- 99
     minTab <- which(expMatDiff == min(expMatDiff), arr.ind = TRUE)
    min3 <- ord[minTab[1,1]]
    minpl3 <- minTab[1,2]
    res <- c(0,0,0)
    res[minpl1] <- min1
    res[minpl2] <- min2
    res[minpl3] <- min3










      return(res)
    }

    res <- NULL

    for (index in 1:numTriplar)
    {
      for (scalesX in 1:(length(object$ScaleFrame)-2))
      {
        for(scalesY in (scalesX+1):(length(object$ScaleFrame)-1))
        {
          for (scalesZ in (scalesY+1):length(object$ScaleFrame))
          {
            #           print(c(scalesX, scalesY, scalesZ))
            res <- append(res, list(GetOrder(scalesX, scalesY, scalesZ, res)))
          }
        }
      }
    }
    return(res)

  }
  RecodeRows <- function(r)
  {
    rm <- matrix(r, ncol = 3)
    for (index in 1:nrow(rm))
    {
      tillf <- rm[index, 1]
       rm[index,1] <- rm[index, 3]
       rm[index, 3] <- tillf
    }
    return(as.vector(rm))
  }
  triplarDataFrame <- data.frame(row.names = 1:nrow(object$ScaleFrame))
  triplarDataFrame2 <- data.frame(row.names = 1:nrow(object$ScaleFrame))
  GetTrip <- function(v1,v2, v3)
  {
    if (v1>v2 && v1>v3 && v2>v3)
      return(c(1,1,1))
      else
        if(v1>v2 && v1>v3 && v2 <= v3)
          return(c(1,1,0))
    else
      if (v1<=v2 && v1>v3 && v2 > v3)
        return(c(0,1,1))
    else
      if (v1<=v2 && v1<=v3 && v2 <= v3)
        return(c(0,0,0))
    else if (v1 <= v2 && v1 <= v3 && v2 > v3)
      return(c(0,0,1))
    else if(v1 > v2 && v1 <= v3 && v2 <= v3)
      return(c(1,0,0))

  }
  if (is.null(tri))
    triplar <- MakeTriplarSys()
  else
    triplar <- tri
  if (is.null(rev))
    reverse <- list()
  reverse <- list()
  sampleRes <- list()
  rev <-c()
  for(y in 1:length(triplar))
    {
      row <- NULL
      row2 <- NULL
      i <- triplar[[y]][1]
      j <- triplar[[y]][2]
      k <- triplar[[y]][3]
      if (is.null(useRandom))
      {
      y1 <- sample(1:ncol(object$ScaleItemFrames[[i]]),1)
      y2 <- sample(1:ncol(object$ScaleItemFrames[[j]]),1)
      y3 <- sample(1:ncol(object$ScaleItemFrames[[k]]),1)
      sampleRes <- append(sampleRes, list(c(y1,y2,y3)))
      }
       else
          {
        y1 <- useRandom[[y]][1]
      y2 <- useRandom[[y]][2]
      y3 <- useRandom[[y]][3]
      sampleRes <- append(sampleRes, list(c(y1,y2,y3)))
      }
 #     browser()
      if (y1 > ncol(object$ScaleItemFrames[[i]]) / 2)
      {
        rev <- y
      }
      else
        rev <- 0
      if (y2 > ncol(object$ScaleItemFrames[[j]]) / 2)
      {
        rev <- c(rev, y)
      }
      else
        rev <- c(rev, 0)
      if (y3 > ncol(object$ScaleItemFrames[[k]]) / 2)
      {
        rev <- c(rev, y)
      }
      else
        rev <- c(rev, 0)
      #      print(c(v1,v2,v3))

      for (x in 1:nrow(object$ScaleFrame))
      {
         # print(c(names(object$ScaleItemFrames[[i]][y1]),
        #         names(object$ScaleItemFrames[[j]][y2]),
        #         names(object$ScaleItemFrames[[k]][y3])))
        # print(c(x, y1, y2, y3))
         v1 <- object$ScaleItemFrames[[i]][x, y1]
        v2 <- object$ScaleItemFrames[[j]][x, y2]
        v3 <- object$ScaleItemFrames[[k]][x, y3]
        # if (sum(is.na(c(v1,v2, v3))) >0)
        # {
        #   browser()
        # }
        if (isTRUE(invert)) {

          if (y1 > ncol(object$ScaleItemFrames[[i]]) / 2)
          {
            v1 <- 4 - v1
          }
          if (y2 > ncol(object$ScaleItemFrames[[j]]) / 2)
          {
            v2 <- 4 - v2
          }
          if (y3 > ncol(object$ScaleItemFrames[[k]]) / 2)
          {
            v3 <- 4 - v3
          }
        }
        #      print(c(v1,v2,v3))
        v1 <- v1 + rnorm(1, 0, 0.1)
        v2 <- v2 + rnorm(1, 0, 0.1)
        v3 <- v3 + rnorm(1, 0, 0.1)
        #      print(c(v1,v2,v3))

        row <- rbind(row, GetTrip(v1, v2, v3))


        # print(row)
        # scan()

      }
#    browser()

      reverse <- append(reverse, list(rev))
      triplarDataFrame <- cbind(triplarDataFrame, row)
    }

#

  return(list(dataFram = as.data.frame(triplarDataFrame),Triplar = triplar,
              Reverse = reverse, SampleRes = sampleRes))

}

# Loadings
# i1i3*1  (L3_n)
# i2i3*1  (L3_n)
# i4i5*-1  (L5_n)

# Uniqueness
# i1i2*2 (e1e2);
# i1i3*2 (e1e3);
# i2i3*2 (e2e3);

# declare correlated uniqunesses and set their starting values
# i1i2 WITH i1i3*1 (e1);
# i1i2 WITH i2i3*-1 (e2_n);
# i1i3 WITH i2i3*1 (e3);

#  factor loadings relating to the same item are equal in absolute value
# L2_n = -L2;
# L5_n = -L5;
# L8_n = -L8;

#   pair's uniqueness is equal to sum of 2 utility uniqunesses
# e1e2 = e1 - e2_n;
# e1e3 = e1 + e3;
# e2e3 = -e2_n + e3;
#
# ! fix one uniqueness per block for identification
# e1=1;


CreateModelCommand <- function(triplar, reverse)
{
  loadings <- list()
  uniqueness <- list()
  corUniqunesses <- list()
  loadingsEqual <- list()
  pairUniqueness <- list()
  fixUniqueness <- list()
  varIndex <- 1
  for (index in 1:length(triplar))
  {
    tri <- triplar[[index]]
    rev <- reverse[[index]]
    if (rev[1] == 0)
      firstLine <- paste("Trait", tri[1],"=~"," start(1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+1), sep = "")
    else
      firstLine <- paste("Trait", tri[1],"=~"," start(-1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+1), sep = "")
    if (rev[2] == 0)
      eecondLine <- paste("Trait", tri[1],"=~"," start(1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+2), sep = "")
    else
      secondLine <- paste("Trait", tri[1],"=~"," start(-1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+2), sep = "")


    if (rev[1] == 0)
      thirdLine <- paste("Trait", tri[2],"=~"," start(-1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+1), sep = "")
    else
      thirdLine <- paste("Trait", tri[2],"=~"," start(1)*", "i",
                         as.character(varIndex),"i", as.character(varIndex+1), sep = "")
    if (rev[2] == 0)
      fourthLine <- paste("Trait", tri[2],"=~"," start(1)*", "i",
                         as.character(varIndex+1),"i", as.character(varIndex+2), sep = "")
    else
      fourthLine <- paste("Trait", tri[2],"=~"," start(-1)*", "i",
                         as.character(varIndex+1),"i", as.character(varIndex+2), sep = "")


    if (rev[2] == 0)
      fifthLine <- paste("Trait", tri[3],"=~"," start(-1)*", "i",
                          as.character(varIndex),"i", as.character(varIndex+2), sep = "")
    else
      fifthLine <- paste("Trait", tri[3],"=~"," start(1)*", "i",
                          as.character(varIndex),"i", as.character(varIndex+2), sep = "")
    if (rev[2] == 0)
      sixthLine <- paste("Trait", tri[3],"=~"," start(-1)*", "i",
                         as.character(varIndex+1),"i", as.character(varIndex+2), sep = "")
    else
      sixthLine <- paste("Trait", tri[3],"=~"," start(1)*", "i",
                         as.character(varIndex+1),"i", as.character(varIndex+2), sep = "")

     loadings <- append(loadings, list(c(firstLine, secondLine, thirdLine,
                                         fourthdLine, fifthLine, sixthLine)))










  }
}

CreateTraitFromTriplar <- function(data, nVar, triplar, reverse = NULL)
{
  res <- matrix( ncol = nVar, nrow = nrow(data) )
  res[,] <- 0
  varIndex <- 1
  for (index in 1:length(triplar))
  {

  tri <- triplar[[index]]
  if (is.null(reverse))
    rev <- c(0,0,0)
  else
    rev <- reverse[[index]]
  if (rev[1] == 0)
    res[, tri[1]] <- res[, tri[1]] + data[, varIndex]
  else
    res[, tri[1]] <- res[, tri[1]] + (data[, varIndex] * -1)
  if (rev[1] == 0)
    res[, tri[1]] <- res[, tri[1]] + data[, varIndex+1]
  else
    res[, tri[1]] <- res[, tri[1]] + (data[, varIndex+1]* -1)

  if (rev[2] == 0)
    res[, tri[2]] <- res[, tri[2]] + (data[, varIndex]*-1)
  else
    res[, tri[2]] <- res[, tri[2]] + data[, varIndex]

  if (rev[2] == 0)
    res[, tri[2]] <- res[, tri[2]] + data[, varIndex+2]
  else
    res[, tri[2]] <- res[, tri[2]] + (data[, varIndex+2]*-1)


  if (rev[3] == 0)
    res[, tri[3]] <- res[, tri[3]] + (data[, varIndex+1]*-1)
  else
    res[, tri[3]] <- res[, tri[3]] + data[, varIndex+1]

  if (rev[3] == 0)
    res[, tri[3]] <- res[, tri[3]] + (data[, varIndex+2]*-1)
  else
    res[, tri[3]] <- res[, tri[3]] + data[, varIndex+2]
  varIndex <- varIndex + 3
  }

return(res)

}


