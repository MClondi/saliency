#TESTED
makeGaussianPyramid <- function(image) {
  pyramid <- list(image)
  level <- 1
  while(min(dim(pyramid[[level]])) > 1) {
    level <- level + 1
    pyramid[[level]] <- gaussianSubsample(pyramid[[level - 1]])
  }
  return(pyramid)
}

#TESTED
gaussianSubsample <- function(img) {
  filter <- c(1,5,10,10,5,1,1)
  filter <- filter/sum(filter)
  dims <- dim(img)
  h <- dims[1]
  w <- dims[2]
  if (min(w, h) > 3) {
    convResult = sepConv2PreserveEnergy(filter, filter, img)
    horResult = convResult[, seq(2, ncol(convResult), 2) - 1]
    result = horResult[seq(2, nrow(horResult), 2) -1,]
  } else {
    if (w <= 1) {
      horResult <- img
    }
    else if (w == 2) {
      horResult <- matrix(rowMeans(img), nrow = h)
    }
    else if (w == 3) {
      horResult <- matrix(rowMeans(img[,c(1,2,2,3)]), nrow = h)
    }
    else {
      horResult <- sepConv2PreserveEnergy(1,filter,img)
      horResult <- matrix(horResult[,seq(1, ncol(horResult) - 1, 2)], nrow = h)
    }
    
    if (h <= 1) {
      result <- horResult
    }
    else if (h == 2) {
      result <- matrix(colMeans(horResult), ncol = ncol(horResult))
    }
    else if (h == 3) {
      result <- matrix(colMeans(matrix(horResult[c(1,2,2,3),], nrow = 4)), ncol = ncol(horResult))
    }
    else {
      verResult <- sepConv2PreserveEnergy(filter,1,horResult)
      result <- verResult[seq(1, length(verResult) - 1, 2),]
      result <- matrix(result, nrow = length(result))
    }
  }
  return(result)
}