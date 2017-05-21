#TESTED
getRGB <- function(img) {
  lum_thresh <- 0.1
  
  r <- img[,,,1]
  g <- img[,,,2]
  b <- img[,,,3]
  maxRGB = pmax(pmax(r,g),b)
  
  low_maxRGB = (maxRGB < lum_thresh)
  r[low_maxRGB] <- 0
  g[low_maxRGB] <- 0
  b[low_maxRGB] <- 0
  return (list(r, g, b, maxRGB))
}

#TESTED
rgb2gray <- function(img) {
  T <- matrix(c(1.0, 0.956, 0.621, 1.0, -0.272, -0.647, 1.0, -1.106, 1.703), nrow = 3, byrow = TRUE)
  coef <- matrix(solve(T)[1,], nrow = 3, ncol = 1)
  orig_size <- dim(img)
  dim(img) <- c(orig_size[1] * orig_size[2], 1, 1, 3)
  img_data <- as.matrix(img)
  gray_img <- img_data %*% coef
  img <- as.cimg(gray_img)
  dim(img) <- c(orig_size[1], orig_size[2], 1, 1)
  return(img)
}

#TESTED
sepConv2PreserveEnergy <- function(filter1, filter2, img) {
  sd1 <- nrow(img)
  sd2 <- ncol(img)
  result <- conv2Vectors(filter1, filter2, img, 0)
  
  fl1 <- length(filter1)
  fl1b <- floor((fl1-1)/2)
  
  fc1a <- cumsum(filter1)
  fc1b <- cumsum(filter1[fl1:1])
  fs1 <- sum(filter1)
  
  if (fl1 > sd1) {
    tmp <- c(rep(0, fl1), fc1a, fs1*rep(1, fl1))
    range <- fl1+fl1b+(1:sd1)
    ff1 <- do.call(cbind, replicate(sd2, tmp[range]-tmp[range-sd1], simplify=FALSE))
    result <- result * fs1 / ff1
  } else {
    ff1a <- do.call(cbind, replicate(sd2, fc1a[(fl1b+1):(length(fc1a)-1)], simplify=FALSE))
    result[1:fl1b,] <- result[1:fl1b,] * fs1 / ff1a
    ff1b <- do.call(cbind, replicate(sd2, fc1b[(fl1b+1):(length(fc1b)-1)], simplify=FALSE))
    result[nrow(result):(nrow(result)-fl1b+1),] <- result[nrow(result):(nrow(result)-fl1b+1),] * fs1 / ff1b
  }
  
  fl2 = length(filter2)
  fl2b = floor((fl2-1)/2)
  
  fc2a = cumsum(filter2)
  fc2b = cumsum(filter2[fl2:1])
  fs2 = sum(filter2)
  
  if (fl2 > sd2) {
    tmp <- c(rep(0, fl2), fc2a, fs2*rep(1, fl2))
    range <- fl2+fl2b+(1:sd2)
    ff2 <- do.call(rbind, replicate(sd1, tmp[range]-tmp[range-sd2], simplify=FALSE))
    result <- result * fs2 / ff2
  } else {
    ff2a <- do.call(rbind, replicate(sd1, fc2a[(fl2b+1):(length(fc2a)-1)], simplify=FALSE))
    result[,1:fl2b] <- result[,1:fl2b] * fs2 / ff2a
    ff2b <- do.call(rbind, replicate(sd1, fc2b[(fl2b+1):(length(fc2b)-1)], simplify=FALSE))
    result[,ncol(result):(ncol(result)-fl2b+1)] <- result[,ncol(result):(ncol(result)-fl2b+1)] * fs2 / ff2b
  }
  return(result)
}

#Rounding problems!!
conv2PreserveEnergy <- function(img, filter) {
  sh <- nrow(img)
  sw <- ncol(img)
  fh <- nrow(filter)
  fw <- ncol(filter)
  fw2 <- floor((fw-1)/2)
  fh2 <- floor((fh-1)/2)
  
  partial_result <- conv2Matrixes(img, filter, 1)
  prh <- nrow(partial_result)
  prw <- ncol(partial_result)
  result <- partial_result[(fh2+1):(prh-fh2),(fw2+1):(prw-fw2)]
  filt2 <- filter[nrow(filter):1, ncol(filter):1]
  fsum <- sum(filter)
  
  if ((fh2 > 0) & (fh2 <= sh) & (sw - 2*fw2 > 0)) {
    TcSum <- rowSums(apply(filter, 2, cumsum))
    #Rounding problem when calculating sT
    sT <- fsum/TcSum[fh2+(1:fh2)]
    sT <- matrix(sT, nrow = length(sT))
    sTmat <- do.call(cbind, replicate(sw - 2*fw2, sT, simplify=FALSE))
    result[1:fh2,(1+fw2):(sw-fw2)] <- result[1:fh2,(1+fw2):(ncol(result)-fw2)] * sTmat
    
    BcSum <- rowSums(apply(filt2, 2, cumsum))
    #Rounding problem when calculating sB
    sB <- (fsum/BcSum[fh2+(1:fh2)])[fh2:1]
    sB <- matrix(sB, nrow = length(sB))
    sBmat <- do.call(cbind, replicate(sw - 2*fw2, sB, simplify=FALSE))
    result[(sh-fh2+1):sh,(1+fw2):(sw-fw2)] <- result[(sh-fh2+1):sh,(1+fw2):(sw-fw2)] * sBmat
  }
  
  # Bug in saliency toolbox - here should be rescaling along left and right borders
  # If condition is always false! Omitting this piece of code
  
  # Bug in saliency toolbox - here should be rescaling for corners
  # If condition is always false! Omitting this piece of code
  
  return(result)
}

#TESTED
imageResize <- function(img, target_size) {
  img_size <- dim(img)
  indicesRows <- contributions(img_size[1], target_size[1])
  indicesCols <- contributions(img_size[2], target_size[2])
  return(img[indicesRows, indicesCols])
}

#TESTED
contributions <- function(in_len, out_len) {
  scale <- out_len / in_len
  x <- 1:out_len
  u <- x/scale + 0.5 * (1 - 1/scale)
  left <- floor(u - 0.5)
  indices <- bsxfunPlus(left, 0:2)
  weights <- hBoxKernel(bsxfunMinus(u, indices))
  weights <- bsxfunRdivide(weights, rowSums(weights))
  indices <- matrix(pmin(pmax(1, indices), in_len), ncol = 3)
  kill <- NULL
  for (col in 1:3) {
    if (all(weights[,col] == 0)) {
      kill <- c(kill, col)
    }
  }
  return(indices[,setdiff(1:3, kill)])
}

#TESTED
attenuateBorders <- function(img, border_size) {
  result <- img
  dsz <- dim(img)
  
  if (border_size * 2 > dsz[1]) {
    border_size <- floor(dsz[1] / 2)
  }
  if (border_size * 2 > dsz[2]) {
    border_size <- floor(dsz[2] / 2)
  }
  if (border_size < 1) {
    return(result)
  }
  
  bs <- 1:border_size
  coeffs <- bs / (border_size + 1)
  
  rec <- do.call(cbind, replicate(dsz[2], t(coeffs), simplify=FALSE))
  result[bs,] <- result[bs,] * rec
  range <- dsz[1] - bs + 1
  result[range,] <- result[range,] * rec
  
  rec <- do.call(rbind, replicate(dsz[1], coeffs, simplify=FALSE))
  result[,bs] <- result[,bs] * rec
  range <- dsz[2] - bs + 1
  result[,range] <- result[,range] * rec
  return(result)
}