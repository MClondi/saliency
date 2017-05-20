#TESTED
maxNormalize <- function(fm, minmax) {
  if (class(fm) == "list") {
    result <- list()
    for (m in 1:length(fm)) {
      result[[m]] <- maxNormalizeIterative(fm[[m]], 3, minmax)
    }
    return(result)
  } else {
    return(maxNormalizeIterative(fm, 3, minmax))
  }
}

#TESTED
maxNormalizeIterative <- function(data, numIter, minmax) {
  iterInhi <- 2.0
  iterCoEx <- 0.5
  iterCoIn <- 1.5
  iterExSig <- 2
  iterInSig <- 25
  
  result <- normalizeImage(clamp(data, 0), minmax)
  
  sz <- max(dim(result))
  maxhw <- max(0,floor(min(dim(result))/2) - 1)
  esig <- sz * iterExSig * 0.01
  isig <- sz * iterInSig * 0.01
  gExc <- gaussKernel(iterCoEx/(esig*sqrt(2*pi)),esig,maxhw)
  gInh <- gaussKernel(iterCoIn/(isig*sqrt(2*pi)),isig,maxhw)
  
  for (iter in 1:numIter) {
    excit <- sepConv2PreserveEnergy(gExc,gExc,result)
    inhib <- sepConv2PreserveEnergy(gInh,gInh,result)
    
    globinhi <- 0.01 * iterInhi * max(result)
    
    result <- clamp((result + excit - inhib - globinhi), 0)
  }
  return(result)
}

#TESTED
clamp <- function(data, bottom, top = NULL) {
  data[data < bottom] <- bottom
  if (!is.null(top)) {
    data[data > top] <- top
  }
  return(data)
}

#TESTED
normalizeImage <- function(img, range) {
  if (range[1] == 0 & range[2] == 0) {
    res <- img
  } else {
    mx <- max(img)
    mn <- min(img)
    if (mx == mn) {
      res <- img - mx + 0.5*sum(range)
    } else {
      res <- (img - mn) / (mx - mn) * abs(range[2]-range[1]) + min(range)
    }
  }
  return(res)
}

#TESTED
gaussKernel <- function(peak, sigma, maxhw) {
  hw <- floor(sigma * sqrt(-2 * log(0.01)))
  
  if ((maxhw > 0) & (hw > maxhw)) {
    hw <- maxhw
  }
  
  if (peak == 0) {
    peak <- 1 / (sigma * sqrt(2*pi))
  }
  
  sig22 <- -0.5 / (sigma * sigma)
  tmp <- peak * exp(- (1:hw)^2 / (2*sigma*sigma))
  kernel <- c(tmp[hw:1], peak, tmp)
  return(kernel)
}
