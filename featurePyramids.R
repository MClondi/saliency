#TESTED
makeIntensityPyramid <- function(image) {
  gray_image <- matrix(rgb2gray(image), nrow = ncol(image), byrow = TRUE)
  return(makeGaussianPyramid(gray_image))
}

#TESTED
makeRedGreenPyramid <- function(image) {
  rgb <- getRGB(image)
  rg <- matrix(safeDivide((rgb[[1]] - rgb[[2]]), rgb[[4]]), nrow = ncol(image), byrow = TRUE)
  return(makeGaussianPyramid(rg))
}

#TESTED
makeBlueYellowPyramid <- function(image) {
  rgb <- getRGB(image)
  by <- matrix(safeDivide(rgb[[3]] - pmin(rgb[[1]], rgb[[2]]), rgb[[4]]), nrow = ncol(image), byrow = TRUE)
  return(makeGaussianPyramid(by))
}

#TESTED
makeOrientationPyramid <- function(intensity_pyramid, angle) {
  orientationPyramid <- list()
  gabor_filter <- makeGaborFilter(angle)
  for (level in 1:length(intensity_pyramid)) {
    fres <- abs(conv2PreserveEnergy(intensity_pyramid[[level]], gabor_filter[[1]]))
    fres2 <- abs(conv2PreserveEnergy(intensity_pyramid[[level]], gabor_filter[[2]]))
    fres_sum <- mapply(sum, fres, fres2)
    new_map <- matrix(fres_sum, nrow = nrow(intensity_pyramid[[level]]), byrow = FALSE)
    orientationPyramid[[level]] <- new_map
  }
  return(orientationPyramid)
}
