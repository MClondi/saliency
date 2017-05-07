createGaussianMapsPyramid <- function(picture) {
  
  pyramid <- list(picture)
  pic <- picture
  for(i in 1:8) {
    isoblur(pic, gaussian = TRUE, sigma = 2)
    pic <- resize(pic, round(width(pic)/2), round(height(pic)/2))
    pyramid <- c(pyramid, list(pic))
  }
  return(pyramid)
  
}

createIntensityMapsPyramid <- function(gaussian_maps_pyramid) {
  
  intensity_pyr <- list()
  for(i in 1:(length(gaussian_maps_pyramid))) {
    img <- gaussian_maps_pyramid[[i]]
    mi <- (img[,,,1] + img[,,,2] + img[,,,3] )/3
    intensity_pyr <- c(intensity_pyr, list(mi))
  }
  return(intensity_pyr)
}

createRGMapsPyramid <- function(gaussian_maps_pyramid) {
  
  rb_pyr <- list()
  for(i in 1:(length(gaussian_maps_pyramid))) {
    img <- gaussian_maps_pyramid[[i]]
    rgbMax <- pmax(img[,,,1], img[,,,2], img[,,,3] )
    redSupGreen <- img[,,,1] - img[,,,2]
    rb <- redSupGreen/rgbMax
    rb_pyr <- c(rb_pyr, list(rb))
  }
  return(rb_pyr)
}

createBYMapsPyramid <- function(gaussian_maps_pyramid) {
  
  by_pyr <- list()
  for(i in 1:(length(gaussian_maps_pyramid))) {
    img <- gaussian_maps_pyramid[[i]]
    rgbMax <- pmax(img[,,,1], img[,,,2], img[,,,3] )
    blueSupMinRedGreen <- img[,,,3] - pmin(img[,,,1], img[,,,2])
    by <- blueSupMinRedGreen/rgbMax
    by_pyr <- c(by_pyr, list(by))
  }
  return(by_pyr)
}

createOrientationMapsPyramid <- function(intensity_maps_pyramid, phase) {
  
  # TODO
  return(intensity_maps_pyramid)
}
