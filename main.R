library(imager)
pic <- load.image("lena.jpg")
pyramid <- list(pic)
for(i in 1:8) {
  #todo: blurring the image
  pic <- resize(pic, round(width(pic)/2), round(height(pic)/2))
  pyramid <- c(pyramid, list(pic))
}

intensity_pyr <- list()
rb_pyr <- list()
by_pyr <- list()
for(i in 1:9) {
  img <- pyramid[[i]]
  rgbMax <- pmax(img[,,,1], img[,,,2], img[,,,3] )
  redSupGreen <- img[,,,1] - img[,,,2]
  blueSupMinRedGreen <- img[,,,3] - pmin(img[,,,1], img[,,,2])
  
  mi <- (img[,,,1] + img[,,,2] + img[,,,3] )/3
  intensity_pyr <- c(intensity_pyr, list(mi))
  
  rb <- redSupGreen/rgbMax
  rb_pyr <- c(rb_pyr, list(rb))
  
  by <- blueSupMinRedGreen/rgbMax
  by_pyr <- c(by_pyr, list(by))
}