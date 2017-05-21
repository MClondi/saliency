library(imager)
source("featurePyramids.R")
source("gaussianPyramid.R")
source("imageProcessing.R")
source("utils.R")
source("normalization.R")
source("featureMaps.R")
image <- load.image("img/balloons.png")

# 3 rodzaje piramid - intensity, color, orientation
# Konieczne do obliczenia feature map dla intensity, color, orientation
color_RG_maps_pyramid <- makeRedGreenPyramid(image)
color_BY_maps_pyramid <- makeBlueYellowPyramid(image)

intensity_maps_pyramid <- makeIntensityPyramid(image)

orientation_0_maps_pyramid <- makeOrientationPyramid(intensity_maps_pyramid, 0)
orientation_45_maps_pyramid <- makeOrientationPyramid(intensity_maps_pyramid, 45)
orientation_90_maps_pyramid <- makeOrientationPyramid(intensity_maps_pyramid, 90)
orientation_135_maps_pyramid <- makeOrientationPyramid(intensity_maps_pyramid, 135)

# Tworzenie feature maps, z których powstaną conspicuity maps
# Feature map ma 6 poziomów
color_RG_feature_maps <- maxNormalize(centerSurround(color_RG_maps_pyramid), c(0, 10))
color_BY_feature_maps <- maxNormalize(centerSurround(color_BY_maps_pyramid), c(0, 10))

intensity_feature_maps <- maxNormalize(centerSurround(intensity_maps_pyramid), c(0, 10))

orientation_0_feature_maps <- maxNormalize(centerSurround(orientation_0_maps_pyramid), c(0, 10))
orientation_45_feature_maps <- maxNormalize(centerSurround(orientation_45_maps_pyramid), c(0, 10))
orientation_90_feature_maps <- maxNormalize(centerSurround(orientation_90_maps_pyramid), c(0, 10))
orientation_135_feature_maps <- maxNormalize(centerSurround(orientation_135_maps_pyramid), c(0, 10))

# Tworzenie conspicuity maps
color_RG_combined_map <- maxNormalize(combineMaps(color_RG_feature_maps), c(0, 0))
color_BY_combined_map <- maxNormalize(combineMaps(color_BY_feature_maps), c(0, 0))
intensity_combined_map <- maxNormalize(combineMaps(intensity_feature_maps), c(0, 0))
orientation_0_combined_map <- maxNormalize(combineMaps(orientation_0_feature_maps), c(0, 0))
orientation_45_combined_map <- maxNormalize(combineMaps(orientation_45_feature_maps), c(0, 0))
orientation_90_combined_map <- maxNormalize(combineMaps(orientation_90_feature_maps), c(0, 0))
orientation_135_combined_map <- maxNormalize(combineMaps(orientation_135_feature_maps), c(0, 0))

color_conspicuity_map <- maxNormalize(combineMaps(list(color_RG_combined_map, color_BY_combined_map)), c(0, 0)) / 6
intensity_conspicuity_map <- intensity_combined_map / 3
orientation_conspicuity_map <- maxNormalize(combineMaps(list(orientation_0_combined_map, orientation_45_combined_map, orientation_90_combined_map, orientation_135_combined_map)), c(0, 0)) / 12

# Tworzenie silency map
silency_map <- maxNormalize(combineMaps(list(
  color_conspicuity_map,
  intensity_conspicuity_map,
  orientation_conspicuity_map)), c(0, 2))

plot(imrotate(as.cimg(silency_map[,ncol(silency_map):1]), 90))
