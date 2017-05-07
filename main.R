library(imager)
source("pyramid.R")
picture <- load.image("lena.jpg")

# Potrzebna do innych obliczen piramida Gaussa
# Piramida ma 9 poziomów
gaussian_maps_pyramid <- createGaussianMapsPyramid(picture)

# 3 rodzaje piramid - intensity, color, orientation
# Konieczne do obliczenia feature map dla intensity, color, orientation
# Piramida ma 9 poziomów
intensity_maps_pyramid <- createIntensityMapsPyramid(gaussian_maps_pyramid)

color_RG_maps_pyramid <- createRGMapsPyramid(gaussian_maps_pyramid)
color_BY_maps_pyramid <- createBYMapsPyramid(gaussian_maps_pyramid)

orientation_0_maps_pyramid <- createOrientationMapsPyramid(intensity_maps_pyramid, 0)
orientation_45_maps_pyramid <- createOrientationMapsPyramid(intensity_maps_pyramid, 45)
orientation_90_maps_pyramid <- createOrientationMapsPyramid(intensity_maps_pyramid, 90)
orientation_135_maps_pyramid <- createOrientationMapsPyramid(intensity_maps_pyramid, 135)

# Tworzenie feature maps, z których powstaną conspicuity maps
# Feature map ma 6 poziomów
intensity_feature_maps <- createFeatureMaps(intensity_maps_pyramid)

color_RG_feature_maps <- createFeatureMaps(color_RG_maps_pyramid)
color_BY_feature_maps <- createFeatureMaps(color_BY_maps_pyramid)

orientation_0_feature_maps <- createFeatureMaps(orientation_0_maps_pyramid)
orientation_45_feature_maps <- createFeatureMaps(orientation_45_maps_pyramid)
orientation_90_feature_maps <- createFeatureMaps(orientation_90_maps_pyramid)
orientation_135_feature_maps <- createFeatureMaps(orientation_135_maps_pyramid)

# Tworzenie conspicuity maps
intensity_conspicuity_map <- doNormalization(addFeatureMapsAcrossScale(intensity_feature_maps))

color_RG_temp_map <- doNormalization(addFeatureMapsAcrossScale(color_RG_feature_maps))
color_BY_temp_map <- doNormalization(addFeatureMapsAcrossScale(color_BY_feature_maps))
color_conspicuity_map <- doNormalization(addMaps(c(color_RG_temp_map, color_BY_temp_map)))

orientation_0_temp_map <- doNormalization(addFeatureMapsAcrossScale(orientation_0_feature_maps))
orientation_45_temp_map <- doNormalization(addFeatureMapsAcrossScale(orientation_45_feature_maps))
orientation_90_temp_map <- doNormalization(addFeatureMapsAcrossScale(orientation_90_feature_maps))
orientation_135_temp_map <- doNormalization(addFeatureMapsAcrossScale(orientation_135_feature_maps))
orientation_conspicuity_map <- doNormalization(addMaps(c(orientation_0_temp_map, orientation_45_temp_map, orientation_90_temp_map, orientation_135_temp_map)))

# Tworzenie silency map

# PS Nie wiem, czy to dzielenie przejdzie w takiem formie :D
silency_map <- addMaps(c(intensity_conspicuity_map, color_conspicuity_map, orientation_conspicuity_map)) / 3



# Funkcje do zaimplementowania, można je przenieść do innych plików
# source("nazwaPliku.R")
createFeatureMaps <- function(pyramid) {
  
  featureMaps <- list()
  for (c in 2:4) {
    for(s in c+3:c+4) {
      featureMaps <- c(featureMaps, list(createFeatureMap(pyramid, c, s)))
    }
  }
  return(featureMaps)
}

createFeatureMap <- function(pyramid, c, s) {
  
  mapAfterSubtraction <- subtractMapsAcrossScale(pyramid[[c]], pyramid[[s]])
  featureMap <- doNormalization(mapAfterSubtraction)
  return(featureMap)
}

subtractMapsAcrossScale <- function(map1, map2) {
  
  return(map1)
}

doNormalization <- function(map) {
  
  return(map)
}

addFeatureMapsAcrossScale <- function(feature_maps) {
  
  resultMap <- feature_maps[[1]]
  for(i in 2:length(feature_maps)) {
    resultMap <- addMapsAcrossScale(resultMap, feature_maps[[i]])
  }
  return(resultMap)
}

addMapsAcrossScale <- function(map1, map2) {
  
  return(map1)
}


# maps to lista map
addMaps <- function(maps) {
  
}