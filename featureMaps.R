#TESTED
centerSurround <- function(pyr) {
  siz <- dim(pyr[[5]])
  numLevels <- length(pyr)
  maps <- list()
  featureMaps <- list()
  for (level in 3:9) {
    if (level > numLevels) {
      break
    }
    maps[[level - 2]] <- imageResize(pyr[[level]], siz)
  }
  cc <- 1
  border_size <- round(max(siz)/20)
  for (level in 3:5) {
    for (delta in 3:4) {
      l2 <- level + delta
      if (l2 <= numLevels) {
        featureMaps[[cc]] <- attenuateBorders(abs(maps[[level - 2]] - maps[[l2 - 2]]), border_size)
        cc <- cc + 1
      }
    }
  }
  return(featureMaps)
}