#TESTED
safeDivide <- function(vec_dividend, vec_divisor) {
  zero_division_indexes <- vec_divisor == 0
  vec_divisor[zero_division_indexes] <- 1
  result <- vec_dividend / vec_divisor
  result[zero_division_indexes] <- 0
  return(result)
}

#TESTED
makeGaborFilter <- function(angle) {
  rtDeg <- pi * angle / 180
  omega <- 2 * pi / 7
  co <- cos(rtDeg)
  si <- -sin(rtDeg)
  sigq <- 2 * (2 + 1/3)^2
  vec <- as.matrix(-4:4)
  vlen <- 9
  vco <- vec*co
  vsi <- vec*si
  major <- do.call(cbind, replicate(vlen, vco, simplify=FALSE)) + do.call(rbind, replicate(vlen, t(vsi), simplify=FALSE))
  major2 <- major^2
  minor <- do.call(cbind, replicate(vlen, vsi, simplify=FALSE)) - do.call(rbind, replicate(vlen, t(vco), simplify=FALSE))
  minor2 <- minor^2
  phase0 <- exp(- major2 / sigq - minor2 / sigq)
  
  result <- cos(omega * major) * phase0
  result <- result - mean(result)
  filter <- result / sqrt(sum(result^2))
  
  result <- cos(omega * major + pi/2) * phase0
  result <- result - mean(result)
  filter <- list(filter, + result / sqrt(sum(result^2)))
  return(filter)
}

#TESTED
conv2 <- function(matA, matB) {
  matB_dim <- dim(matB) - 1
  result_dim <- dim(matA) + matB_dim
  extended_matA <- matA
  for (row in 1:matB_dim[1]) {
    extended_matA <- rbind(0, extended_matA)
    extended_matA <- rbind(extended_matA, 0)
  }
  for (col in 1:matB_dim[2]) {
    extended_matA <- cbind(0, extended_matA)
    extended_matA <- cbind(extended_matA, 0)
  }
  
  matB <- matB[nrow(matB):1, ncol(matB):1]
  result <- matrix(0, nrow = result_dim[1], ncol = result_dim[2])
  for (row in 1:result_dim[1]) {
    for (col in 1:result_dim[2]) {
      result[row, col] <- sum(matB * extended_matA[row:(row+matB_dim[1]), col:(col+matB_dim[2])])
    }
  }
  return(result)
}

#TESTED
conv2Vectors <- function(vec1, vec2, mat, full) {
  mat_vec1 <- matrix(vec1, nrow = length(vec1))
  mat_vec2 <- matrix(vec2, ncol = length(vec2))
  return(conv2Matrixes(mat, mat_vec1 %*% mat_vec2, full))
}

#TESTED
conv2Matrixes <- function(matA, matB, full) {
  convolution <- conv2(matA, matB)
  if (full) {
    return(convolution)
  } else {
    orig_size <- dim(matA)
    current_size <- dim(convolution)
    row_diff <- current_size[1] - orig_size[1]
    col_diff <- current_size[2] - orig_size[2]
    return(convolution[(ceiling(row_diff / 2) + 1):(orig_size[1] + floor(row_diff / 2)),
                       (ceiling(col_diff / 2) + 1):(orig_size[2] + floor(col_diff / 2))])
  }
}


#TESTED
bsxfunPlus <- function(vec1, vec2) {
  result <- matrix(0, nrow = length(vec1), ncol = length(vec2))
  for (row in 1:length(vec1)) {
    for (col in 1:length(vec2)) {
      result[row, col] <- vec1[row] + vec2[col]
    }
  }
  return(result)
}

#TESTED
bsxfunMinus <- function(vec, mat) {
  result <- matrix(0, nrow = length(vec), ncol = ncol(mat))
  for (row in 1:length(vec)) {
    for (col in 1:ncol(mat)) {
      result[row, col] <- vec[row] - mat[row, col]
    }
  }
  return(result)
}

#TESTED
bsxfunRdivide <- function(mat, vec) {
  result <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  for (row in 1:nrow(mat)) {
    for (col in 1:ncol(mat)) {
      result[row, col] <- mat[row, col] / vec[row]
    }
  }
  return(result)
}

#TESTED
hBoxKernel <- function(A) {
  return(A < 0.5 & A >= -0.5)
}

#TESTED
combineMaps <- function(maps) {
  combinedMap <- 0
  for (map in 1:length(maps)) {
    combinedMap <- combinedMap + maps[[map]]
  }
  return(combinedMap)
}