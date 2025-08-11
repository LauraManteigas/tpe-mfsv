transform_samples <- function(x, bounds) {
  y <- matrix(NA, nrow = nrow(x), ncol = ncol(x))
  colnames(y) <- colnames(x)
  
  for (param in colnames(x)) {
    a <- bounds[parameter == param, min]
    b <- bounds[parameter == param, max]
    # Clip to avoid log(0)
    x_clip <- pmin(pmax(x[, param], a + 1e-9), b - 1e-9)
    y[, param] <- log((x_clip - a) / (b - x_clip))
  }
  y
}

inverse_transform_samples <- function(y, bounds) {
  x <- matrix(NA, nrow = nrow(y), ncol = ncol(y))
  colnames(x) <- colnames(y)
  
  for (param in colnames(y)) {
    a <- bounds[parameter == param, min]
    b <- bounds[parameter == param, max]
    e_y <- exp(y[, param])
    x[, param] <- (a + b * e_y) / (1 + e_y)
  }
  
  x
}
