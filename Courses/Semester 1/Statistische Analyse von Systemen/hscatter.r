hscatter <- function(geoobj, h, eps, which.col = 1) {
  coords <- geoobj$coords
  data <- geoobj$data
  if(is.matrix(data)) data <- data[,which.col]
  n <- NROW(coords)
  if(n<2) stop("Dataset is too small!")
  idxpairs <- expand.grid(1:n,1:n)
  d <- c(as.matrix(dist(coords[1:n,])))
  ij <- idxpairs[abs(d-h)<=eps,]
  if(is.null(ij)) {warning("No pairs found!"); return(ij)}
  else return(cbind(data[ij[,1]], data[ij[,2]]))
}
cat("hscatter successfully loaded")