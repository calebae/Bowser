
#' Unsupervised bayesian method to estimate the number of clusters
#'
#' @param image.mat Raw image which the number of clusters should be estimated
#' @param interval.size Interval size that used when binarize the image
#' @param r Width of the neighborhood when applying the median filtering
#' @param vacuum.size Minimum size of clusters
#'
#' @return
#' @export
#'
bowser <- function(image.mat, interval.size = 0.1, r = 6, vacuum.size = 30){

  image.mat[image.mat==0] <- NA


  datForClustr <- image.mat[!is.na(image.mat)]
  result <- Mclust(datForClustr, G=1:2)

  den1 <- density(datForClustr[result$classification==1])
  den2 <- density(datForClustr[result$classification==2])

  intersection.point <- getCutoff(result, density1 = den1, density2 = den2, interval = interval.size)

  image.ROI <- Binarize(image.mat, intersection.point)

  k <- shapeKernel(c(r,r), type="box")
  image.Filtered <- mmand::medianFilter(image.ROI, k)

  q <- shapeKernel(c(2,2), type = "box")
  image.Connected <- mmand::components(image.Filtered, q)

  if(sum(!is.na(image.Connected)) !=0){
  image.Vacuum <- Vacuum(image.Connected, size = vacuum.size)
  image.final <- mmand::components(image.Vacuum, q)

  num.Cluster <- max(image.final, na.rm=T)}else{
    num.Cluster = 0
  }

  return(num.Cluster)
}
