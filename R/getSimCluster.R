

#' Generate the cluster for simulation
#'
#' @param mask.img Maksed image
#' @param background.img Image to be used to generate the normal state
#' @param K Number of clusters to be simulated
#' @param locations Location of clusters to be simulated
#' @param sizes Size of clusters to be simulated
#' @param ints How high the clusters value should be
#'
#' @return Image of clusters
#' @export
#'
getSimCluster <- function(mask.img, background.img, K, locations, sizes, ints){
  stopifnot(nrow(locations) == K)
  stopifnot(length(sizes) == K)
  stopifnot(length(ints) == K)

  image.array <- array(NA, dim = c(64,64,K))

  for(i in 1:K){
    image.array[,,i] <- getSimData(mask.img, background.img, locations[i,], sizes[i], ints[i])
  }

  image.cluster <- apply(image.array, c(1,2), mean)

  return(image.cluster)
}
