
#' Generate random cluster for simulation
#'
#' @param mask.img Maksed image
#' @param background.img Image to be used to generate the normal state
#' @param Ks Vector of number of clusters to be simulated
#' @param locations Location of clusters to be simulated
#' @param sizes Size of clusters to be simulated
#' @param ints How high the clusters value should be
#'
#' @return Image of clusters with random number of clusters, locations, sizees and intensity
#' @export
#'
getRandomCluster <- function(mask.img, background.img, Ks, locations, sizes, ints){
  K = sample(Ks, 1)

  size = sample(sizes, K, replace = TRUE)
  int = sample(ints, K, replace = TRUE)
  inds = sample(1:nrow(locations), K)
  location = locations[inds,]
  imgCluster <- getSimCluster(mask = mask.img,
                              background.img = background.img,
                              K = K,
                              location = location,
                              size = size,
                              int = int)

  return(list(imgCluster = imgCluster, K = K))
}
