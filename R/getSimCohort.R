
#' Generate a simulation cohort
#'
#' @param mask.img Maksed image
#' @param background.img Image to be used to generate the normal state
#' @param N Number of images to be generated to this cohort
#' @param Ks Vector of number of clusters to be simulated
#' @param locations Location of clusters to be simulated
#' @param sizes Size of clusters to be simulated
#' @param ints How high the clusters value should be
#'
#' @return Array of images and its number of clusters
#' @export
#'
getSimCohort <- function(mask.img, background.img, N, Ks, locations, sizes, ints){

  img.cohort <- array(0, dim = c(64,64,N))
  N.cluster <- rep(0, dim = N)

  imgCluster <- replicate(N, {
    getRandomCluster(mask.img = mask.img, background.img = background.img,
                     Ks = Ks,
                     locations = locations,
                     sizes = sizes,
                     ints = ints)
  })

  for(i in 1:N){
    img.cohort[,,i] <- imgCluster[1,i]$imgCluster
    N.cluster[i] <- imgCluster[2,i]$K
  }


  return(list(img.cohort = img.cohort, N.cluster = N.cluster))
}
