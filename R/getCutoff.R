
#' Get a intersection point from two densities
#'
#' @param mclust.object Object obtained from Mclust().
#' @param density1 Density object of cluster 1 obtained from density().
#' @param density2 Density object of cluster 2 obtained from density().
#'
#' @return Intersection point where the density1 and density2 cross
#'


getCutoff <- function(mclust.object, density1, density2){

  result <- mclust.object

  mu <- result$parameters$mean
  var <- result$parameters$variance$sigmasq

  ys <- seq(min(density2$x), max(density1$x), by = 0.1)


  if(mu[1]>mu[2]){
    density.difference = dnorm(ys, mean = mu[1], sd= sqrt(var[1])) -
      dnorm(ys, mean = mu[2], sd= sqrt(var[2]))}else{
        density.difference = dnorm(ys, mean = mu[2], sd= sqrt(var[2])) -
          dnorm(ys, mean = mu[1], sd= sqrt(var[1]))
      }

  intersection.point <- ys[(which(diff(density.difference > 0) != 0) + 1)]

  if(mu[1]>mu[2]){
    density.difference = dnorm(ys, mean = mu[1], sd= sqrt(var[1])) -
      dnorm(ys, mean = mu[2], sd= sqrt(var[2]))}else{
        density.difference = dnorm(ys, mean = mu[2], sd= sqrt(var[2])) -
          dnorm(ys, mean = mu[1], sd= sqrt(var[1]))
      }

  intersection.point <- ys[(which(diff(density.difference > 0) != 0) + 1)]

  return(intersection.point)
}
