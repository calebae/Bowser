
#' Binarize the image
#'
#' @param image Image that need to be binarize
#' @param intersection.point Cutoff value to binarize the image
#'
#' @return Binarized image
#' @export
#'

Binarize <- function(image, intersection.point){
  image.Binarize <- image

  image.Binarize[image.Binarize> intersection.point] <- 1
  image.Binarize[image.Binarize< intersection.point] <- 0
  image.Binarize[is.na(image.Binarize)]<-0

  return(image.Binarize)
}
