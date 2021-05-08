
#' Remove the noise
#'
#' @param image Matrix (or array) with connected components
#' @param size Minimum number of pixels (or voxels) to be considered as cluster
#'
#' @return Matrix (or array) with noise removed
#'

Vacuum <- function(image, size = 30){

  image.connected <- image

  table.tmp <- data.frame(cluster = unique(as.vector(image.connected[!is.na(image.connected)])))
  for(i in 1:nrow(table.tmp)){
    table.tmp$size[i] <- sum(image.connected[image.connected==table.tmp$cluster[i]], na.rm=T)
  }

  table.tmp <- table.tmp %>% filter(size > 30)

  final.img <- apply(image.connected, c(1,2), function(x){x %in% table.tmp$cluster})

  return(final.img)

}
