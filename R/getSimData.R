
#' Generate the data for simulation
#'
#' @param mask.img Maksed image
#' @param background.img Image to be used to generate the normal state
#' @param location Location of cluster to be simulated
#' @param size Size of cluster to be simulated
#' @param int How high the cluster value should be
#'
#' @return Image with cluster
#'
#' @export
#'


getSimData <- function(mask.img, background.img, location, size, int){

  stopifnot(length(location)==2)
  stopifnot(length(size)==1)
  stopifnot(length(int)==1)

  # Mask
  mask <- antsImageRead(mask.img)
  mask <- resampleImage(mask, c(3,3,3)) # Resample for computation
  mask <- mask == 1 # Mask for right lung
  img <- reduce_scan(mask, mask) # Remove masked values
  img <- img$img
  img <- as.array(img) # Convert to array
  slice <- img[,, round(median(1:dim(img)[3])) ] # Pick the middle axial slice
  slice <- cbind(rep(0, 48), slice)

  ## Updated part, make dimension 64x64
  slice <- rbind(rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64),
                 rep(0, 64),
                 slice,
                 rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64),
                 rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64), rep(0, 64))

  # Use the mean image to generate background noise
  meanimg <- antsImageRead(background.img)
  meanimg <- resampleImage(meanimg, c(3,3,3)) # Resample for computation
  meanimg <- maskImage(meanimg, mask) # Mask scan
  meanimg <- reduce_scan(meanimg, mask) # Remove masked values
  meanimg <- meanimg$img
  meanimg <- as.array(meanimg) # Convert to array
  meanslice <- meanimg[,, round(median(1:dim(meanimg)[3])) ] # Pick the middle axial slice
  meanslice <- meanslice[meanslice != 0]

  # Put together images into data frame
  df <- data.frame(r = as.vector(row(slice)),
                   c = as.vector(col(slice)),
                   slice = as.vector(slice))


  # Add background
  df$background <- 0
  df$background[slice == 1] <- sample(meanslice, size = 1867, replace = T)

  # Add disease spot
  df$dist <- cdist(df[,c('r','c')], cbind.data.frame(location[1], location[2]))
  df$spot <- 0
  df$spot[df$dist <= size] <- int
  df$img <- df$background + df$spot

  # Convert back to matrix format
  img <- df$img
  img <- array(img, dim = c(64,64))

  return(img)
}

