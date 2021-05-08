## code to prepare `loc5_size2_int200_sim1` dataset goes here



library(ANTsR)
library(extrantsr)
library(neurobase)

# Function used to reduce scan
reduce_scan = function(img, mask) {
  img = check_ants(img)
  mask = check_ants(mask)
  inds = getEmptyImageDimensions(mask)
  drop_img = applyEmptyImageDimensions(img = img,inds = inds)
  drop_mask = applyEmptyImageDimensions(img = mask, inds = inds)
  drop_img = mask_img(drop_img, drop_mask)
  L = list(img = drop_img,mask = drop_mask)
  return(L)
}



########################################################
# 1. Pick slice from template lung to base simulations
########################################################

# Mask
mask <- ANTsRCore::antsImageRead('inst/extdata/lung_template_mask.nii.gz')
mask <- ANTsRCore::resampleImage(mask, c(3,3,3)) # Resample for computation
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
meanimg <- ANTsRCore::antsImageRead('inst/extdata/lung_template_meanHU.nii.gz')
meanimg <- ANTsRCore::resampleImage(meanimg, c(3,3,3)) # Resample for computation
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
set.seed(032720) # Randomness comes into play when adding the background

df$background <- 0
df$background[slice == 1] <- sample(meanslice, size = 1867, replace = T)

## Simulation settings
#loc1s <- c(26, 18, 31, 40, 40)
#loc2s <- c(16, 31, 46, 21, 57)
#sizes <- c(2, 4, 6, 3, 5)
#ints <- c(100, 200, 400, 0, 300)

# Simulation parameters
loc1 = 40
loc2 = 57
size = 2
int = 200



# Add disease spot
df$dist <- rdist::cdist(df[,c('r','c')], cbind.data.frame(loc1, loc2))
df$spot <- 0
df$spot[df$dist <= size] <- int
df$img <- df$background + df$spot

# Convert back to matrix format
img <- df$img
img <- array(img, dim = c(64,64))


loc5_size2_int200_sim1 <- img

usethis::use_data(loc5_size2_int200_sim1, overwrite = TRUE)
