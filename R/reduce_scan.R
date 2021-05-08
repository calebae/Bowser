#' Function used to reduce images
#'
#' @param img Name of the file to read the image from
#' @param mask Name of the file to read the mask from
#'
#' @return Lists of reduced image
#'
#'

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
