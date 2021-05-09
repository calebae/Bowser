
#' Generate a simulation cohort replicates
#'
#' @param mask.img Maksed image
#' @param background.img Image to be used to generate the normal state
#' @param R Number of replicates to generate
#' @param N Number of images to be generated to this cohort
#' @param Ks Vector of number of clusters to be simulated
#' @param locations Location of clusters to be simulated
#' @param sizes Size of clusters to be simulated
#' @param ints How high the clusters value should be
#' @param parallel Parallel the code or not
#' @param cores Number of cores for parallelization
#' @param seed Setting a seed for reproducibility
#'
#' @return
#' @export
#'
getSimCohortReplicate <- function(mask.img, background.img, R, N, Ks, locations, sizes, ints,
                                  parallel = FALSE, cores = 1, seed = 1){

  img.cohort.rep <- array(0, dim = c(64,64,N,R))
  N.cluster.rep <- array(0, dim = c(N,R))

  if(parallel){

    if(Sys.info()[1]=="Windows"){
      cl = makeCluster(cores, setup_strategy = "sequential")
      clusterSetRNGStream(cl, iseed=seed)

      clusterExport(cl, c("getSimCohort", "getRandomCluster", "getSimCluster", "antsImageRead",
                          "mask.img", "background.img", "N", "Ks", "locations",
                          "sizes", "ints", "resampleImage", "check_ants", "getEmptyImageDimensions",
                          "applyEmptyImageDimensions", "mask_img", "maskImage", "cdist"))

      cohort = parLapply(cl, 1:R, function(i, ...){
        getSimCohort(mask.img, background.img, N, Ks, locations, sizes, ints)
      })
      stopCluster(cl)

    }else{ # for AAPL
      RNGkind("L'Ecuyer-CMRG")
      set.seed(seed)
      cohort <- mclapply(1:R, function(i, ...){
        getSimCohort(mask.img, background.img, N, Ks, locations, sizes, ints)
      }, mc.cores = cores)
    }

    for(i in 1:R){
      img.cohort.rep[,,,i] = cohort[[i]]$img.cohort
      N.cluster.rep[,i] = cohort[[i]]$N.cluster
    }

  }else{
    for(i in 1:R){
      cohort <- getSimCohort(mask.img, background.img, N, Ks, locations, sizes, ints)
      img.cohort.rep[,,,i] = cohort$img.cohort
      N.cluster.rep[,i] = cohort$N.cluster
    }

  }
  return(list(cohort.replicate = img.cohort.rep, N.cluster.rep = N.cluster.rep))
}
