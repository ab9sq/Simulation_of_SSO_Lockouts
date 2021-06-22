# simulation run function
sim <- function(run = 100,
                sites = 2){
     runs <- 1:run
     dcCount[1:sites] <- 0
     test <- FALSE
     trialCount <- 0
     results <- NULL
     for(index in runs){
          while(test == FALSE){
               trialCount <- trialCount + 1
               dataCenter <- siteSelectioned(sites = sites)
               dcCount[dataCenter] <- dcCount[dataCenter] + 1
               if(dcCount[dataCenter] >= 10){
                    test <- TRUE
               }
          }
          results[index] <- trialCount
          trialCount <- 0
     }
     return(results)
}

# rnd function/call
siteSelectioned <- function(sites = 2) {
     # error checks and corrections
     if (is.null(sites)){
          sites <- 2
     }
     if ( ( (sites < 2)) | (is.na(sites))) {
          sites <- 2
     }
     if (!is.numeric(sites)){
          sites <- 2
     }
     # functions work
     sites <- as.integer(sites)
     sites <- as.integer(stats::runif(1, min = 1, max = sites + 1))
     return(sites)
}
