# simulation run function







# rnd function/call
site <- function(sites = 2) {
     # error checks
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
     roll <- as.integer(stats::runif(1, min = 1, max = sites + 1))
     return(roll)
}