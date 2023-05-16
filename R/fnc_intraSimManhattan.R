#' Function to evaluate intra-similarity
#'
#' @export

intraSim <- function(comm) {
  # Transform community data as matrix
  comm <- as.matrix(comm)
  # comm <- comm + 1

  # Data frames to store results
  nC <- ncol(comm)
  # sp <- colnames(comm)
  outlist <- data.frame(contr = numeric(nC),
                        sd = numeric(nC),
                        stringsAsFactors = F)

  # Measure mean dissimilarity contribution in group per variable
  for(i in seq_len(nC)) {
    # Similarity for each column and data of group in comparison[j]
    sim <- dist(comm[,i,drop=FALSE], method="manhattan")
    sim <- as.vector(sim)

    # Extract mean and sd contribution to inter-group dissimilarity
    outlist$contr[i] <- mean(sim)
    outlist$sd[i] <- sd(sim)
  }

  # Transform to similarity using .1 / .1+contribution
  outlist$contr <- .1 / (.1 + outlist$contr)

  return(outlist)
}

#' Function to evaluate intra-similarity with bootstrap procedure
#'
#' @export

bootIntraSim <- function(x, iter = 250, samp = .25, boot = 40000) {
  # If the dataset is too large, the analysis will be done through a bootstrap
  # List to store simper results for post-processing
  # Determine if the bootstrap procedure is necessary
  # I evaluated the minimum number of observations necessary to initiate the bootstrap
  # procedure to be 4000. That was chosen by running the analyses and opting for
  # a number of observations that I deemed could run fast enough.
  # It is however still a subjective choice and can definitely be changed.
  if(nrow(x) > boot) {
    sim <- vector('list', iter)

    # Repeat the process for the desired number of iterations
    for(i in 1:iter) {
      cat("   i: ", i, ' of ', iter, "\r")
      # Subsampled observation id
      id <- sample(1:nrow(x), size = samp*nrow(x))
      # Similarity analysis
      sim[[i]] <- intraSim(x[id, ])
    }
  } else {
    sim <- intraSim(x)
  }
  sim
}
