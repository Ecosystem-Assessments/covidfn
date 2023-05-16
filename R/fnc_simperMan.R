#' Function to run SIMPER analysis 
#'
#' @export

simperMan <- function(comm, group) {
  
  # library(slmeta)
  # data(drivers)
  # dr <- drivers[,-1]
  # load('./data/clMed.RData')
  # # samp <- 100:200
  # prop = .05
  # clSub <- ceiling(table(clMed) * prop)
  # samp <- vector('list', length(clSub))
  # for(j in 1:length(clSub)) samp[[j]] <- sample(x = which(clMed == j), size = clSub[j])
  # samp <- unlist(samp)
  # comm <- dr[samp, ]
  # group <- clMed[samp]
  #
  # system.time(x <- simperMan(comm, group))

  # Transform community data as matrix
  comm <- as.matrix(comm)

  ## Identify groups
  comp <- t(combn(as.character(unique(group)), 2))
  nG <- nrow(comp)

  ## Data averages by group
  spavg <- apply(comm, 2, function(x) tapply(x, group, mean))

  # Data frames to store results
  nC <- ncol(comm)
  # sp <- colnames(comm)
  outlist <- vector('list', nG)
  names(outlist) <- apply(comp, 1, paste, collapse="_")
  for(i in seq_len(nG)) {
    outlist[[i]] <- data.frame(contr = numeric(nC),
                               sd = numeric(nC),
                               ava = spavg[as.numeric(comp[i, 1]), ],
                               avb = spavg[as.numeric(comp[i, 2]), ],
                               stringsAsFactors = F)
  }

  ## Function to match constrasts
  contrmatch <- function(X, Y, patt) X != Y & X %in% patt & Y %in% patt

  # Measure mean dissimilarity contribution per grouping
  for(j in seq_len(nG)) {
    # ID of group in comparison[j]
    id <- which(group %in% as.numeric(comp[j, ]))

    # Contrast of pairwise group comparison and extract ID of inter-group comparisons
    contrast <- outer(group[id], group[id], FUN=contrmatch, patt=comp[j,])
    contrast <- contrast[lower.tri(contrast)]

    for(i in seq_len(nC)) {
      # Similarity for each column and data of group in comparison[j]
      sim <- dist(comm[id,i,drop=FALSE], method="manhattan")
      sim <- as.vector(sim)

      # Extract mean and sd contribution to inter-group dissimilarity
      outlist[[j]]$contr[i] <- mean(sim[contrast,drop=FALSE])
      outlist[[j]]$sd[i] <- sd(sim[contrast,drop=FALSE])
    }
  }
  return(outlist)
}


#' Function to run an iterative SIMPER analysis 
#'
#' @export

# iter <- 10
# prop <- .02 # .05: ~500s/iter | .01: ~20s/iter | .02: ~80s/iter
# The analysis being very long, we will proceed using a
# bootstrap procedure, i.e. iterative simper analysis
bootstrapSimper <- function(x, cl, iter = 100, prop = .1, accr = '') {
  # List to store simper results for post-processing
  bootSimper <- vector('list', iter)

  # Determine the number of data points to sample from each cluster
    clSub <- ceiling(table(cl) * prop)

  # Repeat the process for the desired number of iterations
  for(i in 1:iter) {
    cat("   i: ", i, ' of ', iter, "\r")

    # Subsampled dataset
    samp <- vector('list', length(clSub))
    for(j in 1:length(clSub)) samp[[j]] <- sample(x = which(cl == j), size = clSub[j])
    samp <- unlist(samp)

    # Run simper analysis on subset of data
    bootSimper[[i]] <- simperMan(x[samp, ], cl[samp])

    # Save file
    save(bootSimper, file = paste0('./data/bootSimper', accr, '.RData'))
  }
  return(bootSimper)
}
