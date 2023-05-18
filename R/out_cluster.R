#' Script to run a cluster analysis
#'
#' @export
out_cluster <- function() {
  # Load and prepare data
  # dat <- vroom::vroom("output/fn_extract/first_nations_location-ce594316.csv")[,-1]
  dat <- vroom::vroom("output/fn_extract/canadian_geographical_names-92230392.csv")[,-1]
  
  # Change names 
  colnames(dat) <- tools::file_path_sans_ext(colnames(dat))
  
  # Select variables (not efficient or clean, but it works)
  nm <- c(
    "canadian_vulnerabilities_census",
    "census_road_network_file_2021-distance_to_road_network",
    "housing_acceptability_canada",
    "open_database_healthcare_facilities"
  ) |>
  paste(collapse = "|")
  uid <- stringr::str_detect(colnames(dat), nm)
  
  # Covid period
  cases <- stringr::str_detect(colnames(dat), "cases")
  deaths <- stringr::str_detect(colnames(dat), "deaths")
  per1 <- stringr::str_detect(colnames(dat), "2021_10")
  per2 <- stringr::str_detect(colnames(dat), "2022_03")
  cid <- colnames(dat) == 'covid_timeline_canada-a56e753b-cases_population_proportion'
  did <- colnames(dat) == 'covid_timeline_canada-a56e753b-deaths_population_proportion'
  dat[, cid] <- rowSums(dat[,which(cases & per1):which(cases & per2)], na.rm = TRUE)
  dat[, did] <- rowSums(dat[,which(deaths & per1):which(deaths & per2)], na.rm = TRUE)

  # Final column filter
  dat <- dat[, uid | cid | did]

  # Transform as matrix and scale 
  dat <- as.matrix(dat) |>
         scale()

  # ~~~~~~~~~~~~~~~~~~~ k-medoids clustering ~~~~~~~~~~~~~~~~~~~ #
  out <- here::here("output","clustering")
  pipedat::chk_create(out)
  exist <- file.exists(here::here(out, "kmedoid.RData"))

  if (!exist) {
    library(cluster)
    # Cluster analysis
    nSample <- 100
    sampleSize <- ceiling(nrow(dat) * .2) # 10000
    k <- 2:10
    kmedoid <- list()
    for(i in k) {
      cat("   k: ", i, "\r")
      kmedoid[[i]] <- clara(
        dat,
        k = i,
        sampsize = sampleSize,
        samples = nSample,
        metric="manhattan",
        stand = TRUE,
        keep.data = FALSE,
        correct.d = TRUE
      )
      save(kmedoid, file = here::here(out, "kmedoid.RData"))
    }
    kmedoid[[1]] <- NULL
    save(kmedoid, file = here::here(out, "kmedoid.RData"))

    # ~~~~~~~~~~~~~~~~~~~ Average silhouette width ~~~~~~~~~~~~~~~~~~~ #
    aswMedoid <- unlist(lapply(kmedoid, function(x) x$silinfo$avg.width))
    save(aswMedoid, file = here::here(out, "aswMedoid.RData"))
    
    # ~~~~~~~~~~~~~~~~~~~ Elbow method ~~~~~~~~~~~~~~~~~~~ #
    # Verify number of clusters by using the elbow method,
    # i.e. the inflexion point in the sum of squared error (SSE) scree plot
    # Extract within cluster sum of squared error (WSS) from each clustering run
    # Wss function
    wss <- function(x) (nrow(x)-1) * sum(apply(x,2,var), na.rm = TRUE)

    # Measure for each k and each cluster
    wssMedoid <- wss(dat)
    for(i in 1:length(kmedoid)) { # For each k value tested
      wssTemp <- numeric()
      for(j in 1:k[i]) { # For each cluster
        id <- kmedoid[[i]]$clustering == j # Identify observations in cluster j
        wssTemp[j] <- wss(dat[id, ])
      }
      wssMedoid[k[i]] <- sum(wssTemp)
    }
    save(wssMedoid, file = here::here(out,"wssMedoid.RData"))
  }

  # ~~~~~~~~~~~~~~~~~~~ cluster validation ~~~~~~~~~~~~~~~~~~~ #

  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  #
  # Clusters are validated visually using the Average silhouette width and the
  # Elbow method
  #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
  outfig <- here::here("figures","clustering")
  pipedat::chk_create(outfig)
  exist <- file.exists(here::here(outfig, "validation.jpg"))
  if (!exist) {
    k <- 2:10

    # ~~~~~~~~~~~~~~~~~~~ Data ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out,'aswMedoid.RData'))
    load(here::here(out,'wssMedoid.RData'))
    
    # ~~~~~~~~~~~~~~~~~~~ Figure ~~~~~~~~~~~~~~~~~~~ #
    jpeg(here::here(outfig,'validation.jpg'), width = 920, height = 920, res = 200, pointsize = 7)
    layout(matrix(1:2, ncol = 1))
    par(mar = c(5,5,2,1))
    # k-medoid
      # Silhouette
      plot(aswMedoid ~ k,
           pch = 20,
           ylab = 'Average Silhouette Width',
           type = 'b',
           frame = F,
           xlab = '',
           xlim = c(0,10),
           cex = 1.5,
           cex.axis = .8)
      mtext(side = 3, 'k-medoids clustering', font = 2, line = .75)

      # WSS
      plot(x = c(1,k),
           y = wssMedoid,
           type="b",
           frame = F,
           xlab="Number of Clusters (k)",
           ylab="Total within-clusters sum of squares",
           pch = 20,
           xlim = c(0,10),
           cex = 1.5,
           cex.axis = .8)
    dev.off()  
  }

  # ~~~~~~~~~~~~~~~~~~~ select cluster ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(out, "clusters.RData"))
  if (!exist) {
    # Number of clusters selected
    # WARNING: This is selected and adjusted manually
    k <- 2

    # Object with only selected clustering results
    clMed <- kmedoid[[k-1]]$clustering

    # Export simplified dataset
    save(k, file = here::here(out, 'k.RData'))
    save(clMed, file = here::here(out, 'clusters.RData'))
  }



  # ~~~~~~~~~~~~~~~~~~~ select cluster ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(out, 'simperMean.RData'))
  if (!exist) {
    # ~~~~~~~~~~~~~~~~~~~ DATA ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out, 'k.RData'))
    load(here::here(out, 'Clusters.RData'))

    # ~~~~~~~~~~~~~~~~~~~ INTER-CLUSTER SIMILARITY ~~~~~~~~~~~~~~~~~~~ #
    # ~~~~~~~~~~~~~~~~~~~ SIMPER ~~~~~~~~~~~~~~~~~~~ #
    # Run iterative simper analysis
    iter <- 300
    prop <- .05
    system.time(bootSimper <- bootstrapSimper(dat, clMed, iter = iter, prop = prop))
    save(bootSimper, file = here::here(out, 'bootSimper.RData'))

    # ~~~~~~~~~~~~~~~~~~~ SIMPER SUMMARY ~~~~~~~~~~~~~~~~~~~ #
    # Summarize in array format
    iter <- length(bootSimper) # To delete
    nList <- length(bootSimper[[1]])

    # Name of variables to extract from simper analyses
    varNames <- c('contr','sd','ava','avb')

    # Empty list to store summaries
    simperSummary <- vector('list', nList)
    names(simperSummary) <- names(bootSimper[[1]])

    # Empty arrays to store inter-cluster dissimilarity results
    for(i in 1:nList) {
      simperSummary[[i]] <- array(data = 0,
                                  dim = c(ncol(dat), length(varNames), iter),
                                  dimnames = list(colnames(dat), varNames))
    }

    # Extract summary for simper analyses interations
    for(i in 1:iter) {
      temp <- bootSimper[[i]]
      for(j in 1:nList) {
        simperSummary[[j]][,,i] <- as.matrix(temp[[j]][, varNames])
      }
    }

    # Summarize the results over all iterations
    simperMean <- vector('list', nList)
    names(simperMean) <- names(simperSummary)
    for(i in 1:nList) {
      simperMean[[i]] <- apply(X = simperSummary[[i]],
                               MARGIN = c(1,2),
                               FUN = mean,
                               na.rm = T)
    }

    # Export
    save(simperMean, file = here::here(out, 'simperMean.RData'))
    
    # ~~~~~~~~~~~~~~~~~~~ INTRA-CLUSTER SIMILARITY ~~~~~~~~~~~~~~~~~~~ #
    # As with the simper analysis, the dataset is mostly too large to use
    # regular function in packages available in R. Also, while the software
    # PRIMER automatically measures the intra-cluster similarity using the
    # Bray-Curtis similarity, `vegan::simper` does not do it.
    # I therefore create a custom function that allows me to extract the
    # intra-cluster Bray-Curtis similarity and that does it using a bootstrap
    # procedure when the cluster has too many observations.
    #
    sim <- vector('list', k)
    for(i in 1:k) {
      cat("   k: ", i, "\r")
      id <- clMed == i
      sim[[i]] <- bootIntraSim(dat[id, ], iter = 50, samp = .25)
      save(sim, file = here::here(out, 'intraSimilarity.RData'))
    }

    # Extract species, contribution and sd
    # Summarize in array format
    iter <- length(sim[[4]])
    varNames <- c('contr')
    simSummary <- vector('list', k)
    for(i in 1:k) simSummary[[i]] <- array(data = 0, dim = c(ncol(dat), 1, iter), dimnames = list(colnames(dat), varNames))

    for(i in 1:k) {
      # Check if there are multiple iterations or not.
      # If length(sim[[i]]) == 2 it means that there were no iterations and the whole cluster was parsed through the simper2 function
      # If not, then sim[[i]] should be equal to the number of iterations
      if(i %in% c(1,2,3)) {
        simSummary[[i]] <- matrix(ncol = 1,
                                  data = sim[[i]]$contr,
                                  dimnames = list(colnames(dat), 'contr'))
      } else {
        for(j in 1:iter) {
          simSummary[[i]][,'contr',j] <- sim[[i]][[j]]#$contr
        }
      }
    }

    # Summarize the results over all iterations
    similarityMean <- vector('list', k)
    for(i in 1:k) similarityMean[[i]] <- apply(X = simSummary[[i]], MARGIN = c(1,2), FUN = mean, na.rm = T)

    # Export
    save(similarityMean, file = here::here(out, 'similarityMean.RData'))
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~ Dissimilarity Contribution ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(outfig, 'interDissimilarity.jpg'))
  if (!exist) {
    # ~~~~~~~~~~~~~~~~~~~ EXTRA LIBRARIES ~~~~~~~~~~~~~~~~~~~ #
    library(gridExtra)
    library(ggplot2)
    library(ggplotify)
    library(grid)


    # ~~~~~~~~~~~~~~~~~~~ DATA ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out, 'k.RData'))
    load(here::here(out, 'Clusters.RData'))
    load(here::here(out, 'simperMean.RData'))

    # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL CONTRIBUTION TO INTER-CLUSTER DISSIMILARITY ~~~~~~~~~~~~~~~~~~~ #
    # Driver percent contribution to dissimilarity
    contDiss <- lapply(simperMean, function(x) x[,'contr'] / sum(x[,'contr'], na.rm = TRUE)) |>
                as.data.frame()
    colnames(contDiss) <- names(simperMean)
    contDiss[is.na(contDiss)] <- 0

    # Total dissimilarity
    totDiss <- unlist(lapply(simperMean, function(x) sum(x[, 'contr'], na.rm = TRUE)))

    # Driver intensity in compared clusters
    drInt1 <- lapply(simperMean, function(x) x[,'ava']) |>
              as.data.frame()
    drInt2 <- lapply(simperMean, function(x) x[,'avb']) |>
              as.data.frame()

    # Mean value in each cluster
    totMean <- numeric(k)
    for(i in 1:k) {
      id <- clMed == i
      totMean[i] <- mean(as.matrix(dat[id, ]), na.rm = TRUE)
    }

    # Set figure layout view
    mat <- matrix(0,k,k)
    mat[lower.tri(mat)] <- (max(mat)+1):(sum(lower.tri(mat)) + max(mat))
    diag(mat) <- (max(mat)+1):(max(mat)+k)
    mat <- t(mat)
    mat[lower.tri(mat)] <- (max(mat)+1):length(mat)
    mat <- t(mat)
    mat <- rbind(mat, c(100,100,rep(max(mat)+1, 5),100,100))

    # Circulat plots
    p1 <- vector('list', ncol(contDiss))
    for(i in 1:length(contDiss)) {
      temp <- data.frame(individual = colnames(dat),
                         group = 1:ncol(dat),
                         value = round(contDiss[, i]*100))
      rownames(temp) <- NULL
      temp2 <- round(totDiss[i], 2)
      p1[[i]] <- circularPlot(temp, temp2)
    }

    # Individual clusters
    spat <- sf::st_read("output/fn_extract/first_nations_location-ce594316.gpkg", quiet = TRUE)
    can <- sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)
    p2 <- vector('list', k)
    for(i in 1:length(p2)) {
      # Identify observations in cluster i
      id <- clMed == i
    
      # Select only grid cells in cluster i
      temp <- spat |>
              dplyr::filter(clMed == i) |>
              sf::st_coordinates() |>
              as.data.frame()
    
      ext <- sf::st_bbox(can)
    
      p2[[i]] <- ggplot() +
        geom_sf(data = can, fill = NA) +
        geom_point(aes(x = X, y = Y), colour = '#187962', size = .01, data = temp) +
        coord_sf(crs = sf::st_crs(can), datum = NA) +
        annotate(
          "text",
          x = ext$xmin + diff(c(ext$xmin, ext$xmax))*.2,
          y = ext$ymax - diff(c(ext$ymin, ext$ymax))*.05,
          label = paste0('Cluster ', i),
          color="black",alpha=0.6, size=5
        ) +
        theme(
          panel.ontop = TRUE, ## Note: this is to make the panel grid visible in this example
          panel.grid = element_blank(),
          line = element_blank(),
          rect = element_blank(),
          text = element_blank(),
          plot.background = element_rect(fill = "transparent")
        )
    }
    
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
    }
    nGroup = 1
    
    # Driver intensity per cluster
    p3 <- vector('list', ncol(drInt1))
    for(i in 1:ncol(drInt1)) {
      p3[[i]] <- as.ggplot(~butterfly(drInt1[, i], drInt2[, i]))
    }

    # Group legend
    grLegend <-function() {
      xDiff <- 1/nGroup
      xGap <- .075
      cols <- gg_color_hue(nGroup)[1:nGroup]
      plot0(xlim = c(0,1), ylim = c(0,1))
      for(i in 1:nGroup) {
        x1 <- (i*xDiff) - xGap - xDiff/2
        x2 <- (i*xDiff) + xGap - xDiff/2
        y1 <- .25
        y2 <- .75
        polygon(x = c(x1, x2, x2, x1, x1),
                y = c(y1, y1, y2, y2, y1),
                col = cols[i],
                border = '#000000')
        text(x = (i*xDiff) - xDiff/2,
             y = 0,
             labels = "Nope",
             # labels = grNames$name[i],
             adj = c(.5,.5),
             font = 2)
      }
    }

    p4 <- list(as.ggplot(~grLegend()))

    # List of all plots
    p <- c(p1,p2,p3,p4)
    # p <- c(p1,p3,p4)

    # Full plot
    jpeg(
      here::here(outfig, 'interDissimilarity.jpg'), 
      width = 3500, 
      height = 3500, 
      res = 200, 
      pointsize = 7
    )
    grid.arrange(
      grobs = p,
      layout_matrix = mat,
      heights = c(1,1,1,1,1,1,1,1,1,.5),
      right = textGrob(
        'Driver intensity', 
        gp=gpar(fontsize=20,fontface='bold'), 
        rot = -90
      ),
      left = textGrob(
        'Cumulative dissimilarity contribution', 
        gp=gpar(fontsize=20,fontface='bold'), 
        rot = 90
      ),
      padding = unit(2.5, "line")
    )
    dev.off()  
  }
  
  
  # ~~~~~~~~~~~~~~~~~~~ Dissimilarity Contribution ~~~~~~~~~~~~~~~~~~~ #
  exist <- file.exists(here::here(outfig, 'intraSimilarity.jpg'))
  if (!exist) {
    # ~~~~~~~~~~~~~~~~~~~ EXTRA LIBRARIES ~~~~~~~~~~~~~~~~~~~ #
    library(gridExtra)
    library(ggplot2)
    library(ggplotify)
    library(grid)
    library(graphicsutils)

    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100, alpha = .5)[1:n]
    }
    nGroup = ncol(dat)


    # ~~~~~~~~~~~~~~~~~~~ DATA ~~~~~~~~~~~~~~~~~~~ #
    load(here::here(out, 'k.RData'))
    load(here::here(out, 'Clusters.RData'))
    load(here::here(out, 'similarityMean.RData'))

    # ~~~~~~~~~~~~~~~~~~~ SIMILARITY CONTRIBUTION ~~~~~~~~~~~~~~~~~~~ #
    # Driver percent contribution to dissimilarity
    contSim <- lapply(similarityMean, function(x) x[,'contr'] / sum(x[,'contr'], na.rm = TRUE)) |>
                as.data.frame()
    colnames(contSim) <- names(similarityMean)

    # Total similarity
    totSim <- lapply(similarityMean, function(x) sum(x[,'contr'], na.rm = TRUE)) |>
              as.data.frame()

    # Circulat plots
    p1 <- vector('list', ncol(contSim))
    for(i in 1:length(contSim)) {
      temp <- data.frame(individual = colnames(dat),
                         group = 1:ncol(dat),
                         value = round(contSim[, i]*100))
      rownames(temp) <- NULL
      temp2 <- round(totSim[i],2)
      p1[[i]] <- circularPlot(temp, temp2)
    }

    spat <- sf::st_read("output/fn_extract/first_nations_location-ce594316.gpkg", quiet = TRUE)
    can <- sf::st_read("data/basemap/canada_full.gpkg", quiet = TRUE)
    p2 <- vector('list', k)
    for(i in 1:length(p2)) {
      # Identify observations in cluster i
      id <- clMed == i
    
      # Select only grid cells in cluster i
      temp <- spat |>
              dplyr::filter(clMed == i) |>
              sf::st_coordinates() |>
              as.data.frame()
    
      ext <- sf::st_bbox(can)
    
      p2[[i]] <- ggplot() +
        geom_sf(data = can, fill = NA) +
        geom_point(aes(x = X, y = Y), colour = '#187962', size = .01, data = temp) +
        coord_sf(crs = sf::st_crs(can), datum = NA) +
        annotate(
          "text",
          x = ext$xmin + diff(c(ext$xmin, ext$xmax))*.2,
          y = ext$ymax - diff(c(ext$ymin, ext$ymax))*.05,
          label = paste0('Cluster ', i),
          color="black",alpha=0.6, size=5
        ) +
        theme(
          panel.ontop = TRUE, ## Note: this is to make the panel grid visible in this example
          panel.grid = element_blank(),
          line = element_blank(),
          rect = element_blank(),
          text = element_blank(),
          plot.background = element_rect(fill = "transparent")
        )
    }

    # Group legend
    grLegend <-function() {
      xDiff <- 1/nGroup
      xGap <- .075
      cols <- gg_color_hue(nGroup)[1:nGroup]
      plot0(xlim = c(0,1), ylim = c(0,1))
      for(i in 1:nGroup) {
        x1 <- (i*xDiff) - xGap - xDiff/2
        x2 <- (i*xDiff) + xGap - xDiff/2
        y1 <- .25
        y2 <- .75
        polygon(x = c(x1, x2, x2, x1, x1),
                y = c(y1, y1, y2, y2, y1),
                col = cols[i],
                border = '#000000')
        text(x = (i*xDiff) - xDiff/2,
             y = 0,
             labels = 'Nope',
             # labels = grNames$name[i],
             adj = c(.5,.5),
             font = 2)
      }
    }

    p3 <- list(as.ggplot(~grLegend()))


    # Layout view
    # mat <- matrix(nrow = 4, data = c(1,3,5,NA,8,10,12,7,2,4,6,14,9,11,13,NA))
    mat1 <- matrix(nrow = 3, data = 1:9)
    mat2 <- matrix(nrow = 3, data = 10:18)
    mat <- cbind(mat1[,1], mat2[,1],mat1[,2], mat2[,2],mat1[,3], mat2[,3])
    mat <- rbind(mat, c(NA,NA,19,19,NA,NA))

    # Figure
    p <- c(p2,p1,p3)
    jpeg(
      here::here(outfig,'intraSimilarity.jpg'), 
      width = 3000, 
      height = 2000, 
      res = 200, 
      pointsize = 7
    )
    grid.arrange(
      grobs = p,
      layout_matrix = mat,
      heights = c(1,1,1,.25),
      top = textGrob('Intra-cluster similarity', gp=gpar(fontsize=20,fontface='bold')),
      padding = unit(2.5, "line")
    )
    dev.off()
  }  
}

