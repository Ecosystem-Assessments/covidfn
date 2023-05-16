#' Butterfly graph
#'
#' @export

butterfly <- function(x1, x2) {
  # Gap
  gap <- .25
  yG <- .15

  # Combine with drNames
  # This makes the function unusable for another setting, but simply removing it would make it ok
  x1 <- data.frame(x = x1, names = colnames(dat), group = "1", stringsAsFactors = T)
  x2 <- data.frame(x = x2, names = colnames(dat), group = "1", stringsAsFactors = T)
  # x1 <- data.frame(x = x1, names = drNames$accr, group = drNames$group, stringsAsFactors = T)
  # x2 <- data.frame(x = x2, names = drNames$accr, group = drNames$group, stringsAsFactors = T)

  # Format data
  x1$x <- -x1$x - gap
  x1 <- x1[order(x1$group), ]
  x1$col <- as.numeric(x1$group)
  x1 <- x1[nrow(x1):1, ]

  x2$x <- x2$x + gap
  x2 <- x2[order(x2$group), ]
  x2$col <- as.numeric(x2$group)
  x2 <- x2[nrow(x2):1, ]

  # Colors
  cols <- gg_color_hue(nGroup)
  x1$col <- cols[x1$col]
  x2$col <- cols[x2$col]

  # Plot
  library(graphicsutils)
  par(mar = c(0,0,0,0))
  plot0(xlim = c(-1.25, 1.25), ylim = c(0,length(x1$x)+1))
  axis(1, at = seq(-gap, -1-gap, by = -.25), labels = c('0','.25','.5','.75','1'), lwd = 1, font = 2, cex.axis = .7, tck = -.005, mgp = c(1, 0, 0))
  axis(1, at = seq(gap, 1+gap, by = .25), labels = c('0','.25','.5','.75','1'), lwd = 1, font = 2, cex.axis = .7, tck = -.005, mgp = c(1, 0, 0))
  axis(1, at = seq(-gap, gap, by = 2*gap), labels = c('',''), lwd = 1, tck = -.005)
  text(x = rep(0, length(x1$x)), y = 1:(length(x1$x)), labels = x1$names, cex = .7)
  for(i in c(seq(-gap, -1-gap, by = -gap), seq(gap, 1+gap, by = gap))) {
    lines(x = c(i,i), y = c(.5,length(x1$x)+1.5), lty = 'dotted', col = '#00000066')
  }

  # Add bar plot
  for(i in 1:22) {
    rect(xleft = x1$x[i], ybottom = i-yG, xright = -gap, ytop = i+yG, col = x1$col[i], border = x1$col[i])
    rect(xleft = x2$x[i], ybottom = i-yG, xright = gap, ytop = i+yG, col = x2$col[i], border = x2$col[i])
  }
}

#' Circular plots
#'
#' @export

circularPlot <- function(data, center) {
  # library
  library(tidyverse)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=3
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)
  data=data %>% arrange(group)
  data$id=seq(1, nrow(data))

  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)

  # prepare a data frame for base lines
  base_data=data %>%
    group_by(group) %>%
    summarize(start=min(id), end=max(id) - empty_bar) %>%
    rowwise() %>%
    mutate(title=mean(c(start, end)))

  # prepare a data frame for grid (scales)
  grid_data = base_data
  grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start = grid_data$start - 1
  grid_data=grid_data[-1,]

  # Make the plot
  p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +

    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +

    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("0.2", "0.4", "0.6", "0.8") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +

    geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
    ylim(-15,60) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-.25,4), "cm")
    ) +
    coord_polar() +
    geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, inherit.aes = FALSE ) +

    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.3 , inherit.aes = FALSE ) +
    # geom_text(data=base_data, aes(x = title, y = -18, label=1), hjust=c(.5), colour = "black", alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE)
    annotate("text", x = 0, y = -15, label = center, color="#4f4f4f99", size=2 , angle=0, fontface="bold", hjust=.5)

  p
}
