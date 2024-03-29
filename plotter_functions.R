
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)


# Plot function for binary target and continuous variable

c_plot_numeric <- function(col, target, bins=10, bin_method='hist', xname="X Variable", yname="Target") {
  
  if(bin_method=='hist') {
    breaks <- seq(min(col), max(col), (max(col)-min(col))/bins)
    x_label <- paste(xname, "Bin", collapse=" ")
    y_label <- paste(yname, "Rate", collapse=" ")
  }
  else if(bin_method=='quant') {
    breaks <- unique(quantile(col, probs=seq(0, 1, 1/bins)))
    x_label <- paste(xname, "Quantile Bin", collapse=" ")
    y_label <- paste(yname, "Rate", collapse=" ")
  }
  else {
    print("Not a valid binning method ... please choose 'hist' or 'quant'")
    return(NA)
  }
  
  bin_col <- cut(as.numeric(col), breaks = breaks, include.lowest = TRUE, labels = FALSE)
  bin_labels <- levels(cut(as.numeric(col), breaks = breaks, include.lowest = TRUE))
  
  if(bins>30) {
    r = ceiling(bins/30)
    for(i in seq(r, length(bin_labels), r))  { bin_labels[i] <- "" }
  }
  
  df <- as.data.frame(cbind(bin_col, col, target))
  colnames(df) <- c("bins","col","target")
  
  sum <- df %>%
    group_by(bins) %>%
    summarise(count = n(),
              target = mean(target))
  
  scale_factor = max(sum$target) / max(sum$count)
  
  ggplot(data=sum, aes(x=bins)) +
    geom_bar(mapping = aes(y=count), stat='identity', col='blue3', fill='cornflowerblue') +
    geom_line(mapping = aes(y=target/scale_factor), col='red', size=2) +  
    geom_point(mapping = aes(y=target/scale_factor), size = 3, shape = 21, fill = "white") +
    scale_y_continuous(name = "Frequency Count", 
                       sec.axis = sec_axis(~.*scale_factor, 
                                           name=y_label)) +
    scale_x_continuous(name = x_label,
                       breaks = seq(1, bins, 1),
                       labels = bin_labels) +
    theme(axis.title = element_text(size=16),
          axis.title.y = element_text(color='blue3'),
          axis.title.y.right = element_text(color='red'),
          axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
          axis.text = element_text(size=12))
}


# Plot function for binary target and categorical variable

c_plot_factor <-function(col, target, xname="X Variable", yname="Target", show_max=-1)  {
  df2 <- cbind.data.frame(col, target)
  colnames(df2) <- c("col","target")
  
  sum <- df2 %>%
    group_by(col) %>%
    summarise(count = n(),
              target = mean(target))
  
  if(show_max>0){
    sum <- sum %>%
      arrange(desc(count)) %>%
      head(show_max)
    }
  
  sum$x <- seq(1, nrow(sum), 1)
  sum$x_start <- sum$x -.3
  sum$x_end <- sum$x + .3
  sum$label = str_wrap(sum$col, width=20)
  
  scale_factor = max(sum$target) / max(sum$count)
  
  y_label <- paste(yname, "Rate", collapse=" ")
  
  ggplot(data=sum, aes(x=x)) +
    geom_bar(mapping = aes(y=count), stat='identity', col='blue3', fill='cornflowerblue') +
    geom_segment(aes(x=x_start, xend=x_end, y=target/scale_factor, yend=target/scale_factor),
                 col='red', size=1) +
    geom_point(mapping = aes(y=target/scale_factor), size = 3, shape = 21, col='red', fill = "white") +
    scale_y_continuous(name = "Frequency Count", 
                       sec.axis = sec_axis(~.*scale_factor, 
                                           name = y_label)) +
    scale_x_continuous(breaks = sum$x, labels = sum$label, name = xname) +
    theme(axis.title = element_text(size=16),
          axis.title.y = element_text(color='blue3'),
          axis.title.y.right = element_text(color='red'),
          axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          axis.text = element_text(size=12))
}


# Plot function for continuous target and continuous variable

n_plot_numeric <- function(col, target, samp_size=1000, line=TRUE, x_log=FALSE, y_log=FALSE,
                           xname="X Variable", yname="Y Variable") {
  
  df <- as.data.frame(cbind(col, target))
  colnames(df) <- c("col","target")
  
  if(samp_size < nrow(df)) {
    df2 <- sample_n(df, samp_size)
  }
  else {
    df2 <- df
  }
  
  p <- ggplot(data=df2, aes(x=col, y=target)) +
    geom_point(alpha=.3) +
    theme(axis.title = element_text(size=16),
          axis.text = element_text(size=12))
  
  if(line==TRUE) {
    p <- p + geom_smooth(data=df, method='loess', color='red')
  }
  
  if(y_log==TRUE) {
    p <- p + scale_y_log10(name = yname)
  }
  else {
    p <- p + scale_y_continuous(name = yname)
  }
  
  if(x_log==TRUE) {
    p <- p + scale_x_log10(name = xname)
  }
  else {
    p <- p + scale_x_continuous(name = xname)
  }
  
  print(p)
}


# Plot function for continuous target and categorical variable

n_plot_factor <-function(col, target, xname="X Variable", yname="Y Variable", 
                         y_log=FALSE, show_max=20)  {
  
  df <- cbind.data.frame(col, target)
  colnames(df) <- c("xcol","target")
  
  keepers <- df %>%
    group_by(xcol) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>% 
    mutate(labels = str_wrap(xcol, width=20)) %>%
    head(show_max)
  
  df2 <- df %>%
    filter(xcol %in% keepers$xcol) %>%
    mutate(xcolnew = factor(as.character(xcol), levels=keepers$xcol))
  
  hist_rug <- ggplot(data=df2, aes(x=xcolnew)) +
    geom_bar() +
    theme_void() +
    theme(axis.line = element_line(colour = "black"))
  
  boxplot <- ggplot(data=df2, aes(x=xcolnew, y=target)) +
    geom_boxplot(col='blue3', fill='cornflowerblue') +
    labs(x=xname, y=yname) +
    scale_x_discrete(labels = keepers$labels) +
    theme(axis.title = element_text(size=16),
          axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          axis.text = element_text(size=12))
  
  plot_grid(boxplot, hist_rug, rel_heights=c(0.9, 0.1), ncol=1, align='v')
}
