
library(dplyr)
library(ggplot2)
library(stringr)

df <- read.csv("~/Desktop/us_census_income/census_train.csv")

# Plot function for numeric data

plot_numeric <- function(col, target, bins=10, bin_method='hist', xname="X Variable") {
  
  if(bin_method=='hist') {
    breaks <- seq(min(col), max(col), (max(col)-min(col))/bins)
    x_label <- paste(xname, "Bin", collapse=" ")
  }
  else if(bin_method=='quant') {
    breaks <- unique(quantile(col, probs=seq(0, 1, 1/bins)))
    x_label <- paste(xname, "Quantile Bin", collapse=" ")
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
                                           name="Target Rate")) +
    scale_x_continuous(name = x_label,
                       breaks = seq(1, bins, 1),
                       labels = bin_labels) +
    theme(axis.title = element_text(size=16),
          axis.title.y = element_text(color='blue3'),
          axis.title.y.right = element_text(color='red'),
          axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
          axis.text = element_text(size=12))
}


plot_numeric(df$age, df$income50k, bins=30)


plot_factor <-function(col, target, xname="X Variable", show_max=-1)  {
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
  
  print(sum)
  str(sum)
  
  ggplot(data=sum, aes(x=x)) +
    geom_bar(mapping = aes(y=count), stat='identity', col='blue3', fill='cornflowerblue') +
    geom_segment(aes(x=x_start, xend=x_end, y=target/scale_factor, yend=target/scale_factor),
                 col='red', size=1) +
    geom_point(mapping = aes(y=target/scale_factor), size = 3, shape = 21, col='red', fill = "white") +
    scale_y_continuous(name = "Frequency Count", 
                       sec.axis = sec_axis(~.*scale_factor, 
                                           name = "Target Rate")) +
    scale_x_continuous(breaks = sum$x, labels = sum$label) +
    theme(axis.title = element_text(size=16),
          axis.title.y = element_text(color='blue3'),
          axis.title.y.right = element_text(color='red'),
          axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          axis.text = element_text(size=12))
}


plot_factor(df$marital.status, df$income50k)
plot_factor(df$major.occupation.code, df$income50k, show_max = 5)
plot_factor(df$state.of.previous.residenc, df$income50k, show_max=10)

