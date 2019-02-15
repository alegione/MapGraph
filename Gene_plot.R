library(tidyverse)

path <- "D:\\Documents/PhD/Sequences/MinION/NanostatWTP/MiniMap/"
folder <- "CbotC"

titles <- basename(Sys.glob(file.path("D:\\Documents/PhD/Sequences/MinION/NanostatWTP/MiniMap/", folder, "*.cov.txt")))

drawplot <- function(target = x){
  print(paste("reading file", target))
  fileinput <- paste0(path, folder, "/", target)
  
  dat <- read.delim(file = fileinput,
                  header = FALSE, col.names = c("Gene", "Position", "Depth"))
  DepthHigh <- max(dat$Depth)

  if ( DepthHigh >= 5) {
    sort(as.character(dat$Gene))
    dat$Base <- seq(from = 1, to = nrow(dat), by = 1)
    genomeLength <- max(dat$Base)
    print(paste("Max depth is", DepthHigh, ". Drawing plots for", genomeLength, "bp genome"))
    
    plottitle <- paste((strsplit(target, "[.]")[[1]][1]), as.character(head(dat$Gene, 1)))
  
    ggplot(data = dat) +
      geom_line(mapping = aes(x = Base, y = Depth)) + 
      theme_bw() +
      scale_x_continuous(limits = c(1, genomeLength), breaks = c(seq(from = 1, to = genomeLength, by = genomeLength/10))) + 
      ggtitle(label = plottitle) +
      
      NULL
    
    ggsave(filename = paste0(plottitle, ".png"),
           device = "png",
           path = paste0(path, folder, "/"),
           dpi = 300)
    
  } else {
    print(paste("Max depth is ", DepthHigh, ". Skipping plot drawing"))
  }
}


lapply(titles, drawplot)
