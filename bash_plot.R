#!/home/genomics/.linuxbrew/bin/R

# load required libraries
suppressMessages(library(tidyverse))

#Allow command line arguments
args = commandArgs(trailingOnly=TRUE)

# Rscript "$ProjectDir/$i/" "$i.$j.cov.txt" "$ProjectName" "$RefType" "$i" "$j"
# Rscript "$ProjectDir/" "$i.cov.txt" "$ProjectName" "$RefType" "$i"

varSWD <- args[1]		#current working directory
varfilename <- args[2]	#depth file in .txt format
ProjectName <- args[3] #parse reference genome name from input
reftype <- args[4]
readfileName <- args[5]

if ( reftype == "Database" ) {
  geneName <- args[6]
}

setwd(varSWD)
roundUp <- function(x) { 10^ceiling(log10(x)) }

print(paste("reading file", varfilename))

dat <- read.delim(file = varfilename,
                  header = FALSE, col.names = c("Gene", "Position", "Depth"))

DepthHigh <- max(dat$Depth)

if ( DepthHigh >= 5) {
  dat <- arrange(dat, Gene)
  dat$Base <- seq(from = 1, to = nrow(dat), by = 1)
  genomeLength <- max(dat$Base)
  print(paste("Max depth is", DepthHigh, ". Drawing plots for", genomeLength, "bp genome"))
  
  if ( reftype == "Database" ) {
    plottitle <- paste0(readfileName, "-" , geneName)
    } else {
    plottitle <- paste0(ProjectName, "-" , readfileName)
  }

  
  ggplot(data = dat) +
    geom_line(mapping = aes(x = Base, y = Depth)) + 
    theme_bw() +
#    scale_y_log10(limits = c(1, roundUp(DepthHigh))) +
    scale_x_continuous(limits = c(1, genomeLength), breaks = c(seq(from = 1, to = genomeLength, by = genomeLength/10))) + 
    ggtitle(label = plottitle) +
    
    NULL
  
  ggsave(filename = paste0(plottitle, ".png"),
         device = "png",
         dpi = 300)
  
} else {
  print(paste("Max depth is ", DepthHigh, ". Skipping plot drawing"))
}

