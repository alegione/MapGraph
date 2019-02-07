library(tidyverse)



dat <- read.delim(file = "D:\\Documents/PhD/Sequences/MinION/NanostatWTP/MiniMap/CARD/WTPv1_07_BC01.CARD/WTPv1_07_BC01.lnuC.cov.txt",
                  header = FALSE, col.names = c("Gene", "Position", "Depth"))

title <- as.character(head(dat$Gene, 1))

ggplot(data = dat) +
  geom_line(mapping = aes(x = Position, y = Depth)) + 
  ggtitle(label = title)
