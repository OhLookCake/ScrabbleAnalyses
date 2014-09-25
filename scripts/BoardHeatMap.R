
library(reshape2)
library(ggplot2)
library(RColorBrewer)

setwd("C:/Projects/Data/_Ongoing/Scrabble")
source("scripts/Scrabble.R")

#Initialize empty board
countBoard <- matrix(data=0, nrow=15, ncol=15)


ctr=0
for(gcgFileName in list.files("data","anno.*\\.gcg")){
  ctr=ctr+1
  cat(" ", ctr," ")
  gcgFileName <- paste("data", gcgFileName, sep ="/")
  gcg <- readLines(gcgFileName)
  
  board <- MakeBoard(gcg)
  countBoard <- countBoard + as.numeric(board!=".")
  cat(gcgFileName, "\n")
}

countBoard.long <- melt(countBoard, varnames=c("y","x"), value.name="count")

boardHeatMap <- ggplot(countBoard.long,
                       aes(x = LETTERS[x], y = 16-y, fill = count)) +
                       geom_tile()
boardHeatMap <- boardHeatMap +
                  coord_equal() +
                  scale_x_discrete(expand = c(0, 0)) +
                  scale_y_reverse(breaks=1:15, expand = c(0,0)) +
                  xlab("") + ylab ("")

boardHeatMap + scale_fill_gradient(low = "#FFFFFF", high = "#AA0000", space = "Lab")
spectralPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
boardHeatMap + scale_fill_gradientn(colours = spectralPalette(100))




