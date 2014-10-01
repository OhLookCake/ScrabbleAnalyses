
library(reshape2)
library(ggplot2)
library(RColorBrewer)

setwd("C:/Projects/Data/_Ongoing/Scrabble")
source("scripts/Scrabble.R")

#Initialize empty board
countBoard <- matrix(data=0, nrow=15, ncol=15)

allFiles <- list.files("data","anno.*\\.gcg")

ctr <- 0
sampleFraction <- 1 # If we want to use only a fraction of the boards
for(gcgFileName in sample(allFiles, length(allFiles) * sampleFraction, replace=FALSE)){
    try(
    {
        cat(" ", ctr," ")
        gcgFileName <- paste("data", gcgFileName, sep ="/")
        gcg <- readLines(gcgFileName)
        
        board <- MakeBoard(gcg)
        countBoard <- countBoard + as.numeric(board!=".")
        cat(gcgFileName, "\n") 
        ctr <- ctr+1
    })
}

countBoard.long <- melt(countBoard, varnames=c("y","x"), value.name="count")


#Plot the board on screen
boardHeatMap <- ggplot(countBoard.long,
                       aes(x = LETTERS[x], y = 16-y, fill = count)) +
                       geom_tile()
boardHeatMap <- boardHeatMap +
                  coord_equal() +
                  scale_x_discrete(expand = c(0, 0)) +
                  scale_y_reverse(breaks=1:15, expand = c(0,0)) +
                  xlab("") + ylab ("")

spectralPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
boardHeatMap + scale_fill_gradientn(colours = spectralPalette(100))
boardHeatMap + scale_fill_gradient(low = "#FFFFFF", high = "#00AAAA", space = "Lab")


#Save to file
jpeg('output/HeatMap1.jpg',width = 860, height = 800)
  boardHeatMap + scale_fill_gradient(low = "#FFFFFF", high = "#00AAAA", space = "Lab")
dev.off()
jpeg('output/HeatMap2.jpg',width = 860, height = 800)
  boardHeatMap + scale_fill_gradientn(colours = spectralPalette(100))
dev.off()





