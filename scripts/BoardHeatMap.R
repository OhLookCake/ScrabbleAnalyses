
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(Hmisc)

setwd("C:/Projects/Data/_Ongoing/Scrabble")

source("scripts/ParseFunctions.R")
source("scripts/BoardFunctions.R")

#Initialize empty board
sampleFraction <- 0.01
listdfMoves <- LoadBoards(sampleFraction = sampleFraction, derivative = T)

countBoard <- matrix(data=0, nrow=15, ncol=15)
ctr <- 0
for(dfMoves in listdfMoves){
  board <- MakeBoard(dfMoves)
  countBoard <- countBoard + as.numeric(board!=".")
  ctr <- ctr+1
  cat(" ", ctr, "\n")
}

countBoard.long <- melt(countBoard, varnames=c("y","x"), value.name="count")
countBoard.long$PercentGames <- countBoard.long$count*100/length(listdfMoves)


#Plot the board on screen
boardHeatMap <- ggplot(countBoard.long,
                       aes(x = LETTERS[x], y = 16-y, fill = PercentGames)) +
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
