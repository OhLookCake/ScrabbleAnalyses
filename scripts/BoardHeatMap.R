setwd("C:/Projects/Data/_Ongoing/Scrabble")

source("Scrabble.R")
gcgFileName <- "example.gcg"
gcg <- readLines(gcgFileName)

board <- MakeBoard(gcg)


