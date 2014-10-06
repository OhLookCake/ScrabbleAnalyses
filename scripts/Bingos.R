library(Hmisc)

source("scripts/Scrabble.R")
CountBingos <- function(gcg){
  
  dfMoves <- PlaysList(gcg)
  players <- unique(dfMoves$Player)
  bingoCount <- c(0,0)
  
  for(i in 1:nrow(dfMoves)){
    player <- gsub("\\ .+$", "", move)
    word <- gsub("\\ .*$", "", 
                 gsub("^[[:alnum:][:punct:]]*\\ [A-Z\\?]*\\ [0-9A-Oa-o]*\\ ", "", move)
    )
    numLettersUsed <- nchar(gsub("\\.", "", word))
    if(numLettersUsed == 7){
      inc(bingoCount[which(players==player)]) <- 1
    }
    
  }
  
  return(bingoCount)
  
}



allFiles <- list.files("data","anno.*\\.gcg")

bingoCounter <- NULL

ctr <- 1
sampleFraction <- 0.1 # If we want to use only a fraction of the boards

for(gcgFileName in sample(allFiles, length(allFiles) * sampleFraction, replace=FALSE)){
try(
{
  cat(" ", ctr," ")
  cat(gcgFileName, "\n") 
  gcgFileName <- paste("data", gcgFileName, sep ="/")
  
  gcg <- readLines(gcgFileName)
  gcg <- iconv(gcg, to="ASCII//TRANSLIT")
  bingoCounter <- rbind(bingoCounter, sort(CountBingos(gcg)))
  
  inc(ctr) <- 1
})
}

cor(bingoCounter)

