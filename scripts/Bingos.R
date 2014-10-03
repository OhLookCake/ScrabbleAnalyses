library(Hmisc)

source("scripts/Scrabble.R")
CountBingos <- function(gcg){
  
  #Clean up comments, meta data
  gcg <- gcg[-c(grep("^#", gcg), grep("^[\\ ]*$", gcg))]
  gcg <- gcg[grep("^>", gcg)]
  gcg <- gsub("#.*$", "", gcg)
  
  cleanGcg <- gsub("^[[:alnum:][:punct:]]*\\:\\ [A-Z\\?]*[\\ ]*", "", gcg)
  
  #if No game details found, return NULL
  if(length(cleanGcg)==0) { return(NULL) } # this statement need to be done twice because CategorizeMoves() also throws an error is empty
  
  players <- unique(gsub("\\ .+$", "", gcg))
    
  #Use only valid moves
  types <- CategorizeMoves(cleanGcg)
  gcg <- gcg[types=="h" | types=="v"]
  #types <- types[types=="h" | types=="v"]
  
  if(length(gcg)==0) { return(NULL) }
  bingoCount <- c(0,0)
  
  for(move in gcg){
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
  cat(gcgFileName) 
  gcgFileName <- paste("data", gcgFileName, sep ="/")
  
  gcg <- readLines(gcgFileName)
  gcg <- iconv(gcg, to="ASCII//TRANSLIT")
  bingoCounter <- rbind(bingoCounter, sort(CountBingos(gcg)))
  cat(gcgFileName, "\n") 
  inc(ctr) <- 1
})
}

cor(bingoCounter)

