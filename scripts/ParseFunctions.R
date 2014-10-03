library(plyr)

CategorizeMoves <- function(cleanGcg){
  types <- sapply(cleanGcg, function(move){
    type="unknown"  
    if(grepl("^-[A-Z\\?]+", move)) { type <- "exchange"}
    if(grepl("^-\\ ", move)) { type <- "pass"}
    if(grepl("^--", move)) { type <- "successfulchallenge"}
    if(grepl("^\\(challenge\\)", move)) { type <- "challengebonus"}
    if(grepl("^\\(time\\)", move)) { type <- "timepenalty"}
    if(grepl("^\\([A-Z\\?]\\)*", move)) { type <- "leave"}
    if(grepl("^[0-9]+[A-Oa-o]+", move)) { type <- "h"}
    if(grepl("^[A-Oa-o]+[0-9]+", move)) { type <- "v"}
    type
  })
  
  for(i in 1:length(types)){
    if(types[i]=="successfulchallenge") { types[i-1] <- "phoney" }
  }
  
  types
  
}



PlaysList <- function(gcg){
  
  #Clean up comments, meta data
  gcg <- gcg[-c(grep("^#", gcg), grep("^[\\ ]*$", gcg))]
  gcg <- gcg[grep("^>", gcg)]
  gcg <- gsub("#.*$", "", gcg)
  
  cleanGcg <- gsub("^[[:alnum:][:punct:]]*\\:\\ [A-Z\\?]*[\\ ]*", "", gcg)
  
  #if No game details found, return NULL
  if(length(cleanGcg)==0) { return(NULL) } # this statement need to be done twice because CategorizeMoves() also throws an error is empty
  
  #Use only valid moves
  types <- CategorizeMoves(cleanGcg)
  gcg <- gcg[types=="h" | types=="v"]
  types <- types[types=="h" | types=="v"]
  
  if(length(gcg)==0) { return(NULL) }
  
  dfMoves <- ldply(strsplit(gcg, split=" "))
  dfMoves <- cbind(dfMoves, types)
  rownames(dfMoves) <- NULL
  colnames(dfMoves) <- c("Player", "Rack", "Start", "Word", "Score", "Cumulative", "Direction")
  
  dfMoves$Player <- gsub("^>", "", gsub("\\:$","", dfMoves$Player))
  dfMoves$Score <- as.numeric(dfMoves$Score)
  dfMoves$Cumulative <- as.numeric(dfMoves$Cumulative)
  
  dfMoves$StartRow <- as.numeric(gsub("[A-Oa-o]*", "", dfMoves$Start))
  dfMoves$StartCol <- sapply(gsub("[0-9]*",    "", dfMoves$Start),
                             function(x) as.numeric(which(LETTERS == toupper(x)))
                             )
  
  return(dfMoves)
  
}
