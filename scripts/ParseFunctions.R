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



PlaysList <- function(gcg, derivative=TRUE){
  #derivative:  Bool. Whether we need secondary/derivative information, 
  #             like wordlength, bingo, etc.
  
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
  
  
  if(derivative){
    #Secondary/Derivative Information:
    dfMoves$NumLetters <- nchar(gsub("\\.", "", dfMoves$Word))
    dfMoves$Bingo <- dfMoves$NumLetters==7
  }
  
  return(dfMoves)
  
}




LoadBoards <- function(sampleFraction = 0.1, derivative=TRUE){
  # Use sampleFraction to use only a fraction of the boards
  
  allFiles <- list.files("data","anno.*\\.gcg")
  
  
  filelist <- sample(allFiles, length(allFiles) * sampleFraction, replace=FALSE)
  ctr <- 1
  
  listdfMoves <- lapply(filelist, function(gcgFileName){
    cat(" ", ctr," ", gcgFileName,"...\t")
    dfMoves <- tryCatch({
      gcgFileName <- paste("data", gcgFileName, sep ="/")
      gcg <- readLines(gcgFileName)
      
      dfMoves <- PlaysList(gcg, derivative=derivative)
      cat("Done\n") 
      dfMoves
    },
    error=function(e) {
      cat("Failed\n")
      NULL
    }
    )
    ctr <<- ctr + 1
    dfMoves 
  })
  
  
  numFailed <- sum(sapply(listdfMoves, is.null))
  listdfMoves <- listdfMoves[!sapply(listdfMoves, is.null)]
  numSuccessful <- length(listdfMoves)
  cat("\n", numSuccessful, "\tSuccessful\n", numFailed, "\t\t\tFailed\n\n")
  
  return(listdfMoves)
}






