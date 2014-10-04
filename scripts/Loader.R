

LoadData <- function(sampleFraction = 0.1){
  # Use sampleFraction to use only a fraction of the boards
  
  allFiles <- list.files("data","anno.*\\.gcg")
  
  
  filelist <- sample(allFiles, length(allFiles) * sampleFraction, replace=FALSE)
  ctr <- 1
  
  listdfMoves <- lapply(filelist, function(gcgFileName){
    cat(" ", ctr," ", gcgFileName,"...\t")
    dfMoves <- tryCatch({
      gcgFileName <- paste("data", gcgFileName, sep ="/")
      gcg <- readLines(gcgFileName)
      
      dfMoves <- PlaysList(gcg)
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
  cat(numSuccessful, "\tSuccessful\n", numFailed, "\t\t\tFailed\n")
  
  return(listdfMoves)
}




