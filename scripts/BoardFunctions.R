
source("scripts/ParseFunctions.R")

PrintBoard <- function(board, sep=""){
  for(row in 1:nrow(board)){
    rowtext <- do.call(paste,as.list(c(board[row,], sep=sep)))
    cat(rowtext, "\n")
  }
}

MakeBoard <- function(gcg){
  
  #Initialize empty board
  board <- matrix(data=".", nrow=15, ncol=15)
  
  dfMoves <- PlaysList(gcg)
  
  for(i in 1:nrow(dfMoves)){
    move <- dfMoves$[i]
    direction <- types[i]
    word <- gsub("\\ .*$", "", gsub("^[0-9A-Oa-o]*\\ ", "", move))
    location <- gsub("\\ .*$", "", move)
    row <- as.numeric(gsub("[A-Oa-o]", "", location))
    col <- as.numeric(which(LETTERS == gsub("[0-9]", "", location)))
    if(length(col)==0) {
      col <- as.numeric(which(letters == gsub("[0-9]", "", location)))
    }
    
    for(l in strsplit(word,"")[[1]]){
      if(l!="."){
        board[row,col] <- l
      }
      if(direction=="v"){ 
        row <- row + 1
      } else {
        col <- col + 1
      }
      
    }
  }
  
  board
  
}





