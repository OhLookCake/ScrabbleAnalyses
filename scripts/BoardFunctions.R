
source("scripts/ParseFunctions.R")

PrintBoard <- function(board, sep=" "){
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
            
    row <- dfMoves$StartRow[i]
    col <- dfMoves$StartCol[i]
    
    for(l in strsplit(dfMoves$Word[i], "")[[1]]){
      if(l!="."){
        board[row,col] <- l
      }
      if(dfMoves$Direction[i] == "v"){ 
        row <- row + 1
      } else {
        col <- col + 1
      }
      
    }
  }
  
  board
  
}





