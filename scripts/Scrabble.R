
PrintBoard <- function(board, sep="")
{
  for(row in 1:nrow(board)){
    rowtext <- do.call(paste,as.list(c(board[row,], sep=sep)))
    cat(rowtext, "\n")
  }
}

CategorizeMoves <- function(cleanGcg){
  types <- sapply(cleanGcg, function(move){
      
    if(grepl("^-[A-Z\\?]+", move)) { type <- "exchange"}
    if(grepl("^-\\ ", move)) { type <- "pass"}
    if(grepl("^--", move)) { type <- "successfulchallenge"}
    if(grepl("^\\(challenge\\)", move)) { type <- "challengebonus"}
    if(grepl("^\\(time\\)", move)) { type <- "timepenalty"}
    if(grepl("^\\([A-Z\\?]\\)*", move)) { type <- "leave"}
    if(grepl("^[0-9]+[A-O]+", move)) { type <- "h"}
    if(grepl("^[A-O]+[0-9]+", move)) { type <- "v"}
    type
  })
  
  for(i in 1:length(types)){
    if(types[i]=="successfulchallenge") { types[i-1] <- "phoney" }
  }
  
  types
  
}

MakeBoard <- function(gcg){
  
  #Clean up comments, meta data
  gcg <- gcg[-c(grep("^#", gcg), grep("^[\\ ]*$", gcg))]
  #Remove player names and rack
  cleanGcg <- gsub("^.*\\:\\ [A-Z\\?]*\\ ", "", gcg)
  types <- CategorizeMoves(cleanGcg)
  
  cleanGcg <- cleanGcg[types=="h" | types=="v"]
  types <- types[types=="h" | types=="v"]
  
  
  #Initialize empty board
  board <- matrix(data=".", nrow=15, ncol=15)
  
  #iterate over valid moves one by one, and update the board
  for(i in 1:length(cleanGcg)){
    move <- cleanGcg[i]
    direction <- types[i]
    word <- gsub("\\ .*$", "", gsub("^[0-9A-O]*\\ ", "", move))
    location <- gsub("\\ .*$", "", move)
    row <- as.numeric(gsub("[A-O]", "", location))
    col <- as.numeric(which(LETTERS == gsub("[0-9]", "", location)))
      
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





