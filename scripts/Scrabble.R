
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
    if(grepl("^[0-9]+[A-Oa-o]+", move)) { type <- "h"}
    if(grepl("^[A-Oa-o]+[0-9]+", move)) { type <- "v"}
    type
  })
  
  for(i in 1:length(types)){
    if(types[i]=="successfulchallenge") { types[i-1] <- "phoney" }
  }
  
  types
  
}

MakeBoard <- function(gcg){
  
  #Initialize empty board
  board <- matrix(data=".", nrow=15, ncol=15)
  
  #Clean up comments, meta data
  gcg <- gcg[-c(grep("^#", gcg), grep("^[\\ ]*$", gcg))]
  gcg <- gcg[grep("^>", gcg)]
  gcg <- gsub("#.*$", "", gcg)
  
  #Remove player names and rack
  cleanGcg <- gsub("^[[:alnum:][:punct:]]*\\:\\ [A-Z\\?]*[\\ ]*", "", gcg)
  
  #if No game details found, return blank board
  if(length(cleanGcg)==0) { return(board) } # this statement need to be done twice because CategorizeMoves() also throws an error is empty
  
  types <- CategorizeMoves(cleanGcg)
  
  cleanGcg <- cleanGcg[types=="h" | types=="v"]
  types <- types[types=="h" | types=="v"]
  
  if(length(cleanGcg)==0) { return(board) }
    
  
  #iterate over valid moves one by one, and update the board
  for(i in 1:length(cleanGcg)){
    print(i)
    move <- cleanGcg[i]
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





