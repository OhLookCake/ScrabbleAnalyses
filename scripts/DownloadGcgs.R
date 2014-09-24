
#Download gcg files

baseurl <- "http://www.cross-tables.com/annotated/selfgcg/10"

rawdata <- readLines(baseurl)
gcgFileLines <- grep('^<li><a href=\\"anno',rawdata, value=T)
gcgFiles <- gsub("^.*>\\ ","", gsub("</a></li>","", gcgFileLines))

fileCount <- 0
for(gfile in gcgFiles){
  fullurl <- paste(baseurl, gfile, sep="/")
  download.file(fullurl, paste("data", gfile, sep="/"), quiet=T) #save to the data directory
  fileCount <- fileCount + 1
  cat(fileCount)
}



