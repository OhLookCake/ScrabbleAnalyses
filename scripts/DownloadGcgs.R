
#Download gcg files

basebaseurl <- "http://www.cross-tables.com/annotated/selfgcg"

for(i in 1:193)
{
  baseurl <- paste(basebaseurl, i, sep="/")
  cat("\n", i," ")
  rawdata <- readLines(baseurl)
  gcgFileLines <- grep('^<li><a href=\\"anno',rawdata, value=T)
  gcgFiles <- gsub("^.*>\\ ","", gsub("</a></li>","", gcgFileLines))
  
  fileCount <- 0
  for(gfile in gcgFiles){
    fullurl <- paste(baseurl, gfile, sep="/")
    download.file(fullurl, paste("data", gfile, sep="/"), quiet=T) #save to the data directory
        #If something isn't working, set quiet=F
    fileCount <- fileCount + 1
    #cat(fileCount,"") #Enable if you want continuous count
  }
  
}


