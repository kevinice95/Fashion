#TO DO: Clean up code. remove lines that aren't useful. dont run!
fabrics <- read.csv('fictionfabrics%.csv')
document <- readLines("ngrams.tsv")
string <- ''
columns <- colnames(fabrics)[-(1:2)]
for (col in columns){
  string <- paste0(string,"\\b",col,"\\b|")
}
string <- substring(string,first=1,last=nchar(string)-1)
indices <- unique(grep(string,document))  
rawdata <- c()
for (i in 1:length(indices)){
  rawdata[i] <- document[indices[i]]
}

parsed <- c()
index = 1
for (i in 1:length(rawdata)){
  test <- strsplit(rawdata[i],'\t')
  test <- gsub('[[:digit:]]+', '', test[[1]][1])
  test <- gsub(' ','',test)
  if (grepl(test,paste0(colnames(fabrics)[-(1:2)],collapse=' '))==1) {
    parsed[index] <- strsplit(rawdata[i],'\t')
    index <- index + 1
  }
}
data <- data.frame(parsed)
tempnames <- c()
for (i in 1:ncol(data)){
  tempnames[i] <- gsub('[[:digit:]]+','',as.character(data[1,i]))
}
colnames(data) <- tempnames 
data <- cbind(years = 1800:1950,data[-(1:2),])
data <- data[, -grep("oxford", colnames(data))]
#data <- data[, -grep("cotton", colnames(data))]
write.csv(data,'rawfabrics.csv',row.names = FALSE)


#Clothes
clothes <- read.csv('fictionclothes%.csv')
document <- readLines("ngrams.tsv")
string <- ''
columns <- colnames(clothes)[-(1:2)]
for (col in columns){
  string <- paste0(string,"\\b",col,"\\b|")
}
string <- substring(string,first=1,last=nchar(string)-1)
indices <- unique(grep(string,document))  
rawdata <- c()
for (i in 1:length(indices)){
  rawdata[i] <- document[indices[i]]
}

parsed <- c()
index = 1
for (i in 1:length(rawdata)){
  test <- strsplit(rawdata[i],'\t')
  test <- gsub('[[:digit:]]+', '', test[[1]][1])
  test <- gsub(' ','',test)
  if (grepl(test,paste0(colnames(clothes)[-(1:2)],collapse=' '))==1) {
    parsed[index] <- strsplit(rawdata[i],'\t')
    index <- index + 1
  }
}
data <- data.frame(parsed)
tempnames <- c()
for (i in 1:ncol(data)){
  tempnames[i] <- gsub('[[:digit:]]+','',as.character(data[1,i]))
}
colnames(data) <- tempnames 
data <- cbind(Year = 1800:1950,data[-(1:2),])
#data <- data[, -grep("oxford", colnames(data))]
#data <- data[, -grep("cotton", colnames(data))]
write.csv(data,'rawclothes.csv',row.names = FALSE)
