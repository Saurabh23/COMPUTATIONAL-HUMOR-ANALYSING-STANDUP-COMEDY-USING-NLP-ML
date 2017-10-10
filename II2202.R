Live <- read.delim2("~/Live.txt", header=FALSE, row.names=NULL, quote="", stringsAsFactors=FALSE)
View(Live)

subset = Live[1:5,1]
#Create a function with grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+",subset[3])

index <- vector(mode="integer",length=noofrows)
starttime <- vector(mode="integer",length=noofrows)
endtime <- vector(mode="integer",length=noofrows)
joke <- rep(NA,length=noofrows)
df = data.frame(index, starttime, endtime, joke)
#find max ID of dataset
df_index = 0
for (i in 1:nrow(Live)) { #make as a function that takes a dataframe and finds no of rows of new dataframe
  #if is index
  if (!is.na(as.numeric(Live[i,1]))) { #produces warnings
    df_index = df_index + 1 #keep track of data frame index
    df[df_index,1] = df_index #add to dataframe

  }
  #if is timestamp
  if (substring(Live[i,1],14,16) == "-->") {
    
    
    the_split <- strsplit(Live[i,1], " --> ")
    df[df_index,2] = unlist(the_split)[1] #add start time
    df[df_index,3] = unlist(the_split)[2] #add end time
  }
  #if is joke Maybe just check if first char is a char.
  if (grepl("[^0-9]",(substring(Live[i,1],1,1)))){
    if (is.na(df[df_index,4])){ #if dataframe row is NA (first time it is encountered and a joke should be put in)
      df[df_index,4] <- Live[i,1]
    } else { #Else, concatenate joke with the following halves of it.
      df[df_index,4] <- paste0(df[df_index,4], " ", Live[i,1])
    }
 
  }
}
###############
#MAJOR THOUGHT!
###############
#It seems like whole jokes are not contained in a single closed caption
#Maybe we need to split jokes based on how much time is inbetween subtitle(df(i)) and subtitle (df(i+1))
#so if the difference between (endtime (i) to starttime(i+1)) is greater than a threshold
#  then a joke must have preceeded that threshold.
###############
#MAJOR THOUGHT OVER!
###############


###############
#Convert current time format
#hh:mm:ss,ms ---> ms OR ssss
###############


converttime <- function(string){
  tenhr <- as.numeric(substring(string,1,1))
  onehr <- as.numeric(substring(string,2,2))
  tenmin <- as.numeric(substring(string,4,4))
  onemin <- as.numeric(substring(string,5,5))
  tensec <- as.numeric(substring(string,7,7))
  onesec <- as.numeric(substring(string,8,8))
  onemilsec <- as.numeric(substring(string,10,10))
  tenmilsec <- as.numeric(substring(string,11,11))
  hunmilsec <- as.numeric(substring(string,12,12))
  #milsec <- substring(string,10,13).
  
  inmsecs <- onemilsec + tenmilsec*10 + hunmilsec*100 + onesec*1000 + tensec*10*1000 + onemin*60*1000 + tenmin*60*1000*10 #+ onehr*60*60*60
  return(inmsecs)
}

#####
#Add new columns (starttime in ms and end time in ms) and compute values using convertime function

starttimems <- vector(mode="integer",length=noofrows)
df$starttimems <- starttimems
endtimems <- vector(mode="integer",length=noofrows)
df$endtimems <- endtimems

df[,5] <- converttime(df[,2])
df[,6] <- converttime(df[,3])

####
#Add sentiment of jokes
####
install.packages("devtools")

require(devtools)
install_github('okugami79/sentiment140')
library(sentiment)

#Compute sentiments

for (i in nrow(df)){
  l = sentiment(df[,4])
}

polarity <- vector(mode="integer",length=noofrows)

#map polarity from text to integer (-1 for negative , 0 for neutral +1 for positive.)
l[,2] <- revalue(x = l[,2], c("positive" = 1, "negative" = -1, "neutral" = 0))

#add sentiments to data frame (Df)
df$polarity <- l[,2]
cbind(df$polarity,l[,2])

####
#Export as csv for processing in Matlab (with audio)
####
write.csv(df,"preprocessedhumor.csv")


#Compute audio characteristics from after a joke to the next one. (endtime (i) to starttime(i+1))

#Audio <- readWave("C:\\Users\\Anotherpc\\Documents\\Downloads\\complete\\Live.At.The.Apollo.S11E01.720p.HDTV.x264-C4TV-NewzTown\\test.wav")
#load audio (mp3)
Audio <- readMP3("C:\\Users\\Anotherpc\\Documents\\Downloads\\complete\\Live.At.The.Apollo.S11E01.720p.HDTV.x264-C4TV-NewzTown\\test.wav")

s1 <- Audio@left
s1 <- s1 / 2^(Audio@bit -1)
timeArray <- (0:(121152-1)) / Audio@samp.rate
plot(timeArray, s1, type='l', col='black', xlab='Time s', ylab='Amplitude') 


121152/Audio@samp.rate #=2.747211 = a sample is 2747ms (0.12 = 120ms)

