setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")
wordsList <- file(description = "corncob_caps.txt", open = "r", blocking = TRUE)
words <- readLines(wordsList)

alphabeticalWords <- c()
for(i in 1:length(words)){
  if(words[i] == paste(sort(unlist(strsplit(words[i], split = ""))), collapse = "")) alphabeticalWords <- c(alphabeticalWords, words[i])
}
alphabeticalWords

maxLength <- 0
longestWords <- c()
for(i in 1:length(alphabeticalWords)){
  if(length(unlist(strsplit(alphabeticalWords[i], split = ""))) >= maxLength) {maxLength <- length(unlist(strsplit(alphabeticalWords[i], split = ""))); longestWords <- c(longestWords, alphabeticalWords[i])}
}
for(i in 1:length(longestWords)){
  if(length(unlist(strsplit(longestWords[i], split = ""))) < maxLength) longestWords[i] <- NA
}
longestWords <- longestWords[!is.na(longestWords)]
longestWords
