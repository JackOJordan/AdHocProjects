setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")
wordsList <- file(description = "corncob_caps.txt")
words <- readLines(wordsList)

threePairs <- c()
for(i in 1:length(words)){
  if(length(table(unlist(strsplit(words[i], split = "")))[table(unlist(strsplit(words[i], split = ""))) >= 2]) >= 3) threePairs <- c(threePairs, words[i])
}

tripleConsecPairs <- c()
for(i in 1:length(threePairs)){
  testWord <- unlist(strsplit(threePairs[i], split = ""))
  testLetters <- names(table(testWord)[table(testWord) >= 2])
  consecCounter <- 0
  for(j in 1:length(testLetters)){
    positions <- which(testWord == testLetters[j])
    for(k in 1:(length(positions)-1)){
      if((positions[k+1] - positions[k]) == 1) consecCounter <- consecCounter + 1
    }
    if(consecCounter == 3) {tripleConsecPairs <- c(tripleConsecPairs, threePairs[i]); break}
  }
}
tripleConsecPairs

minLength <- 9999
for(i in 1:length(tripleConsecPairs)){
  if(length(unlist(strsplit(tripleConsecPairs[i], split = ""))) < minLength) minLength <- length(unlist(strsplit(tripleConsecPairs[i], split = "")))
}
shortestWords <- c()
for(i in 1:length(tripleConsecPairs)){
  if(length(unlist(strsplit(tripleConsecPairs[i], split = ""))) == minLength) shortestWords <- c(shortestWords, tripleConsecPairs[i])
}
shortestWords
