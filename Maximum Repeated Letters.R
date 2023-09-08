setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")
wordsList <- file(description = "corncob_caps.txt")
words <- readLines(wordsList)

fives <- c()
for(i in 1:length(words)){
  if(max(table(unlist(strsplit(words[i], split = "")))) >= 5) fives <- c(fives, words[i])
}

sixes <- c()
for(i in 1:length(fives)){
  if(max(table(unlist(strsplit(fives[i], split = "")))) >= 6) sixes <- c(sixes, fives[i])
}

sevens <- c()
for(i in 1:length(sixes)){
  if(max(table(unlist(strsplit(sixes[i], split = "")))) >= 7) sevens <- c(sevens, sixes[i])
}

paste(c(length(fives), " words have the same letter repeated at least five times."), collapse = ""); paste(c(length(sixes), " words have the same letter repeated at least six times."), collapse = ""); paste(c(length(sevens), " words have the same letter repeated at least seven times."), collapse = "")
sixes