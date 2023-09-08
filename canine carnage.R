# 0 = 10 (for code purposes), 1,2,3,4 are dogs, C = Collector, W = Wild, B = Bones, X# = x# Multipliers, S = Scatters

icons <- c("0","J","Q","K","A","1","2","3","4","C","W","B","X2","X3","X5","S")

# Payouts for each icon of a 3-line run

payouts_3 <- c(0.1,0.1,0.1,0.1,0.1,0.4,0.4,0.4,0.4,NA,NA,NA,NA,NA,NA,NA)

# Payouts for each icon of a 4-line run

payouts_4 <- 4*payouts_3

# Payouts for each icon of a 5-line run

payouts_5 <- 10*payouts_3

# Dataframe of Icon, Probability and Payout info

df <- data.frame(icons,payouts_3,payouts_4,payouts_5)

# Creating the probabilities of icons showing on each of the 5 reels, from left to right

r1 <- c(5,5,5,5,5,3,3,3,3,1,1,1,1,1,1,1)
r1p <- r1/sum(r1)
r2 <- c(5,5,5,5,5,3,3,3,3,1,1,1,1,1,1,0)
r2p <- r2/sum(r2)
r3 <- c(5,5,5,5,5,3,3,3,3,1,1,1,1,1,1,1)
r3p <- r3/sum(r3)
r4 <- c(5,5,5,5,5,3,3,3,3,1,1,1,1,1,1,0)
r4p <- r4/sum(r4)
r5 <- c(5,5,5,5,5,3,3,3,3,1,1,1,1,1,1,1)
r5p <- r5/sum(r5)

# Creating all winning 3-line combos

winningCombos_3 <- data.frame()
for(i in 1:9){
  winningCombos_3 <- rbind(winningCombos_3,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[-2^3,])
  if(i == 9) winningCombos_3 <- rbind(winningCombos_3,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[2^3,])
}
names(winningCombos_3) <- c("Reel 1","Reel 2","Reel 3")

# Creating all winning 4-line combos

winningCombos_4 <- data.frame()
for(i in 1:9){
  winningCombos_4 <- rbind(winningCombos_4,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[-2^4,])
  if(i == 9) winningCombos_4 <- rbind(winningCombos_4,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[2^4,])
}
names(winningCombos_4) <- c("Reel 1","Reel 2","Reel 3","Reel 4")

# Creating all winning 5-line combos

winningCombos_5 <- data.frame()
for(i in 1:9){
  winningCombos_5 <- rbind(winningCombos_5,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[-2^5,])
  if(i == 9) winningCombos_5 <- rbind(winningCombos_5,expand.grid(c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]),c(icons[i],icons[11]))[2^5,])
}
names(winningCombos_5) <- c("Reel 1","Reel 2","Reel 3","Reel 4","Reel 5")

# Combining all winning combos into a single list (winners)

winners <- c()
for(i in 1:length(winningCombos_3[,1])){
  winners <- c(winners, paste(c(winningCombos_3[i,1],winningCombos_3[i,2],winningCombos_3[i,3]),collapse = ""))
}
for(i in 1:length(winningCombos_4[,1])){
  winners <- c(winners, paste(c(winningCombos_4[i,1],winningCombos_4[i,2],winningCombos_4[i,3],winningCombos_4[i,4]),collapse = ""))
}
for(i in 1:length(winningCombos_5[,1])){
  winners <- c(winners, paste(c(winningCombos_5[i,1],winningCombos_5[i,2],winningCombos_5[i,3],winningCombos_5[i,4],winningCombos_5[i,5]),collapse = ""))
}

# Creating a sample game board for testing

gameboard <- matrix(NA, nrow = 3, ncol = 5)

gameboard[,1] <- sample(icons, size = 3, replace = TRUE, prob = r1p)
gameboard[,2] <- sample(icons, size = 3, replace = TRUE, prob = r2p)
gameboard[,3] <- sample(icons, size = 3, replace = TRUE, prob = r3p)
gameboard[,4] <- sample(icons, size = 3, replace = TRUE, prob = r4p)
gameboard[,5] <- sample(icons, size = 3, replace = TRUE, prob = r5p)

gameboard

# Finding all 3-line combos in the sample game board, along with their row indices

combosList <- data.frame(cbind(NA,NA))
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
          combosList[9*(i-1)+3*(j-1)+k,1] <- paste(c(gameboard[i,1],gameboard[j,2],gameboard[k,3]), collapse = "")
          combosList[9*(i-1)+3*(j-1)+k,2] <- paste(c(i,j,k), collapse = "")
    }
  }
}

# Finding all 4-line combos in the sample game board, then adding them to the list of 3-line combos

for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      for(l in 1:3){
          combosList <- rbind(combosList,c(paste(c(gameboard[i,1],gameboard[j,2],gameboard[k,3],gameboard[l,4]), collapse = ""),
                                           paste(c(i,j,k,l), collapse = "")))
      }
    }
  }
}


# Finding all 5-line combos in the sample game board, then adding them to the list of 3- and 4-line combos

for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      for(l in 1:3){
        for(m in 1:3){
          combosList <- rbind(combosList,c(paste(c(gameboard[i,1],gameboard[j,2],gameboard[k,3],gameboard[l,4],gameboard[m,5]), collapse = ""),
                                           paste(c(i,j,k,l,m), collapse = "")))
        }
      }
    }
  }
}

# Finding all lines in the game board

allLines <- combosList[,1]

# Finding all winning combos which appear in the sample game board

allWins <- c(allLines[allLines %in% winners])

allWins
gameboard

# Filtering only the longest wins in each line

if (length(allWins) > 0){
  for(i in 1:length(allWins)){
    if (length(grep(allWins[[i]],allWins[!(allWins == allWins[[i]])],value = TRUE)) > 0) allWins[[i]] <- NA # allWins <- allWins[!(allWins == allWins[[i]])]
    if (i >= length(allWins)) break
  }
  allWins <- allWins[!is.na(allWins)]
  allWins
}

# Finding the row indices of the winning lines

winningLines <- combosList[combosList[,1] %in% allWins,]
winningLines

# Row and Column values of W

wildLocations <- which(gameboard == "W", arr.ind = TRUE)
if (length(wildLocations) == 2) wildLocations <- t(as.data.frame(wildLocations))
if (length(wildLocations) > 2) wildLocations <- wildLocations[!duplicated(wildLocations[,1]),]
if (length(wildLocations) == 2) wildLocations <- t(as.data.frame(wildLocations))
rowsToDouble <- wildLocations[,1]

# For every winning line, finding the row index of the first W in the line (WIP)

if(length(winningLines) > 0){
  if("W" %in% gameboard == TRUE){
    for(m in 1:length(wildLocations[,1])){
      for(j in 1:length(allWins)){
        test <- as.numeric(unlist(strsplit(winningLines[,2][j],split = "")))
        for(i in 1:wildLocations[m,2]){
          test[i] <- NA
        }
        test <- test[!(is.na(test))]
        if(length(test[test %in% rowsToDouble == TRUE]) >= 1){
          for(l in 1:(2^(length(test[test %in% rowsToDouble == TRUE]))-1)){
            winningLines <- rbind(winningLines,winningLines[j,])
          }
        }
      }
    }
  }
}
winningLines

# Use to see if multipliers are in the sample game board

# grep("X",icons,value = TRUE) %in% gameboard
gameboard
length(winningLines[,1])
table(winningLines[,2])
# 12123 12133 12323 12333 13123 13133 13323 13333 32123 32133   322 32323 32333 33123 33133 33323 33333 
# 3     4     5     8     5     8     9    16     3     4     2     5     8     5     8     9    16
gameboard


for(i in 1:length(wildLocations[,2])){
  test <- c("12133")
  test <- as.numeric(unlist(strsplit(test,split = "")))
  for(j in 1:wildLocations[,2][i]){
    test[j] <- NA
  }
  if(length(test[test %in% rowsToDouble[1] == TRUE]) >= 1){
    for(l in 1:(2^(length(test[test %in% rowsToDouble == TRUE]))-1)){
      winningLines <- rbind(winningLines,winningLines[j,])
    }
  }
}
allWins

if(length(winningLines) > 0){
  if("W" %in% gameboard == TRUE){
    for(i in 1:length(wildLocations[,2])){
      for(j in 1:length(allWins)){
        test <- unlist(strsplit(allWins[j],split = ""))
        for(k in 1:wildLocations[i,2]){
          test[k] <- NA
        }
        print(sum(test %in% rowsToDouble[i]));print(test)
      }
          for(l in 1:(2^(length(test[test %in% rowsToDouble == TRUE]))-1)){
            winningLines <- rbind(winningLines,winningLines[j,])
          }
        }
      }
    }

if(length(winningLines) > 0){
  if("W" %in% gameboard == TRUE){
    for(i in 1:length(wildLocations[,2])){
      for(j in 1:length(allWins)){
        test <- as.numeric(unlist(strsplit(winningLines[j,2],split = "")))
        for(k in 1:wildLocations[i,2]){
          test[k] <- NA
          print(sum(test %in% rowsToDouble[i]));print(test)
        }
      }
    }
  }
}
test
test[test %in% rowsToDouble[1]]
wildLocations
for(i in 4:5){
  print(i)
  for(j in 6:7){
    print(i*j)
  }
}
gameboard
power <- 0
test <- c("12123")
test <- as.numeric(unlist(strsplit(test,split = "")))
for(j in 1:length(wildLocations[,2])){
  test <- c("12123")
  test <- as.numeric(unlist(strsplit(test,split = "")))
  for(k in 1:wildLocations[j,2]){
    test[k] <- NA
  }
  power <- sum(power,test %in% rowsToDouble[1])
}
power


# THIS ONE IS PROMISING!
# REMEMBER TO MAKE GAMEBOARD AS SAMPLE IN PHOTOS
power <- 0
for(i in 1:length(allWins)){
  power <- 0
  for(j in 1:length(wildLocations[,2])){
    test <- winningLines[i,2]
    test <- as.numeric(unlist(strsplit(test,split = "")))
    for(k in 1:wildLocations[j,2]){
      test[k] <- NA
    }
    power <- sum(power,test %in% rowsToDouble[j])
  }
  print(power)
}
power
winningLines[17,2]
wildLocations
