# Probability of No Duplicates when Drawing Numbered Balls (0000 to 9999)

sureness <- 1
probs <- c()
limits <- 10000:9800
for(i in limits){
  sureness <- sureness*(i/max(limits))
  probs <- c(probs, sureness)
  # if(sureness < 0.9) {print(10001 - i - 1); break}
}
plot(probs, xlim = c(43,49), ylim = (c(0.88,0.92)), xlab = "Number of Draws", ylab = "Probability",
     main = "Probability of No Duplicates")
abline(h = 0.9, col = "red")
text(x = c(46,47), y = c(probs[46], probs[47]), labels = round(c(probs[46], probs[47]), 5), pos = 2)
max(which(probs > 0.9))

######

min(which(probs < 0.5)) # This means that in a room of 119 people, it is more likely than not
                        # that two people have the same PIN.