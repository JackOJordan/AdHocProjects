startingCredits <- 10
betAmount <- 1
pA <- 1/20
pK <- 3/20
pQ <- 5/20
pJ <- 5/20
p10 <- 6/20
pA_3 <- pA^3
pK_3 <- pK^3
pQ_3 <- pQ^3
pJ_3 <- pJ^3
p10_3 <- p10^3
pA_2 <- 2*(pA^2)*(1-pA)
pK_2 <- 2*(pK^2)*(1-pK)
pQ_2 <- 2*(pQ^2)*(1-pQ)
pJ_2 <- 2*(pJ^2)*(1-pJ)
p10_2 <- 2*(p10^2)*(1-p10)
payouts <- c(100,20,10,8,5,2,1,1,1,1)
rtp <- pA_3*payouts[1]+pK_3*payouts[2]+pQ_3*payouts[3]+pJ_3*payouts[4]+p10_3*payouts[5]+pA_2*payouts[6]+pK_2*payouts[7]+pQ_2*payouts[8]+pJ_2*payouts[9]+p10_2*payouts[10]
outcomes <- (20^3)*c(pA_3,pK_3,pQ_3,pJ_3,p10_3,pA_2,pK_2,pQ_2,pJ_2,p10_2)
outcomes
winnings <- c()
for(i in 1:10){
  winnings <- c(winnings,rep(payouts[i],outcomes[i]))
}
zeros <- seq(0,0,length.out=8000-length(winnings))
winnings <- c(winnings,zeros)
sample(winnings,10,replace = TRUE)-1
winnings[ceiling(runif(1,min=0,max=8000))]
balance <- startingCredits
for(i in 1:100){
  balance <- balance + winnings[ceiling(runif(1,min=0,max=8000))] - 1
  print(balance)
  if(balance == 0) {print(paste("Turns:", i, sep = " "), quote = FALSE); break}
}