chooseFunc <- function(n,r){
  prod(n:(n-r+1))/factorial(r)
}
binomExpand <- function(y,n,Terms = 4){
  approx <- 1
  for(i in 1:(Terms-1)){
    approx <- approx + chooseFunc(n,i)*(y^i) 
  }
  approx
}
2*binomExpand(1/4,1/2,5) # Estimate for sqrt(5)
