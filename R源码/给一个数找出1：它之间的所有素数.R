f<-function(n){
  j<-c()
  for(x in 3:n){
    if(all(x%%(2:(x-1))!=0))j<-c(j,x)
  }
  c(2,j)
}