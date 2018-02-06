WS<-function(b){
  r<-c()
  for(s in 2:b){
    a<-1:(s-1)
    if(s==sum(a[s%%(1:(s-1))==0])){
      r<-c(r,s)
    }
  }
  return(r)
}