f<-function(){    ##u以内正整数能组成哪些数，没有重数
  re<-c()
  A<-1:4
  for(a in A){
    for(b in A[A!=a]){
      for(c in A[A!=a&A!=b]){
        r<-a*100+b*10+c
        re<-c(re,r)
      }
    }
  }
  return(re)
}