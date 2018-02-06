ysfj<-function(m){##因式分解。如输入20，输出2 2 5，即20=2x2x5
  a<-function(n){
    for(i in 2:n){
      if(n%%i==0){return(i)}
    }
  }
  b<-c()
  repeat{
    A<-a(m)
    b<-c(b,A)
    if(A==m)break
    m<-m/A
  }
  return(b)
}