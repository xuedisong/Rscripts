mainf<-function(n){
  a<-feibo(n)
  sum(a[a%%2==0])
}