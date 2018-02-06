##n的最大质数因子

##sushu
##ysfj

mainf<-function(n){
  a<-ysfj(n)
  for(i in length(a):1)
  if(sushu(a[i]))return(a[i])
}