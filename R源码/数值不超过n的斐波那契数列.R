##数值不超过n的斐波那契数列
feibo<-function(n){
  fb<-c()
  fb[1]<-1
  fb[2]<-2
  for(i in 2:n){
    fb[i+1]<-fb[i-1]+fb[i]
    if(fb[i+1]>n){break}
  }
  fb[-i-1]
}