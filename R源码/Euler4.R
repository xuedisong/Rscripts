##找出最大的由两个三位数乘积构成的回文数

##回文数.R

mainf<-function(){
  w<-c()
  for(i in 999:100){
    for(j in 999:100){
      if(F(i*j))w<-c(w,i*j)
    }
  }
  max(w)
}