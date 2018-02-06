##素数(正整数)判断
sushu<-function(x){
  if(x==1)return(F)else
    if(x==2)return(T)else
      return(!any(x%%(2:(x-1))==0))
}