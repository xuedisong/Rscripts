GY<-function(B,b){
  if(B<b){
    t<-b
    b<-B
    B<-t
  }
  if(B*b==0){return("傻瓜，必须输入正整数，正整数才有公因数")}
  repeat{
    temp<-b
    b<-B%%b
    B<-temp
    if(b==0) break
  }
  return(B)
}