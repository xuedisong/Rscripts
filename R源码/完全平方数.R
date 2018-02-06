f<-function(){
  r<-0
  w<-c()
  for(i in 1:1000000){
    a<-sqrt(r+100)
    b<-sqrt(r+168)
    if(a==floor(a)&b==floor(b))w<-c(w,r)
    r<-r+1
  }
  return(w)
}
