f<-function(a,b){        ##a是长字符，b为短字符
  w<-c()
  u<-nchar(a)
  y<-nchar(b)
  for(i in 1:(u-y+1)){
    h<-substr(a,i,i+y-1)
    w<-c(w,h)
  }
  o<-length(w)
  j<-c()
  for(i in 1:o){
    if(w[i]==b){j<-c(2,j)}
  }
  p<-length(j)
  return(p)
}