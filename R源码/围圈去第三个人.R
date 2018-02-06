f<-function(n){
  a<-seq(n)
  if(n==1)return("本程序n只能输入大于等于2的数")
  repeat{ b<-length(a)
          if(b%%3==1){m<-a[b]}
          if(b%%3==2){q<-c(b-1,b)
                      m<-a[q]}
          if(b%%3==0){m<-c()}
          d<-seq(b)
          e<-d[d%%3!=0]
          a<-a[e]
          u<-length(a)
          if(u==2){ break }
          w<-c(m,a)
          a<-unique(w)
  }
  k<-a[2]
  return(k)} 