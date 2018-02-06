w<-c()
o<-c()
for(a in 100:999){
  for(y in 1:10){
     if((a/100)>=y&(a/100)<(y+1)){m<-y}
      r<-(a-100*m)
      for(b in 1:10){
                    if((r/10)>=b&(r/10)<(b+1)){n<-b}
                    if(r/10<1){n<-0}    
        }
      l<-(r-10*n)
      m
      n
      l
      if(m^3+n^3+l^3==a){o<-a}
  w<-c(w,o)
  }
  }
w
unique(w)
