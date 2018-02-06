w=c()
for(n in 1:10000){
  t<-c(1:50)
  for(i in 1:49){
    j=51-i
    cq<-sample(1:j,2)
    jd<-abs(t[cq[1]]-t[cq[2]])
    t=t[-cq]
    t=c(t,jd)
}
w=c(w,t)
}
for(m in 1:50){
  cat(m,"\t",sum(w==m),"\n")
  }