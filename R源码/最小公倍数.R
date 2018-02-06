GB<-function(B,b){
  for(i in B:(b*B)){
    if((i%%B==0)&(i%%b==0)) break
  }
  return(i)
}