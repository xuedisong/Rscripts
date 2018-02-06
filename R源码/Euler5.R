##最小的能被1-20中每个数整除的正整数是多少？
##在n内查找目标,找不到输出n
##

mainf<-function(n){
  for(i in 1:n)if(all(i%%(1:20)==0))break
  i
}