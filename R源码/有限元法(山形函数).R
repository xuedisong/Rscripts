##注意：需要给定（输入全局变量）：a,b,n,p,q,f后才可运行该脚本！！！！！

##预处理,导入定积分所在包
library("stats", lib.loc="D:/R-3.3.2/library")

##1
  h<-abs(b-a)/n##h等距步长
  X<-c()##用以存储节点，注意从1开始
  X[1]<-a
  for(i in 1:n)X[i+1]<-X[i]+h##所以X下标为1：(n+1)
  H<-c()##用以存储步长
  for(i in 1:n)H[i]<-X[i+1]-X[i]##H下标为1：n
##1确定X与H向量，非均匀划分请修改此处！！！！！！！！！！！
  
  
  N<-n-1
##确定试探空间的维数
  
##确定矩阵AX=B的右端项左端项A  
  A<-matrix(0,N,N)
##循环赋值以确定A(标准化后的A)
  for(i in 1:N){
    for(j in 1:N){
      if(abs(i-j)>=2)A[i,j]<-0##非三对角处赋0
      else{
        ##下分三种情况，地位均等
        
        ##1
        if(i==j){
          AUV1<-function(ep){1/H[i]*p(X[j]+H[j]*ep)+H[j]*q(X[j]+H[j]*ep)*ep^2}##让我们抄书上的公式吧
          AUV2<-function(ep){1/H[i+1]*p(X[j+1]+H[j+1]*ep)+H[j+1]*q(X[j+1]+H[j+1]*ep)*(1-ep)^2}
          A[i,j]<-integrate(AUV1,0,1)$value+integrate(AUV2,0,1)$value
          ##求定积分integrate函数结果为list,需要取其value部分
        }
        
        ##2
        if(i+1==j){
          AUV<-function(ep){-1/H[j]*p(X[j]+H[j]*ep)+H[j]*q(X[j]+H(j)*ep)*(1-ep)*ep}
          A[i,j]<-integrate(AUV,0,1)$value
        }
        
        ##3
        if(i-1==j){
          AUV<-function(ep){-1/H[j+1]*p(X[j+1]+H[j+1]*ep)+H[j+1]*q(X[j+1]+H[j+1]*ep)*ep*(1-ep)}
          A[i,j]<-integrate(AUV,0,1)$value
        }
      }
      ##当右端值不为0，即需要U（n）则N<-n,矩阵会多一阶下面两行代码有用,右（左）端点边界条件不为0，请修改此处！！！！！！！
      ##AUVN<-function(ep){1/H[n]*p(X[N]+H[N]*ep)}
      ##A[N,N]<-integrate(AUVN,0,1)$value
    }
  }
  
  ##确定矩阵AX=B的右端项B(标准化后的B)
  B<-matrix(0,N,1)
  ##循环赋值以确定B
  for(j in 1:N){
    AU1<-function(ep){f(X[j]+H[j+1]*ep)*ep}
    AU2<-function(ep){f(X[j+1]+H[j+1]*ep)*(1-ep)}
    B[j,1]<-H[j]*integrate(AU1,0,1)$value+H[j+1]*integrate(AU2,0,1)$value
  }
  
  ##解线性方程组,其中A为三对角阵，宜采用追赶法,此处可调用(内嵌)追赶法的算法,并且不影响代码其他部分的稳定性
  ##AX=B时solve(A,B)==X
  solve(A,B)
  
  ##可将整段代码封装,需要给定的参数为:
  ##1.区间[a,b]
  ##2.理论部分a(u,v)中的函数p,q,(此处不含r项)及微分方程中的函数f
  ##3.[a,b]的一个划分，当均匀划分时，即给定n值
  
  ##输出结果为：
  ##基底前的系数阵(向量)
  
  ##补充基底函数的定义(？是标准化后的？)
  ##标准化矩阵有点到底在哪？
  ##验证非均匀步长？
  
  ##实例由PDF课件上cp2的例P15例1测试成功（对基底系数（4个值））