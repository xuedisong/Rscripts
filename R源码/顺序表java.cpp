//////////////////////////////SeqList.java////////////////////////////////
public  class SeqList{
  final int defaultSize=10;
  
  int maxSize;//数组长度
  int size;//元素个数
  Object [] listArray;//数组成员
  
  public SeqList(){
    initiate(defaultSize);
  }//构造函数
  public SeqList(int size){
    initiate(size);
  }//构造函数
  private void initiate(int sz){
    maxSize=sz;
    size=0;
    listArray=new Object[sz];
  }//私有成员
  
  public void insert(int i,Object obj) throws Exception{
    if(size==maxSize){
      throw new Exception("顺序表已满无法插入");
    }
    
    listArray[i]=obj;
    size++;
  }//在i处插入object对象
  
  public Object delete(int i) throws Exception{
    if(size==0){
      throw new Exception("顺序表已空无法删除！");
    }
    Object it=listArray[i];
    for(int j=i;j<size-1;j++)
      listArray[j]=listArray[j+1];//往前移
    
    size--;
    return it;
  }//返回被删对象
  
  public Object getData(int i) throws Exception{
    if(i<0||i>size){
      throw new Exception("参数错误");
    }
    return listArray[i];
  }//返回指定位置的对象
  
  public int size(){
    return size;
  }//返回元素个数
  
  public boolean isEmpty(){
    return size==0;
  }//判断元素个数是否为空
  
  public int MoreDadaDelete(SeqList L,Object x)throws Exception{
    int i,j;
    int tag=0;
    
    for(i=0;i<L.size;i++){
      if(x.equals(L.getData(i))){
        L.delete(i);
        i--;
        tag=1;
      }
    }
    return tag;
  }//删除多个元素，删除成功返回1，检索不到返回0
  
}