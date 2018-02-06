//搞个局部指针t指针指向head所指对象，保证的head指针始终为头指针，并且由于不直接对head进行操作（delete head），可防止成为野指针.


//规则1：delete p;p=NULL;（这是标准做法，但有时不写后半句是因为coder想让p随着局部生命期自然消亡。）-----但是全局变量p必须谨慎，因为它有着全局生命期。
//规则2：全局指针必须赋值给（新建）局部指针进行操作，来保证安全。


//delete p只能释放p指针指向的内存空间；但p本身不会消亡，cout<<p将输出p所指（保存）的内存地址，此时该内存地址上是个随机数（cout<<*p依然有输出），成为野指针。
//若紧跟着新建指针p1指向动态分配的内存空间。----编译器默认将释放掉的内存空间回收然后分配给新开辟的空间。----这样就会造成两个指针指向一个空间，本该消失的p干扰着p1


/////////////////////list.cpp/////////////////////////////////////
#include"list.h"
  
  template<class T>
  void list<T>::add_node(node<T> *temp)//把temp加到链表末尾
  {
    nodes_num++;//个数多一个
    if(head==NULL) head=temp;//表里刚开始就是空的，head就指向temp
    else 
    {
      node<T> *t=head;//搞个t指针指向head所指对象，保证的head始终是头指针
      while(t->next!=NULL)//如果head后还有对象
        t=t->next;//循环直到t指向链表的末尾对象
      t->next=temp;///这时，末尾指向temp，此时t是倒数第二个元了
      //下面的两步是为了区分两个list合并
      t->next->next=NULL;//把temp指的下个对象清空，以防，下面链接后表时判断不出->next==NULL而陷入死循环。我觉得实例化node对象时next=NULL已经严格保证了，不然就太诡异了，难道要next指向某个危险的内存区域吗？
      temp=temp->next;//把temp清0（作死，此时t指向零指针temp）
    }
  }
  
  template<class T>
  node<T>* list<T>::search_node(T key)//通过对象的数值，返回该数(第一个)的内存地址
  {
    node<T> *t=head;//搞个t指针指向head所指对象，保证的head始终是头指针
    while(t!=NULL)
    {
      if(t->key==key)
        return t;
      t=t->next;
    }
    cout<<"can not the data you want"<<endl;//空表
    return NULL;//空表
  }
  
  template<class T>
  void list<T>::combine(list<T> &temp)//没有定义复制构造函数，默认的复制构造函数无法进行深层复制（无法复制自定义的list对象）所以采用引用
  {
    nodes_num+=temp.nodes_num;
    if(head==NULL) head=temp.head;//前者是空表，前表head直接指向后表head所指的对象
    else 
    {
      node<T>* t=head;//head保持不动
      while(t->next!=NULL)//循环使得t指向末尾数
        t=t->next;
      t->next=temp.head;
      temp.head=NULL;//清空后表中的head指针，避免成为野指针
    }
  }
  
  template<class T>
  void list<T>::delete_node(T key)
  {
    nodes_num--;
    if(head==NULL) {cout<<"can not find the data you want to delete"<<endl;}//空表
    else if(head->key==key){node<T>* t=head;head=head->next;delete t;}/*如果表中第一个数就是，就删掉该数，head是空指针，并将head指向后一个数,
    我们在删除一个指针之后，编译器只会释放该指针所指向的内存空间，而不会删除这个指针本身。指针的消亡是随着局部生命期的结束而结束，即块域的结束*/
    else 
    {
      node<T>* temp=head;
      node<T>* t;
      while(temp->next!=NULL)
      {
        if(temp->next->key==key)//只删掉该数
        {
          t=temp->next;
          temp->next=t->next;
          delete t;
          return ;//一般可省，但是此处用于跳出循环，不可省略
        }
        temp=temp->next;
      }
      cout<<"can not find the data you want to delete"<<endl;//如果第一个数后面没数了，就找不到
    }
  }
  
  template<class T>
  void list<T>::push_front(node<T>*temp)//从表前插入数
  {
    temp->next=head;//保证head的不变性
    head=temp;
  }
  
  template<class T>
  ostream & operator <<(ostream &stream,list<T> &t)//流输出运算符"<<"重载，打印list对象中的数
  {
    node<T> *temp=t.head;//保证head的不变性
    while(temp!=NULL)
    {
      cout<<temp->key<<endl;//关键步骤cout
      temp=temp->next;
    }
    return stream;
  }
  
  //////////////////////list.h/////////////////////////////
#ifndef _LIST_H_
#define _LIST_H_
#include<iostream>
  using namespace std;
  
  template<class T>
  class node
  {
  public:
    T key; //整数key
    node<T> *next;//指向node类的指针next
    
    node(T key):next(NULL),key(key){}//一个int初始化node
  };
  
  template<class T>
  class list
  {
  public:
    node<T> *head;//指向node类的指针head
    int nodes_num;//整数nodes_num
    
    list():head(NULL),nodes_num(0){}//无参构造函数
    
    void add_node(node<T>* temp);//参数：指向node类的指针
    void push_front(node<T>*temp);//参数：指向node类的指针，在前面增加
    node<T>* search_node(T key);//参数：整数，返回：指向node类的指针
    void delete_node(T key);//参数：整数
    void combine(list &temp);//参数：list
    
    friend ostream &operator<<(ostream &stream,list<T> &temp);
    
    ~list()//销毁当前head所指的node对象,并将head指向下一个node对象
    {
      node<T> *temp;//定义指向node类的指针temp
      while(head!=NULL)
      {
        temp=head;
        head=head->next;
        delete temp;
      }
    }
  };
  
#endif
  /////////////////////main.cpp///////////////////////////////////
#include<iostream>
#include"list.h"
#include"list.cpp"
  using namespace std;
  int main()
  {
    cout<<"本链表类没有复制构造函数，所以请注意，增加类的功能时，注意传递引用"<<endl;
    cout<<"特此声明!!!"<<endl;
    list<int> temp;//新建一个list对象temp
   
    cout<<"text add nodes"<<endl;
    temp.add_node(new node<int> (30));//动态分配数据存于不连续的内存空间
    temp.add_node(new node<int> (30));//动态分配对象node，实质上是动态分配对象中的数据成员的内存地址。
    temp.add_node(new node<int> (20));
    temp.add_node(new node<int> (10));
    temp.add_node(new node<int> (15));
    temp.add_node(new node<int> (33));
    cout<<temp<<endl;//重载，打印list对象中的数
    cout<<endl;
    
    cout<<"text: search node"<<endl;
    cout<<"search 10"<<endl;
    cout<<temp.search_node(10)<<endl
    cout<<"search 15"<<endl;
    cout<<temp.search_node(15)<<endl
    
    cout<<"text:delete node "<<endl;
    cout<<"delete 30"<<endl;
    temp.delete_node(30);
    cout<<temp<<endl;
    cout<<"delete 20"<<endl;
    temp.delete_node(20);
    cout<<temp<<endl;
    
    cout<<"text: combine list"<<endl;
    list<int> temp2;
    temp2.add_node(new node<int> (11));
    temp2.add_node(new node<int> (11));
    temp2.add_node(new node<int> (11));
    temp2.add_node(new node<int> (11));
    temp2.add_node(new node<int> (11));
    cout<<temp2<<endl;
    cout<<endl;
    
    temp.combine(temp2);
    cout<<temp<<endl;
    
    cout<<"text: push front node"<<endl;
    cout<<"push front 233"<<endl;
    temp.push_front(new node<int> (233));
    cout<<temp<<endl;
    cout<<endl;
    
    return 0;
  }