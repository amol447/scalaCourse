type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)
def emptySet:Set=_=>false
def singletonSet(elem:Int):Set=x=>x==elem
def union(s1:Set,s2:Set):Set=x=>s1(x)|s2(x)
def intersection(s1:Set,s2:Set):Set=x=>s1(x)&s2(x)
def setDiff(s1:Set,s2:Set):Set=x=>s1(x) & !s2(x)
def filter(s:Set,p:Int=>Boolean):Set=x=>s(x)& p(x)
def forall(s: Set, p: Int => Boolean): Boolean={
  def forallHelper(doneTill:Int,maxInt:Int):Boolean={
    if(doneTill>maxInt) true
    else{
      if(s(doneTill+1) & !p(doneTill+1)) false
      else forallHelper(doneTill+1,maxInt)
    }
  }
  forallHelper(-1000,1000)
}

def exists(s:Set,p:Int=>Boolean):Boolean={
  !forall(s,(x=> !p(x)))
}

def map(s:Set,fn:Int=>Int):Set= {
  (-1000 to 1000).toList.filter(s).map(fn andThen singletonSet).reduce(union)
}
val s1=singletonSet(100)
val s2:Set=x=>x%2==0
exists(s1,x=>x>90)
forall(s2,x=>x*5%10==0)
forall(s2,x=>x>10)
val s3=map(s2,x=>x*3)
forall(s3,x=>x%6==0)