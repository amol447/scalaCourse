def pascal(c:Int,r:Int):Option[Int]={
  if(c>r){
    None
  }
  else if((c==r) | (c==1)){
    Some(1)
  }
  else{
    Some(pascal(c-1,r-1).getOrElse(0)+pascal(c,r-1).getOrElse(0))
  }
}
pascal(7,5)


def balance(chars: List[Char]): Boolean= {
  def balanceHelper(parenthesisOpenCount: Int, chars: List[Char]):
  Boolean = {
    if (parenthesisOpenCount < 0) false
    else {
      if (chars.isEmpty)  {
        if(parenthesisOpenCount != 0) false else true

      }
      else if (chars.head == '(') {
        balanceHelper(parenthesisOpenCount + 1, chars.tail)
      }
      else if (chars.head == ')') {
        balanceHelper(parenthesisOpenCount - 1, chars.tail)
      }
      else {
        balanceHelper(parenthesisOpenCount, chars.tail)
      }
    }

  }
  balanceHelper(0,chars)
}
balance("(afsdfads()()(hjdhfjk()(afdadsfd)(dfasda)())(dadfs)()fa)".toList)

def countChange(money:Int,coins:List[Int]): Int = {
  if (coins.isEmpty) {
    if (money!=0) 0 else 1
  }
  else if(money<0) 0
  else {
    countChange(money - coins.head,coins)+countChange(money,coins.tail)
  }
}
countChange(3,List(1,2))