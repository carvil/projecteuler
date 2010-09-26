import List
 
digits n 
{-  123->[3,2,1]
 -}
    |n<10=[n]
    |otherwise= y:digits x 
    where
    (x,y)=divMod n 10
-- 123 ->321
dmm=(\x y->x*10+y)
palind n=foldl dmm 0 (digits n) 
 
isOdd x=(length$takeWhile odd x)==(length x)
isOdig x=isOdd m && s<=h
    where
    k=x+palind x
    m=digits k
    y=floor$logBase 10 $fromInteger x
    ten=10^y
    s=x`mod`10
    h=x`div`ten
 
a2=[i|i<-[10..99],isOdig i]
aa2=[i|i<-[10..99],isOdig i,mod i 10/=0]
a3=[i|i<-[100..999],isOdig i]
m5=[i|i1<-[0..99],i2<-[0..99],
      let i3=i1*1000+3*100+i2,
      let i=10^6*   8+i3*10+5,
      isOdig i
   ]
 
fun i
    |i==2  =2*le aa2
    |even i=(fun 2)*d^(m-1)
    |i==3  =2*le a3
    |i==7  =fun 3*le m5
    |otherwise=0
    where
    le=length
    m=div i 2
    d=2*le a2
 
problem_145 = sum[fun a|a<-[1..9]]

