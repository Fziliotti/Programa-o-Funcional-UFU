import Stack

b:: Stack Int
b= empty

c:: Stack Int
c= teste




main::IO()
main = do putStr(show(push 3 (push 4 (push 5 b))))
       

inserir::  a -> Stack a -> Stack a
inserir x s1 = push x s1


size:: Stack a -> Int -> Int
size s h
  |isEmpty s  = 0
  |otherwise = size (pop s) (h+1)







