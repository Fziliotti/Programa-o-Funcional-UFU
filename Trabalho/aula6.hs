fatorial' 0 r = r
fatorial' n r = fatorial' (n-1) (n*r)


fib n
  |n==0 = 0
  |n==1 = 1
  |n>1 = fib(n-2)+(n-1)

  fib' 0 x y = x
  fib' 1 x y = y
  


