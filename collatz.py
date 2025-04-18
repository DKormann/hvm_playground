#%%

# def ack(a,b ):
  # if a == 0: return b + 1
  # if b == 0: return ack(a-1, 1)
  # return ack(a-1, ack(a, b-1))

def arr(a,b,c):
  if a == 0: return b + c
  res = b
  for i in range(a):
    res = arr(a-1, res, c) 
  return res

arr(3, 3, 3)



#%%


def f(n): return n//2 if n % 2 == 0 else 3 * n + 1

def collatz(n):
  res = 0
  while n != 1:
    res += 1
    n = f(n)
    print(n)
  return res

collatz(11)


