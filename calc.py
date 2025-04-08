

Nroot = 4

def expand(ctx:int):
  if ctx == 0: return 0
  return expand(ctx >> 1) << 2 | ctx & 1

def contract(ctx:int):
  if ctx == 0: return 0
  return contract(ctx >> 2) << 1 | ctx & 1

class Root:
  def __init__(self, id:int):
    assert 0 <= id < Nroot
    self.id = id

  def __eq__(self, other): return self.id == other.id

  def __repr__(self): return "\n".join(self.rep())

  def rep(self): return [f"Root({self.id})"]

class Node(Root):
  def __init__(self, a: Root, b:Root):
    if a.id > b.id: a, b = b, a
    self.a, self.b = a, b
    a,b = a.id, b.id
    b -= a
    id = Nroot
    id = expand(a) << 1 | expand(b)
    self.id = id + Nroot
  
  def __eq__(self, other):
    if not isinstance(other, Node): return False
    return self.a == other.a and self.b == other.b
  
  def rep(self):
    return [
      f"Node {self.id}",
      *[f"  {x}" for x in  self.a.rep()+ self.b.rep()],
    ]

def decode(id:int):
  if id < Nroot: return Root(id)
  id -= Nroot
  a = contract(id >> 1)
  b = contract(id) + a
  return Node(decode(a), decode(b))

r0 = Root(0)
r1 = Root(1)
r2 = Root(2)
r3 = Root(3)

n1 = Node(r0, r0)
n2 = Node(r0, n1)
n3 = Node(n1,n1)
n3_ = Node(n1,n1)

n3 = Node(n2, n3)

nz = decode(n3.id)
print(n3.id, nz == n3)
