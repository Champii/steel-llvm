external
  putchar(int): int
  puts([int8]): void

class Toto
  a: int
  b: int

lol = (t: int) -> t

mdr = (p: Toto) -> p.a = p.a + 3

main = ->
  d = [48, 2]
  d[1] = 3
  c = d[0]
  z = {a: d[0], b: 2}
  z.a = 50
  mdr(z)
  putchar(z.a)
  2
