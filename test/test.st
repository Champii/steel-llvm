external
  putchar(int): int
  puts([int8]): void

class Toto
  a: int
  b: int

lol = (t: int) -> t

main = ->
  # a = [1, 48]
  # mdr(a)
  z = a: 1, b: 48
  putchar(lol(z.b + z.a))
  2
