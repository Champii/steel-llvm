external
  putchar(int): int

class Foo
  a: int
  b: int

f = (it: Foo) -> it.a + it.b
f2 = (it: Foo) -> it.a = it.a + 1

main = ->
  st = {a: 48, b: 2}
  z = a: 48, b: 2
  y =
    a: 48
    b: 2
  z =
    a: 48, b: 2
  x = {
    a: 48
    b: 2
  }
  f2(st)
  putchar(f(st))
  # this should work:
  # putchar(f({a: 48, b: 2}))
  0
