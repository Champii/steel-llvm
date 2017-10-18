external
  putchar(int): int
  puts(string): void

toto = (i: [int]) -> i
lol = (i: int, j: int) -> i + j
mdr = (j: string) ->
  c = j
  puts(c)
  2

main = ->
  a = 40
  z = 2
  b = "lol"
  v = [1, 2, 3]
  mdr(b)
  puts(b)
  toto(v)
  lol(a, z)
