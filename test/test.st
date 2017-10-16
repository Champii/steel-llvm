external
  putchar(int): int
  puts(string): void

tata = (b: string) ->
  c = "mdr"
  puts(c)
  "woot"

mdr = (x: int) ->
  x

lol = (i: int, j: int) ->
  i + j


main = ->
  a = "lol"
  puts(a)
  x = tata(a)
  puts(x)
  lol(1, 42)
