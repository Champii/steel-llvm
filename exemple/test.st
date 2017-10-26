external
  putchar(int): int
  puts([int8]): void
  write(int, [int8], int): int
  read(int, [int8], int): int

# strlen = (str: [int8]) ->
#   i = 0
#   for s in str => i++
#   i

# class Console
#   stdin = 0
#   stdout = 1
#   stderr = 2
#   constructor = -> 2
#   in  = (str: [int8]) -> read(@stdin, str, strlen(str))
#   out = (str: [int8]) -> write(@stdout, str, strlen(str))
#   err = (str: [int8]) -> write(@stderr, str, strlen(str))

# class Net
#   createServer = ->

# class Http : Net

class Toto
  a: int
  b: int

lol = (t: int) -> t

mdr = (p: Toto) -> p.a = p.a + 3

main = ->
  putchar(48)
  putchar(49)
  putchar(10)
  0
