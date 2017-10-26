external
  putchar(int): int
  puts([int8]): void
  write(int, [int8], int): int

main = ->
  putchar(48)
  puts("LOL")
  write(1, "lol", 3)
  0
