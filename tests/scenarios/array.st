external
  putchar(int): int

main = ->
  a = [48, 49, 50]
  putchar(a[0])
  putchar(a[1])
  putchar(a[2])
  a[0] = 51
  putchar(a[0])
  0
