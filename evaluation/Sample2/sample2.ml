let rec mul acc x y = match y with
  | 0 -> acc
  | y -> mul (acc+x) (x) (y-1)
in
print_int (mul 0 12345 54321)
