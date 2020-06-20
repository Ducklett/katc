for (x in 1..100) case {
  x % 3 && x % 5: print("fizzbuzz")
  x % 3: print("fizz")
  x % 5: print("buzz")
  default: print(x)
}