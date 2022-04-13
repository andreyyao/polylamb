
val rec fib' =
 fn (n: int, a: int, b: int) : int =>
    if n = 0 then a
    else fib'(n-1, a+b, a)

val fib = fn (n : int) => fib' (n, 0, 1)

	     
