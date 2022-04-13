
val rec even =
 fn (n: int) : bool =>
    if n = 0 then true
    else if n = 1 then false
    else if n > 0 then even (n - 2)
    else even (n + 2)
