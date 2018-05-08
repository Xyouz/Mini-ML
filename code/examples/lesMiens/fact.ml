let rec f (x:int) = if x = 0 then 1 else x* (f (x - 1))

let _ = f 5


