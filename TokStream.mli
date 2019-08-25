type 'a front = Cons of 'a * (unit -> 'a front) | Nil
and 'a stream = unit -> 'a front
val force : (unit -> 'a) -> 'a