module R = Arith

type normal = R.arith

val normalize : R.arith -> normal
val check_normal : R.arith -> bool