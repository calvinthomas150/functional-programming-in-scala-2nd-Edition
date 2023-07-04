import fpinscala.state.SimpleRNG

val rng = SimpleRNG(16159453)

val(n, rng1) = rng.nextInt
n
rng1.nextInt