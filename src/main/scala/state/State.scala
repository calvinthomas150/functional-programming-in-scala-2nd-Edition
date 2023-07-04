package fpinscala
package state

opaque type State[S, +A] = S => (A, S)

object State:
  extension[S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- run
        b <- s2
      yield f(a,b)

  def unit[S, A](a:A): State[S, A] = s => (a, s)

  def sequence[S, A](stateActions: List[State[S, A]]): State[S, List[A]] =
    stateActions.foldRight(unit(Nil: List[A]))((s, acc) => s.map2(acc)(_ :: _))

  def apply[S, A](f: S => (A, S)): State[S, A] = f



