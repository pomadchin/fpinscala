package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)

  def double(rng: RNG): (Double, RNG) = 
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int,Double), RNG) = 
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    def rec(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if(count <= 0) (xs, r)
      else
        val (x, r1) = r.nextInt
        rec(count - 1, r1, x :: xs)

    rec(count, rng, Nil)

  def intsFold(count: Int)(rng: RNG): (List[Int], RNG) = 
    (0 until count).foldLeft((List.empty[Int], rng)) {
      case ((acc, r1), _) => 
        val (i, r2) = nonNegativeInt(r1)
        (i +: acc, r2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { r0 =>
    val (a, r1) = ra(r0)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = 
    rs.foldRight(unit(List.empty[A]))((r, acc) => map2(r, acc)(_ :: _))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = { r0 =>
    val (a, r1) = r(r0)
    f(a)(r1)
  }

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = 
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra) { a =>
      flatMap(rb) { b =>
        unit(f(a, b))
      }
    }

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] = 
      s => 
        val (a, ss) = run(s)
        (f(a), ss)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- underlying
        b <- sb
      } yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, ss) = run(s)
        f(a)(ss)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = s => (a, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)
  
  def get[S]: State[S, S] = s => (s, s)

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
    val res: State[Machine, List[Unit]] = State.traverse(inputs) { i => 
      for {
        s <- State.get
        _ <- State.set(update(i)(s))
      } yield ()
    }

    res.flatMap(_ => State.get).map(s => s.coins -> s.candies)

  def update(i: Input) = { (s: Machine) => 
    (i, s) match
      case (_, Machine(_, 0, _)) => s
      case (Input.Coin, Machine(false, _, _)) => s
      case (Input.Turn, Machine(true, _, _)) => s
      case (Input.Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Input.Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
  }
