package fpinscala.exercises.laziness

enum LazyList[+A]:
  import LazyList.*

  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = 
    @annotation.tailrec
    def go(ll: LazyList[A], acc: List[A]): List[A] = ll match 
      case Empty => acc.reverse
      case Cons(h, t) => go(t(), h() :: acc)
    go(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match 
    case Cons(h, t) if n > 0  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty) 
    case _                    => empty

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match 
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  def forAll(p: A => Boolean): Boolean = foldRight(true)((l, acc) => p(l) && acc)

  def headOption: Option[A] = foldRight(Option.empty[A])((l, _) => Option(l))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }

  def startsWith[B](s: LazyList[B]): Boolean = zipAll(s).takeWhile(_(1).isDefined).forAll((l, r) => l == r)


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = 
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] = 
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = 
    def go(current: Int, next: Int): LazyList[Int] =
      cons(current, go(next, current + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match 
    case Some((h, s)) => cons(h, unfold(s)(f))
    case _            => empty

  lazy val fibsViaUnfold: LazyList[Int] = 
    unfold((0, 1)) { case (curr, next) => Some((curr, (next, curr + next))) }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
