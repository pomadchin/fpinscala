package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(ds: List[Double]): Double = ds match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val x = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]) =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Cons(_, tail) => tail
    case _             => sys.error("tail of a head with no tail")

  def setHead[A](l: List[A], h: A): List[A] = l match
      case Nil => sys.error("setHead on empty list")
      case Cons(_, t)  => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l
    else
      l match 
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case l                  => l

  // drop head, make a copy of the rest
  def init[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match 
    case Nil        => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f) 

  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns, 1d, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A](), (acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A](), append)

  def incrementEach(l: List[Int]): List[Int] = map(l)(_ + 1)

  def doubleToString(l: List[Double]): List[String] = map(l)(_.toString)

  def map[A,B](l: List[A])(f: A => B): List[B] = l match 
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(h, t) => 
      if f(h) then Cons(h, filter(t)(f)) 
      else filter(t)(f)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match 
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addPairwise(ta, tb))

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = (a, b) match 
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb, f))

  // a stacksafe version of a zipWith function
  def zipWith_stacksafe[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = 
    def rec(l: List[A], r: List[B], acc: List[C]): List[C] = (l, r) match
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => rec(ta, tb, Cons(f(ha, hb), acc))

    rec(a, b, List[C]())

  @annotation.tailrec
  def startsWith[A](list: List[A], prefix: List[A]): Boolean = (list, prefix) match
    // any list starts with Nil
    case (_, Nil) => true
    case (Cons(hl, tl), Cons(hp, tp)) if hl == hp => startsWith(tl, tp)
    case _ => false

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match
    // trivial case
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    // skip if the head does not match, mb we can find smth else?
    case Cons(h, t) => hasSubsequence(t, sub)
