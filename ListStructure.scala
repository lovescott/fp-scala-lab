package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List(Nothing)
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case (0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def tail[A](ds: List[A]): List[A] = ds match{
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] ={
      @an
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  def dropWhile[A] (l: List [A])( f: A => Boolean): List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  def setHead [A](ds: List[A], nv: A): List[A] = ds match{
    case Nil => Nil
    case Cons(_,xs) => Cons(nv, xs)
  }


  def foldRight[A,B](as: List[A], z: B)(f:(A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def foldLeft[A,B](as: List[A], z: B)(f:(B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
    foldRight(1,0)((_, acc) => acc + 1)
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))
  }

  def dToString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
  }

  def apply [A] (as: A*): List[A] =
  if(as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
}
