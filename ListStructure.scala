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


  def apply [A] (as: A*): List[A] =
  if(as.isEmpty) Nil
  else Cons(as.head, apply(as.tail: _*))
}
