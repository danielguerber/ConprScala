

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.Callable

object ParApp extends App {
	val exServ : ExecutorService = Executors.newCachedThreadPool()
  
	def map[A, B](l:List[A],f:A=>B):List[B]=l match{
		case Nil => Nil
		case x::xs =>f(x)::map(xs,f)
	}
	
	def mapPar[A, B](l:List[A],f:A=>B):List[B]=l match{
		case Nil => Nil
		case x::xs => {
		  val res = exServ.submit(new Callable[B]() {
			  def call: B = {f(x)}
		  	})
		  val ret = mapPar(xs,f)
		  return res.get()::ret
		}
	}
	
	def filter[A](l:List[A], p: A => Boolean): List[A] = l match{
	  case Nil => Nil
	  case x::xs => if (p(x)) x::filter(xs,p) else filter(xs,p)
	}
	
	def filterPar[A](l:List[A], p: A => Boolean): List[A] = l match{
	  case Nil => Nil
	  case x::xs => {
	    val res = exServ.submit(new Callable[Boolean]() {
			  def call: Boolean = {p(x)}
		  	})
		val ret = filterPar(xs,p)
		if(res.get()) {
		  return x::ret
		} else {
		  return ret
		}
	  }
	}
	
	def reduce[A](l:List[A], f:(A,A)=>A):A = l match{
	  case Nil => sys.error("reduce of empty list")
	  case x::xs => if (xs==Nil) x else f(x,reduce(xs,f))
	}
	
	def reducePar[A](l:List[A], f:(A,A)=>A):A = l match{
	  case Nil => sys.error("reduce of empty list")
	  case x::Nil=> x
	  case _ => {
	    val (a, b) = l.splitAt(l.length /2)
	    val ret1 = exServ.submit(new Callable[A]() {
			  def call: A = {reducePar(a,f)}
		  	})
		val ret2 = exServ.submit(new Callable[A]() {
			  def call: A = {reducePar(b,f)}
		  	})
	    return f(ret1.get, ret2.get)
	  }
	}
	
	
	
	def expensive(n:Int):Int = {
	  Thread.sleep(50)
	  return n*n
	}
	
	def expensiveP(n:Int):Boolean = {
	  Thread.sleep(50)
	  return (n%2==0)
	}
	
	def expensiveRed(x:Int, y:Int):Int = {
	  Thread.sleep(50)
	  return x*y
	}
	
	val l = List.range(1,100)
	val t1 = System.currentTimeMillis()
	println(reduce(l, expensiveRed))
	println((System.currentTimeMillis() - t1) + " ms")
	val t2 = System.currentTimeMillis()
	println(reducePar(l, expensiveRed))
	println((System.currentTimeMillis() - t2) + " ms")
}