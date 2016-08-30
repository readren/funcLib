package readren.funcLib.dataTypes;

import java.util.concurrent.Callable
import java.util.concurrent.TimeUnit
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionStage
import java.util.function.Supplier
import java.util.function.BiFunction
import java.util.concurrent.ExecutionException
import java.util.concurrent.ForkJoinPool

// Nota: tiempo después de haber hecho los ejercicios, cuando quise usar esta clase, me encontré con problemas que ahora no recuerdo, y los corregí usando CompletableFuture, una clase que vino con Java8 y no existía cuando se escribió el libro. Por ende, las respuestas del libro son muy distintas a las que hay abajo. Incluso cambié el código copiado del libro.
// IMPORTANTE: Esta implementación tiene dos problemas:
//   - provoca dead-locks cuando se anidan mas forks que el máximo pool size soportado por el ExecutorService. Por ende, para evitar este problema hay que usar implementaciones de ExecutorService que tengan pool size no acotado como, por ejemplo, Executors.newCachedThreadPool() o Executors.newWorkStealingPool(..)
//   - el hecho de usar el `Future` de java (en lugar de uno propio como en las respuestas a los ejercicios del libro) impide hacer que Par[A] sea covariant en A (Par[+A]).
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] =
    (es: ExecutorService) => CompletableFuture.completedFuture(a)

  // Agregada luego, mientras leía el capitulo 13 (Free monad) porque es necesaria para implementar `ch13_externalEffects.Free.IO[A](a: => A): IO[A]`
  /**
   * Difiere de `unit` en que el parámetro será evaluado luego, cuando se llame a `Future#get()`. Se parece a `unit` en que ambos operan de forma sincrónica (el `Par` dado ignora al `ExecutionContext`). 
   * Difiere de `async` en que el parámetro será evaluado sincrónicamente en el hilo en que se llame a `Future#get()`, en lugar de asincrónicamente en el hilo que determine el `ExecutionContext`. Se parece a `async` en que el parámetro es recibido por nombre (evaluado posteriormente).
   */
  def delay[A](a: => A): Par[A] = {
    (es: ExecutorService) =>
      CompletableFuture.completedFuture(())
        .thenApply(new java.util.function.Function[Unit, A] { def apply(u: Unit) = a })
  }
    
  @Deprecated
  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def toCompletableFuture[X](f: Future[X])(es: ExecutorService): CompletableFuture[X] = f match {
    case cs: CompletableFuture[X] => cs
    case f: Future[X] => CompletableFuture.supplyAsync(new Supplier[X] {
      def get: X =
        try {
          f.get
        } catch {
          case e1: InterruptedException => throw new RuntimeException(e1)
          case e2: ExecutionException   => throw new RuntimeException(e2)
        }
    }, es)
  }

  /**
   * Notice these implementations do not evaluate the call to `f` in a separate logical thread. This is in keeping with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
   * La implementación dada en el texto del libro no respeta los timeouts. No se si esta lo hace. Para que la implementación del libro los respete, we need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
   */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      toCompletableFuture(af)(es).thenCombine[B, C](toCompletableFuture(bf)(es), new BiFunction[A, B, C] {
        def apply(a: A, b: B): C = f(a, b);
      })
    }
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    (es: ExecutorService) => {
      val fa = pa(es)
      toCompletableFuture(fa)(es).thenCompose(new java.util.function.Function[A, CompletableFuture[B]] {
        def apply(a: A): CompletableFuture[B] = toCompletableFuture(f(a)(es))(es)
      })
    }

  /**
   * This is the simplest, most natural implementation, but there are some problems with it --for one, the outer `Callable` will block waiting for the 'inner' task to complete. Since this blocked thread occupies a thread in our thread pool or whatever resource backs the `ExecutorService`, this implies we're losing out on some potential parallelism (essentially, we are using two threads when one should do). This is a symptom of a more serious problem with the implementation that we'll discuss later in the chapter. Para resolverlo leer el capitulo "7.4.4 A fully non-blocking Par implementation using actors"
   */
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      CompletableFuture.supplyAsync(new Supplier[A] { def get = a(es).get }, es) // la versión del libro era: es.submit(new Callable[A] { def call = a(es).get })

  // también llamada lazyUnit
  def async[A](a: => A): Par[A] =
    fork(unit(a))

  //Ejercicio 3: Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future. 
  //(no lo he resuelto)

  //Ejercicio 4
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => fork(unit(f(a)))

  //Ejercicio 5 (este ejercicio ya no esta en la última versión del libro)
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    (es: ExecutorService) => toCompletableFuture(pa(es))(es).thenApply(new java.util.function.Function[A, B] {
      def apply(a: A): B = f(a)
    }) //. UnitFuture[B](f(pa(es).get))

  def map2_inTermsOfMapAndProduct[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    map(product(a, b))((p: (A, B)) => f(p._1, p._2))

  //Ejercicio 6   (este ejercicio ya no esta en la última versión del libro)
  def parMap_prim[A, B](l: List[A])(f: A => B): Par[List[B]] =
    sequence(l.map(asyncF(f)))

  //Ejercicio 7  (En la última versión del libro, este es el ejercicio 5)
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List[A]()))((pa: Par[A], acc: Par[List[A]]) => map2(pa, acc)(_ :: _))

  // En la última versión del libro esto, que antes era parte del ejercicio 7, esta desarrollado en el libro.
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
    sequence(l.map(asyncF(f)))

  // En la última versión del libro esto, que antes era parte del ejercicio 7, es el ejercicio 6.
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val rf = parMap(l)((a: A) => if (f(a)) Some(a) else None)
    map(rf)(_.filter(_.isDefined).map(_.get))
  }

  def choice[A](pc: Par[Boolean])(p1: Par[A], p2: Par[A]): Par[A] =
    es =>
      toCompletableFuture(pc(es))(es).thenCompose(new java.util.function.Function[Boolean, CompletionStage[A]] {
        def apply(a: Boolean): CompletableFuture[A] =
          if (a) toCompletableFuture(p1(es))(es)
          else toCompletableFuture(p2(es))(es)
      })

  def choiceN[A](pc: Par[Int])(lp: List[Par[A]]): Par[A] =
    es =>
      toCompletableFuture(pc(es))(es).thenCompose(new java.util.function.Function[Int, CompletionStage[A]] {
        def apply(n: Int): CompletableFuture[A] = toCompletableFuture(lp(n)(es))(es)
      })

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

}

object Tests {
  import java.util.concurrent.Executors

  def main(args: Array[String]): Unit = {
    val executor = Executors.newCachedThreadPool()
    try {
      val list = List(1, 2, 3, 4, 5)
      val pepe = Par.parMap(list)(_ * 2)
      Console.println(pepe(executor).get())
      Console.println(Par.parFilter(list)(_ % 2 == 0)(executor).get())
    } finally executor.shutdown()

    def nestNForks[A](level: Int, p: Par.Par[A]): Par.Par[A] =
      if (level > 0) nestNForks(level - 1, Par.fork(p)) else p

    println("\n\nCon WorkStealingPool(1) ");
    {
      val es = new ForkJoinPool(1, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true);
      def showStats(es: ForkJoinPool) =
        println(s"activeThreads=${es.getActiveThreadCount}, poolSize=${es.getPoolSize}, queuedSubmissions=${es.getQueuedSubmissionCount}, queuedTasks=${es.getQueuedTaskCount}, runningThreads=${es.getRunningThreadCount}, steals=${es.getStealCount}")
      val a1 = Par.async { showStats(es); "funca" }
      val a5 = nestNForks(4, a1)

      try {
        showStats(es)
        val x = a5(es)
        showStats(es)
        println(x.get(1, TimeUnit.SECONDS)) // funca
        showStats(es)
      } catch { case _: Exception => println("no funca") }
      finally es.shutdown()
    }

    print("\n\nCon FixedThreadPool(4) ");
    {
      val a0 = Par.unit("funca")
      val a5 = nestNForks(5, a0)
      val es = Executors.newFixedThreadPool(4) // con newSingleThreadScheduledExecutor() también ocurre un DEADLOCK
      try println(a5(es).get(1, TimeUnit.SECONDS)) // DEADLOCK
      catch { case _: Exception => println("no funca") }
      finally es.shutdownNow()
    }

  }
}

