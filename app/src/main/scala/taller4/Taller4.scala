/**
  * Taller 4: Multiplicación de Matrices 
  * @author Juan Camilo Valencia Rivas - 2259459, Juan Manuel Arango Rodas - 2259571
  * Profesor: Carlos Andrés Delgado
  */

package taller4
import common._
import scala.util.Random
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer


object Taller4{

 type Matriz = Vector[Vector[Int]]
 val random = new Random


 def matrizAlAzar(long: Int, vals: Int): Matriz = {
  val v = Vector.fill (long, long) {random.nextInt(vals)}
  v
 }


 def vectorAlAzar(long: Int, vals: Int): Vector[Int] = {
  val v = Vector.fill (long){random.nextInt(vals)}
  v
 }


 def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
  (v1 zip v2).map({
   case(i, j)
   => (i * j)
  }).sum
 }


 def transpuesta(m: Matriz): Matriz = {
  val l = m.length
    Vector.tabulate (l, l)((i, j) => m(j)(i))
 }


 def multMatriz(m1: Matriz, m2: Matriz): Matriz = {

  Vector.tabulate(m1.length, transpuesta(m2).length) {
   (i, j) => prodPunto(m1(i), transpuesta(m2)(j))
  }
 }

 def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
  
  val resultados = Vector.tabulate(m1.length, transpuesta(m2).length) {
   (i, j) => task {prodPunto(m1(i), transpuesta(m2)(j))}
  }

  resultados.map(row => row.map(_.join()))
 }


 def subMatriz(m: Matriz, i: Int, j: Int, l: Int): Matriz = {
  Vector.tabulate(l) { row =>
   Vector.tabulate(l) { col =>
    m(i + row)(j + col)
   }
  }
 }


 def sumMatriz(m1: Matriz, m2: Matriz): Matriz = {


   def sumaInterna(v1: Vector[Int], v2: Vector[Int]): Vector[Int] =
    Vector.tabulate(v1.length)(i => v1(i) + v2(i))


   def sumaMatrices(m1: Matriz, m2: Matriz): Matriz = {
    def sumaInternaMatrices(x1: Matriz, x2: Matriz): Matriz =
     Vector.tabulate(x1.length)(i => sumaInterna(x1(i), x2(i)))

    sumaInternaMatrices(m1, m2)
   }

   sumaMatrices(m1, m2)
 }


 def multMatrizRec(m1: Matriz, m2: Matriz): Matriz = {

  val n = m1.length

  if (n == 1) Vector(Vector(m1(0)(0) * m2(0)(0)))
  else {
   val (a11, a12, a21, a22) = (
     subMatriz(m1, 0, 0, n / 2),
     subMatriz(m1, 0, n / 2, n / 2),
     subMatriz(m1, n / 2, 0, n / 2),
     subMatriz(m1, n / 2, n / 2, n / 2)
   )
   val (b11, b12, b21, b22) = (
     subMatriz(m2, 0, 0, n / 2),
     subMatriz(m2, 0, n / 2, n / 2),
     subMatriz(m2, n / 2, 0, n / 2),
     subMatriz(m2, n / 2, n / 2, n / 2)
   )
   val c11 = sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21))
   val c12 = sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22))
   val c21 = sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21))
   val c22 = sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

   Vector.tabulate(n) { i =>
    Vector.tabulate(n) { j =>
     if (i < n / 2 && j < n / 2) c11(i)(j)
     else if (i < n / 2 && j >= n / 2) c12(i)(j - n / 2)
     else if (i >= n / 2 && j < n / 2) c21(i - n / 2)(j)
     else c22(i - n / 2)(j - n / 2)
    }
   }
  }
 }


 def multMatrizRecPar(m1: Matriz, m2: Matriz): Matriz = {
  val n = m1.length
  if (n == 1) {
   Vector(Vector(m1(0)(0) * m2(0)(0)))
  } else {
   val (a11, a12, a21, a22) = parallel(
   subMatriz(m1, 0, 0, n / 2),
   subMatriz(m1, 0, n / 2, n / 2),
   subMatriz(m1, n / 2, 0, n / 2),
   subMatriz(m1, n / 2, n / 2, n / 2))

   val (b11, b12, b21, b22) = parallel (subMatriz(m2, 0, 0, n / 2), subMatriz(m2, 0, n / 2, n / 2), subMatriz(m2, n / 2, 0, n / 2), subMatriz(m2, n / 2, n / 2, n / 2))

   val (c11, c12, c21, c22) = parallel(
   sumMatriz(multMatrizRec(a11, b11), multMatrizRec(a12, b21)),
   sumMatriz(multMatrizRec(a11, b12), multMatrizRec(a12, b22)),
   sumMatriz(multMatrizRec(a21, b11), multMatrizRec(a22, b21)),
   sumMatriz(multMatrizRec(a21, b12), multMatrizRec(a22, b22))

   )

   Vector.tabulate(n) { i =>
    Vector.tabulate(n) { j =>
     if (i < n / 2 && j < n / 2) c11(i)(j)
     else if (i < n / 2 && j >= n / 2) c12(i)(j - n / 2)
     else if (i >= n / 2 && j < n / 2) c21(i - n / 2)(j)
     else c22(i - n / 2)(j - n / 2)
    }
   }
  }
 }


 def restaMatriz(m1: Matriz, m2: Matriz): Matriz = {

  def restaMatrices(m1: Matriz, m2: Matriz): Matriz = {

   def restaInterna(v1: Vector[Int], v2: Vector[Int]): Vector[Int] =
    Vector.tabulate(v1.length)(i => v1(i) - v2(i))

   def restaInternaMatrices(x1: Matriz, x2: Matriz): Matriz =
    Vector.tabulate(x1.length)(i => restaInterna(x1(i), x2(i)))

   restaInternaMatrices(m1, m2)
  }

  restaMatrices(m1, m2)
 }


 def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
  val n = m1.length
  if (n == 1) {
   Vector(Vector(m1(0)(0) * m2(0)(0)))
  } else {

   val l = n / 2
   val a11 = subMatriz(m1, 0, 0, l)
   val a12 = subMatriz(m1, 0, l, l)
   val a21 = subMatriz(m1, l, 0, l)
   val a22 = subMatriz(m1, l, l, l)

   val b11 = subMatriz(m2, 0, 0, l)
   val b12 = subMatriz(m2, 0, l, l)
   val b21 = subMatriz(m2, l, 0, l)
   val b22 = subMatriz(m2, l, l, l)

   val s1 = restaMatriz(b12, b22)
   val s2 = sumMatriz(a11, a12)
   val s3 = sumMatriz(a21, a22)
   val s4 = restaMatriz(b21, b11)
   val s5 = sumMatriz(a11, a22)
   val s6 = sumMatriz(b11, b22)
   val s7 = restaMatriz(a12, a22)
   val s8 = sumMatriz(b21, b22)
   val s9 = restaMatriz(a11, a21)
   val s10 = sumMatriz(b11, b12)

   val p1 = multStrassen(a11, s1)
   val p2 = multStrassen(s2, b22)
   val p3 = multStrassen(s3, b11)
   val p4 = multStrassen(a22, s4)
   val p5 = multStrassen(s5, s6)
   val p6 = multStrassen(s7, s8)
   val p7 = multStrassen(s9, s10)

   val c11 = sumMatriz(restaMatriz(sumMatriz(p5,p4),p2),p6)
   val c12 = sumMatriz(p1, p2)
   val c21 = sumMatriz(p3, p4)
   val c22 = restaMatriz(restaMatriz(sumMatriz(p5,p1),p3),p7)

   Vector.tabulate(n) { i =>
    Vector.tabulate(n) { j =>
     if (i < l && j < l) c11(i)(j)
     else if (i < l && j >= l) c12(i)(j - l)
     else if (i >= l && j < l) c21(i - l)(j)
     else c22(i - l)(j - l)
    }
   }
  }
 }

 def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
  val n = m1.length
  if (n == 1) {
   Vector(Vector(m1(0)(0) * m2(0)(0)))
  } else {
   val l = n / 2

   val (a11, a12, a21, a22) = parallel(
    subMatriz(m1, 0, 0, l), subMatriz(m1, 0, l, l),
    subMatriz(m1, l, 0, l), subMatriz(m1, l, l, l)
   )

   val (b11, b12, b21, b22) = parallel(
    subMatriz(m2, 0, 0, l), subMatriz(m2, 0, l, l),
    subMatriz(m2, l, 0, l), subMatriz(m2, l, l, l)
   )

   val (s1, s2, s3, s4) = parallel(
    restaMatriz(b12, b22),
    sumMatriz(a11, a12),
    sumMatriz(a21, a22),
    restaMatriz(b21, b11)
   )
   val (s5, s6, s7, s8) = parallel(
    sumMatriz(a11, a22),
    sumMatriz(b11, b22),
    restaMatriz(a12, a22),
    sumMatriz(b21, b22),

   )
   val (s9, s10) = parallel(
    restaMatriz(a11, a21),
    sumMatriz(b11, b12)
   )

   val ( p5, p6) = parallel(
    multStrassen(s5, s6),
    multStrassen(s7, s8)
   )
   val (p1, p2, p3, p4) = parallel(
    multStrassen(a11, s1),
    multStrassen(s2, b22),
    multStrassen(s3, b11),
    multStrassen(a22, s4)

   )
   val p7 = multStrassen(s9, s10)

   val (c11, c12, c21, c22) = parallel(
    sumMatriz(restaMatriz(sumMatriz(p5, p4), p2), p6),
    sumMatriz(p1, p2),
    sumMatriz(p3, p4),
    restaMatriz(restaMatriz(sumMatriz(p5, p1), p3), p7)
   )

   Vector.tabulate(n) { i =>
    Vector.tabulate(n) { j =>
     if (i < l && j < l) c11(i)(j)
     else if (i < l && j >= l) c12(i)(j - l)
     else if (i >= l && j < l) c21(i - l)(j)
     else c22(i - l)(j - l)
    }
   }
  }
 }


 
  def generarMatriz(n: Int, m: Int): Matriz = {
   Vector.tabulate(n) { _ =>
    Vector.tabulate(m) { _ =>
     Random.nextInt(2)
    }
   }
  }

 def main(args: Array[String]): Unit = {

  type Matriz = Vector[Vector[Int]]


  val m1 = generarMatriz(8, 8)
  val m2 = generarMatriz(8, 8)

  measureAndPrint("Multiplicación Normal", multMatriz, m1, m2)
  measureAndPrint("Multiplicación Paralela", multMatrizPar, m1, m2)
  measureAndPrint("Multiplicación Recursiva", multMatrizRec, m1, m2)
  measureAndPrint("Multiplicación Recursiva Paralela", multMatrizRecPar, m1, m2)
  measureAndPrint("Multiplicación Strassen", multStrassen, m1, m2)
  measureAndPrint("Multiplicación Strassen Paralela", multStrassenPar, m1, m2)
}

def measureAndPrint[T](label: String, func: (Matriz, Matriz) => Matriz, m1: Matriz, m2: Matriz): Unit = {
  val time = withWarmer(new Warmer.Default) measure {
    func(m1, m2)
  }
  println(s"$label: $time")
}

}
