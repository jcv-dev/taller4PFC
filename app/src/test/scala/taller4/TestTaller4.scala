/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4


import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite {

  test("Test Multiplicación") {
    type Matriz = Vector[Vector[Int]]

    val m1: Matriz = Vector(Vector(0, 1), Vector(1, 1))
    val m2: Matriz = Vector(Vector(1, 1), Vector(1, 0))

    val resMultMatriz = Taller4.multMatriz(m1, m2)
   
    assert(resMultMatriz == Vector(Vector(1, 0), Vector(2, 1)))
  }


  test("Test 4x4") {
    type Matriz = Vector[Vector[Int]]
    val m1: Matriz = Vector(Vector(1, 0, 1, 0), Vector(0, 1, 0, 1), Vector(1, 0, 1, 0), Vector(1, 1, 1, 0))
    val m2: Matriz = Vector(Vector(1, 1, 1, 0), Vector(0, 1, 0, 1), Vector(1, 0, 1, 0), Vector(1, 1, 1, 0))

    val resMultMatriz = Taller4.multMatriz(m1, m2)
    val resMultPar = Taller4.multMatrizPar(m1, m2)
    val resRec = Taller4.multMatrizRec(m1, m2)
    val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resStrassen = Taller4.multStrassen(m1, m2)
    val resStrassenPar = Taller4.multStrassenPar(m1, m2)
    val resEsperado = Taller4.multMatriz(m1,m2)

    assert(resMultMatriz == resEsperado)
    assert(resMultPar == resEsperado)
    assert(resRec == resEsperado)
    assert(resRecParalelo == resEsperado)
    assert(resStrassen == resEsperado)
    assert(resStrassenPar == resEsperado)
  }

  test("Test 8x8") {
    type Matriz = Vector[Vector[Int]]

    val m1: Matriz = Vector(
      Vector(0, 1, 1, 0, 1, 0, 1, 1),
      Vector(1, 0, 1, 0, 1, 1, 0, 1),
      Vector(1, 1, 0, 0, 0, 1, 1, 0),
      Vector(0, 1, 1, 1, 0, 0, 1, 1),
      Vector(0, 0, 1, 0, 1, 1, 0, 0),
      Vector(1, 1, 1, 0, 0, 1, 1, 1),
      Vector(0, 0, 1, 1, 1, 0, 1, 0),
      Vector(1, 1, 0, 1, 0, 1, 0, 0)
    )


    val m2: Matriz = Vector(
      Vector(1, 0, 1, 1, 0, 1, 0, 1),
      Vector(0, 1, 0, 1, 1, 0, 1, 0),
      Vector(1, 1, 0, 0, 1, 0, 1, 1),
      Vector(0, 1, 1, 1, 0, 1, 0, 0),
      Vector(1, 0, 1, 0, 0, 1, 1, 1),
      Vector(0, 1, 0, 1, 1, 0, 0, 1),
      Vector(1, 0, 1, 0, 0, 1, 1, 0),
      Vector(0, 1, 1, 1, 1, 0, 0, 0)
    )

    val resMultMatriz = Taller4.multMatriz(m1, m2)
    val resMultPar = Taller4.multMatrizPar(m1, m2)
    val resRec = Taller4.multMatrizRec(m1, m2)
    val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resStrassen = Taller4.multStrassen(m1, m2)
    val resStrassenPar = Taller4.multStrassenPar(m1, m2)
    val resEsperado = Taller4.multMatriz(m1,m2)

    assert(resMultMatriz ==resEsperado)
    assert(resMultPar ==resEsperado)
    assert(resRec ==resEsperado)
    assert(resRecParalelo ==resEsperado)
    assert(resStrassen ==resEsperado)
    assert(resStrassenPar ==resEsperado)
  }
  test("Test 1024x1024") {

    type Matriz = Vector[Vector[Int]]

    val m1 = Taller4.generarMatriz(1024, 1024)
    val m2 = Taller4.generarMatriz(1024, 1024)

    val resMultMatriz = Taller4.multMatriz(m1, m2)
    val resMultPar = Taller4.multMatrizPar(m1, m2)
    val resRec = Taller4.multMatrizRec(m1, m2)
    val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resStrassen = Taller4.multStrassen(m1, m2)
    val resStrassenPar = Taller4.multStrassenPar(m1, m2)
    val resEsperado = Taller4.multMatriz(m1,m2)

    assert(resMultMatriz == resEsperado)
    assert(resMultPar == resEsperado)
    assert(resRec == resEsperado)
    assert(resRecParalelo == resEsperado)
    assert(resStrassen == resEsperado)
    assert(resStrassenPar == resEsperado)
  }  
 test("Test Matriz de Ceros") {
  
  val size = 4
  val m1 = Vector.fill(size, size)(0)
  val m2 = Vector.fill(size, size)(0)


  val resMultMatriz = Taller4.multMatriz(m1, m2)
  val resMultPar = Taller4.multMatrizPar(m1, m2)
  val resRec = Taller4.multMatrizRec(m1, m2)
  val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
  val resStrassen = Taller4.multStrassen(m1, m2)
  val resStrassenPar = Taller4.multStrassenPar(m1, m2)
  val resEsperado = Taller4.multMatriz(m1, m2)

  assert(resMultMatriz == resEsperado)
  assert(resMultPar == resEsperado)
  assert(resRec == resEsperado)
  assert(resRecParalelo == resEsperado)
  assert(resStrassen == resEsperado)
  assert(resStrassenPar == resEsperado)
}

test("Test Matriz Identidad") {
  val matrizIdentidad = (size: Int) => Vector.tabulate(size, size)((i, j) => if (i == j) 1 else 0)

  val size = 4

  val m1 = matrizIdentidad(size)
  val m2 = matrizIdentidad(size)

  val resEsperado = Taller4.multMatriz(m1, m2)

  val resMultMatriz = Taller4.multMatriz(m1, m2)
  val resMultPar = Taller4.multMatrizPar(m1, m2)
  val resRec = Taller4.multMatrizRec(m1, m2)
  val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
  val resStrassen = Taller4.multStrassen(m1, m2)
  val resStrassenPar = Taller4.multStrassenPar(m1, m2)

  assert(resMultMatriz == resEsperado)
  assert(resMultPar == resEsperado)
  assert(resRec == resEsperado)
  assert(resRecParalelo == resEsperado)
  assert(resStrassen == resEsperado)
  assert(resStrassenPar == resEsperado)
}

test("Test 1x1") {
  val size = 1 

  val m1 = Vector(Vector(5))
  val m2 = Vector(Vector(2))

  val resEsperado = Taller4.multMatriz(m1, m2)

  val resMultMatriz = Taller4.multMatriz(m1, m2)
  val resMultPar = Taller4.multMatrizPar(m1, m2)
  val resRec = Taller4.multMatrizRec(m1, m2)
  val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
  val resStrassen = Taller4.multStrassen(m1, m2)
  val resStrassenPar = Taller4.multStrassenPar(m1, m2)

  assert(resMultMatriz == resEsperado)
  assert(resMultPar == resEsperado)
  assert(resRec == resEsperado)
  assert(resRecParalelo == resEsperado)
  assert(resStrassen == resEsperado)
  assert(resStrassenPar == resEsperado)
}

import scala.util.Random

test("Test Valores Random") {
  val size = 8
  val m1 =  Vector.fill(size, size)(Random.nextInt(10)) 
  val m2 = m1.transpose

  val resEsperado = Taller4.multMatriz(m1, m2)

  val resMultMatriz = Taller4.multMatriz(m1, m2)
  val resMultPar = Taller4.multMatrizPar(m1, m2)
  val resRec = Taller4.multMatrizRec(m1, m2)
  val resRecParalelo = Taller4.multMatrizRecPar(m1, m2)
  val resStrassen = Taller4.multStrassen(m1, m2)
  val resStrassenPar = Taller4.multStrassenPar(m1, m2)

  assert(resMultMatriz == resEsperado)
  assert(resMultPar == resEsperado)
  assert(resRec == resEsperado)
  assert(resRecParalelo == resEsperado)
  assert(resStrassen == resEsperado)
  assert(resStrassenPar == resEsperado)
}

}
