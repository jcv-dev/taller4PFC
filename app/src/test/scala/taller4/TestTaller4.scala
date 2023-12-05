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

    val resultadoNormal = Taller4.multMatriz(m1, m2)
    val resultadoParalelo = Taller4.multMatrizPar(m1, m2)
    val resultadoRecursivo = Taller4.multMatrizRec(m1, m2)
    val resultadoRecursivoParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resultadoStrassen = Taller4.multStrassen(m1, m2)
    val resultadoStrassenParalelo = Taller4.multStrassenPar(m1, m2)
    val resEsperado =  Vector(Vector(1, 0), Vector(2, 1))

    assert(Taller4.multMatriz(m1,m2) == resEsperado)
    assert(Taller4.multMatrizPar(m1,m2) == resEsperado)
    assert(resultadoRecursivo == resEsperado)
    assert(resultadoRecursivoParalelo == resEsperado)
    assert(resultadoStrassen == resEsperado)
    assert(resultadoStrassenParalelo == resEsperado)
  }
  test("Test 4x4") {
    type Matriz = Vector[Vector[Int]]
    val m1: Matriz = Vector(Vector(1, 0, 1, 0), Vector(0, 1, 0, 1), Vector(1, 0, 1, 0), Vector(1, 1, 1, 0))
    val m2: Matriz = Vector(Vector(1, 1, 1, 0), Vector(0, 1, 0, 1), Vector(1, 0, 1, 0), Vector(1, 1, 1, 0))

    val resultadoNormal = Taller4.multMatriz(m1, m2)
    val resultadoParalelo = Taller4.multMatrizPar(m1, m2)
    val resultadoRecursivo = Taller4.multMatrizRec(m1, m2)
    val resultadoRecursivoParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resultadoStrassen = Taller4.multStrassen(m1, m2)
    val resultadoStrassenParalelo = Taller4.multStrassenPar(m1, m2)

    assert(resultadoNormal == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
    assert(resultadoParalelo == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
    assert(resultadoRecursivo == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
    assert(resultadoRecursivoParalelo == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
    assert(resultadoStrassen == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
    assert(resultadoStrassenParalelo == Vector(Vector(2, 1, 2, 0), Vector(1, 2, 1, 1), Vector(2, 1, 2, 0), Vector(2, 2, 2, 1)))
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

    val resultadoNormal = Taller4.multMatriz(m1, m2)
    val resultadoParalelo = Taller4.multMatrizPar(m1, m2)
    val resultadoRecursivo = Taller4.multMatrizRec(m1, m2)
    val resultadoRecursivoParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resultadoStrassen = Taller4.multStrassen(m1, m2)
    val resultadoStrassenParalelo = Taller4.multStrassenPar(m1, m2)

    assert(resultadoNormal == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
    assert(resultadoParalelo == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
    assert(resultadoRecursivo == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
    assert(resultadoRecursivoParalelo == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
    assert(resultadoStrassen == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
    assert(resultadoStrassenParalelo == Vector(Vector(3, 3, 3, 2, 3, 2, 4, 2), Vector(3, 3, 3, 3, 3, 2, 2, 4), Vector(2, 2, 2, 3, 2, 2, 2, 2), Vector(2, 4, 3, 3, 3, 2, 3, 1), Vector(2, 2, 1, 1, 2, 1, 2, 3), Vector(3, 4, 3, 4, 4, 2, 3, 3), Vector(3, 2, 3, 1, 1, 3, 3, 2), Vector(1, 3, 2, 4, 2, 2, 1, 2)))
  }
 /* test("Test 1024*1024") {


    import scala.util.Random
    type Matriz = Vector[Vector[Int]]

    // Genera una matriz de tamaño n x m con valores aleatorios de 0 y 1
    def generarMatriz(n: Int, m: Int): Matriz = {
      Vector.tabulate(n) { _ =>
        Vector.tabulate(m) { _ =>
          Random.nextInt(2)
        }
      }
    }

    val m1 = generarMatriz(1024, 1024)
    val m2 = generarMatriz(1024, 1024)

    val resultadoNormal = Taller4.multMatriz(m1, m2)
    val resultadoParalelo = Taller4.multMatrizPar(m1, m2)
    val resultadoRecursivo = Taller4.multMatrizRec(m1, m2)
    val resultadoRecursivoParalelo = Taller4.multMatrizRecPar(m1, m2)
    val resultadoStrassen = Taller4.multStrassen(m1, m2)
    val resultadoStrassenParalelo = Taller4.multStrassenPar(m1, m2)

    assert(resultadoNormal == resEsperado)
    assert(resultadoParalelo == resEsperado)
    assert(resultadoRecursivo == resEsperado)
    assert(resultadoRecursivoParalelo == resEsperado)
    assert(resultadoStrassen == resEsperado)
    assert(resultadoStrassenParalelo == resEsperado)
  }
*/
}
