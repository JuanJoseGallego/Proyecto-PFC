/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package taller4

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import scala.util.Random


object Taller4 {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean


  //Esta funcion genera una secuencia aleatoria de tamaño n (prueba de 3 millones de elementos)
  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString //En cada posición se pega una letra de forma aleatoria
  }


//Prc = Problema en la reconstruccion de cadenas ingenuo
  def prc_ingenuo(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def cadenas_candidatas(alfabeto: Seq[Char], tamano: Int): Seq[Seq[Char]] = {
      if (tamano == 0) Seq(Seq.empty[Char]) //caso base
       else { //longitudes mayores a 0 (caso recursivo)
        alfabeto.flatMap(caracter => cadenas_candidatas(alfabeto, tamano - 1).map(caracter +: _))
      }
    }

    val combinaciones = cadenas_candidatas(alfabeto,tamano)
    //Itera entre los elementos de la secuencia y si cumple con la condicion del oraculo, retorna la secuencia.
    combinaciones.flatMap { seq =>      if (o(seq)) seq      else None}

  }

  def prc_mejorado(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k == tamano) SC
      else {
        val sck = SC.flatMap(elementos_sc => alfabeto.map(letra => elementos_sc :+ letra)).filter(o)
        subcaden_candidatas(k + 1, sck)
      }
    }

    val SC = subcaden_candidatas(0, Seq(Seq()))
    SC.head
  }

  def prc_turbo(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val SCk = SC.flatMap { s1 =>
          SC.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }
        val SCkFiltrado = SCk.filter(o)
        subcaden_candidatas(k*2 , SCkFiltrado)
      }
    }
    val Alfab = alfabeto.map(Seq(_))
    val SC = subcaden_candidatas(1, Alfab)
    SC.head

  }

  //FALTA MEJORARLO
  //def prc_turboMejorado(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {
  //}

    def main(args: Array[String]): Unit = {

      val secuencia = Seq('a', 'c', 'a', 'g')
      val tamano = 4
      val secuenciaRandom = secuenciaaleatoria(tamano)

      val o: Oraculo = (s: Seq[Char]) => {
        secuenciaRandom.containsSlice(s)
      }
      val inicio1 = System.nanoTime()
      val cadena = prc_ingenuo(alfabeto, tamano, o)
      println(s" ingenuo Cadena encontrada: $cadena")
      val fin1 = System.nanoTime()
      val tiempo1 = (fin1 - inicio1)/ 1e6

      val inicio2 = System.nanoTime()
      val cadenaM = prc_mejorado(alfabeto, tamano, o)
      println(s" mejorado Cadena encontrada: $cadenaM")
      val fin2 = System.nanoTime()
      val tiempo2 = (fin2 - inicio2)/ 1e6

      val inicio3 = System.nanoTime()
      val cadenaT = prc_turbo(alfabeto, tamano, o)
      println(s" turbo Cadena encontrada: $cadenaT")
      val fin3 = System.nanoTime()
      val tiempo3 = (fin3 - inicio3)/ 1e6

      val inicio4 = System.nanoTime()
      //val cadenaTM = prc_turboMejorado(alfabeto, tamano, o)
      //println(s" turbo Mejorado Cadena encontrada: $cadenaTM")
      val fin4 = System.nanoTime()
      val tiempo4 = (fin4 - inicio4) / 1e6




      println(s"Tiempo de ejecucion: $tiempo1 ms")
      println(s"Tiempo de ejecucion: $tiempo2 ms")
      println(s"Tiempo de ejecucion: $tiempo3 ms")
      //println(s"Tiempo de ejecucion: $tiempo4 ms")
  }
}

