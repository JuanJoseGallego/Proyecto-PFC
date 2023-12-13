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

  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString
  }



  def prc_ingenuo(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def cadenas_candidatas(alfabeto: Seq[Char], tamano: Int): Seq[Seq[Char]] = {
      if (tamano == 0) {
        Seq(Seq.empty[Char])
      } else {
        alfabeto.flatMap(caracter => cadenas_candidatas(alfabeto, tamano - 1).map(caracter +: _))
      }
    }
    val combinaciones = cadenas_candidatas(alfabeto,tamano)
    combinaciones.flatMap { seq =>
      if (o(seq)) seq
      else None
    }
  }

  def prc_mejorado(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > tamano) SC
      else {
        subcaden_candidatas(k + 1, SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o))
      }
    }

    val SC = subcaden_candidatas(1, Seq(Seq()))
    SC.find(_.length == tamano).getOrElse(Seq())
  }

  def prc_turbo(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {
    def subcaden_candidataspar(k: Int, n:Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > tamano) SC
      else {
        val n = k*2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcaden_candidataspar(k+1 , n, SCk)
      }
    }

    val SC = subcaden_candidataspar(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == tamano).getOrElse(Seq())

  }

  //FALTA MEJORARLO
  def prc_turboMejorado(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {

    def subcaden_candidataspar(k: Int, n: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k > tamano) SC
      else {
        val n = k * 2
        val SCk = SC.flatMap(subc => alfabeto.map(letra => subc :+ letra)).filter(o)
        subcaden_candidataspar(k + 1, n, SCk)
      }
    }

    val SC = subcaden_candidataspar(1, 1, Seq(Seq.empty[Char]))
    SC.find(_.length == tamano).getOrElse(Seq())

  }


    def main(args: Array[String]): Unit = {
      val secuencia = Seq('a', 'c', 'a', 'c','g','t')

      val o: Oraculo = (s: Seq[Char]) => {
        secuencia.containsSlice(s)
      }
      val inicio1 = System.nanoTime()
      val cadena = prc_ingenuo(alfabeto, 6, o)
      println(s" ingenuo Cadena encontrada: $cadena")
      val fin1 = System.nanoTime()
      val tiempo1 = (fin1 - inicio1)/ 1e6
      println(s"Tiempo de ejecucion: $tiempo1 ms")

      val inicio2 = System.nanoTime()
      val cadenaM = prc_mejorado(alfabeto, 6, o)
      println(s" mejorado Cadena encontrada: $cadenaM")
      val fin2 = System.nanoTime()
      val tiempo2 = (fin2 - inicio2)/ 1e6
      println(s"Tiempo de ejecucion: $tiempo2 ms")

      val inicio3 = System.nanoTime()
      val cadenaT = prc_turbo(alfabeto, 6, o)
      println(s" turbo Cadena encontrada: $cadenaT")
      val fin3 = System.nanoTime()
      val tiempo3 = (fin3 - inicio3)/ 1e6
      println(s"Tiempo de ejecucion: $tiempo3 ms")

      val inicio4 = System.nanoTime()
      //val cadenaTM = prc_turboMejorado(alfabeto, 6, o)
      //println(s" turbo Mejorado Cadena encontrada: $cadenaTM")
      val fin4 = System.nanoTime()
      val tiempo4 = (fin4 - inicio4) / 1e6
      println(s"Tiempo de ejecucion: $tiempo4 ms")
  }
}
