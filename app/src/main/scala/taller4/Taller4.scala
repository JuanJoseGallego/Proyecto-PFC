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

  def comparar_algoritmos (funcion: (Seq[Char], Int, Oraculo)=> Seq[Char])
                          (alfabeto: Seq[Char], tamano: Int, o: Oraculo):Double ={
    val tiempo: Double = (withWarmer(new Warmer.Default) measure { funcion(alfabeto, tamano, o) }).value
    tiempo
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
        val SCkF = SCk.filter(o)
        subcaden_candidatas(k*2 , SCkF)
      }
    }
    val Alfab = alfabeto.map(Seq(_))
    val SC = subcaden_candidatas(1, Alfab)
    SC.head

  }

  def prc_turboMejorado(alfabeto: Seq[Char], tamano: Int, o:Oraculo): Seq[Char] = {

    def filtrar(cadenaActual: Seq[Seq[Char]], cadenaAnterior: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (cadenaActual.head.length > 2) {
        cadenaActual.filter { s1 =>
          s1.sliding(s1.length / 2, 1).forall(cadenaAnterior.contains)
        }
      } else cadenaActual
    }
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val SCk = SC.flatMap { s1 =>
          SC.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }
        val SCactual = filtrar(SCk, SC)
        val SCkFiltrado = SCactual.filter(o)
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }
    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }


    def main(args: Array[String]): Unit = {

      val secuencia = Seq('a', 'c', 'a', 'g')
      val tamano = 64
      val secuenciaRandom = secuenciaaleatoria(tamano)

      val o: Oraculo = (s: Seq[Char]) => {
        secuenciaRandom.containsSlice(s)
      }
      //val tiempo1 = comparar_algoritmos(prc_ingenuo)(alfabeto, tamano, o)

      val tiempo2 = comparar_algoritmos(prc_mejorado)(alfabeto, tamano, o)

      val tiempo3 = comparar_algoritmos(prc_turbo)(alfabeto, tamano, o)

      val tiempo4 = comparar_algoritmos(prc_turboMejorado)(alfabeto, tamano, o)


      //resultados
      //val cadena = prc_ingenuo(alfabeto, tamano, o)
      //println(s" ingenuo Cadena encontrada:         $cadena")

      val cadenaM = prc_mejorado(alfabeto, tamano, o)
      println(s" mejorado Cadena encontrada:         $cadenaM")

      val cadenaT = prc_turbo(alfabeto, tamano, o)
      println(s" turbo Cadena encontrada:            $cadenaT")

      val cadenaTM = prc_turboMejorado(alfabeto, tamano, o)
      println(s" turbo Mejorado Cadena encontrada:   $cadenaTM")



      //println(s"Tiempo de ejecucion: $tiempo1 ms")
      println(s"Tiempo de ejecucion: $tiempo2 ms")
      println(s"Tiempo de ejecucion: $tiempo3 ms")
      println(s"Tiempo de ejecucion: $tiempo4 ms")
      //println(tiempo4-tiempo3)
  }
}

