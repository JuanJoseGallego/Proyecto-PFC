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
import common._


object Taller4 {

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean


  //Esta funcion genera una secuencia aleatoria de tamaño n (prueba de 3 millones de elementos)
  def secuenciaaleatoria(tamano: Int): String = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))).mkString //En cada posición se pega una letra de forma aleatoria
  }

  //se usan medidas de rendimiento para evaluar el tiempo que tarde en ejecutarse la función
  //esta función depende de los parámetros alfabeto, tamano y el oraculo
  def comparar_algoritmos(funcion: (Seq[Char], Int, Oraculo) => Seq[Char])
                         (alfabeto: Seq[Char], tamano: Int, o: Oraculo): Double = {
    val tiempo: Double = (withWarmer(new Warmer.Default) measure {
      funcion(alfabeto, tamano, o)
    }).value
    tiempo
  }

  //Prc = Problema en la reconstruccion de cadenas ingenuo
  def prc_ingenuo(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def cadenas_candidatas(alfabeto: Seq[Char], tamano: Int): Seq[Seq[Char]] = {
      //si la variable de control K es igual al tamaño n, se detiene y devuelve la SUBCADENA
      if (tamano == 0) Seq(Seq.empty[Char])
      else { //longitudes mayores a 0 (caso recursivo)
        alfabeto.flatMap(caracter => cadenas_candidatas(alfabeto, tamano - 1).map(caracter +: _))
      }
    }

    //variable que contiene una secuencia de secuencias de posibles candidatas de tamaño n
    val combinaciones = cadenas_candidatas(alfabeto, tamano)
    //Itera entre los elementos de la secuencia y si cumple con la condicion del oraculo, retorna la secuencia.
    combinaciones.flatMap { seq => if (o(seq)) seq else None }

  }

  def prc_ingenuoPar(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def cadenas_candidatas(alf: Seq[Char], tamano: Int): Seq[Seq[Char]] = {
      if (tamano == 0) Seq(Seq.empty[Char]) //caso base
      else { //longitudes mayores a 0 (caso recursivo)
        alf.flatMap(caracter => cadenas_candidatas(alfabeto, tamano - 1).map(caracter +: _))
      }
    }

    val (a, b) = alfabeto.splitAt(alfabeto.length / 2)
    val t1 = task {
      cadenas_candidatas(a, tamano)
    }
    val t2 = task {
      cadenas_candidatas(b, tamano)
    }
    val combinaciones = t1.join ++ t2.join
    //Itera entre los elementos de la secuencia y si cumple con la condicion del oraculo, retorna la secuencia.
    val (comb1, comb2) = combinaciones.splitAt(combinaciones.length / 2)
    val t3 = task {
      comb1.flatMap { seq => if (o(seq)) seq else None }
    }
    val t4 = task {
      comb2.flatMap { seq => if (o(seq)) seq else None }
    }
    t3.join ++ t4.join

  }

  def prc_mejorado(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SCanterior: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k == tamano) SCanterior
      else {
        val sck = SCanterior.flatMap(elementos_sc => alfabeto.map(letra => elementos_sc :+ letra)).filter(o)
        //Una vez hecho el llamado recursivo se evalua el siguiente elemento de la secuencia
        subcaden_candidatas(k + 1, sck)
      }
    }

    val SC = subcaden_candidatas(0, Seq(Seq()))
    SC.head
  }

  def prc_mejoradoPar(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k == tamano) SC
      else {
        val (a, b) = SC.splitAt(SC.length / 2)
        val t1 = task {
          a.flatMap(elementos_sc => alfabeto.map(letra => elementos_sc :+ letra)).filter(o)
        }
        val t2 = task {
          b.flatMap(elementos_sc => alfabeto.map(letra => elementos_sc :+ letra)).filter(o)
        }
        val sck = t1.join ++ t2.join
        subcaden_candidatas(k + 1, sck)
      }
    }

    val SC = subcaden_candidatas(0, Seq(Seq()))
    SC.head
  }

  def prc_turbo(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val SCk = SC.flatMap { s1 =>
          SC.flatMap { s2 =>
            Seq(s1 ++ s2)
          }
        }
        val SCkF = SCk.filter(o)
        subcaden_candidatas(k * 2, SCkF)
      }
    }

    val Alfab = alfabeto.map(Seq(_))
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }

  def prc_turboPar(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val (a, b) = SC.splitAt(SC.length / 2)
        val t1 = task {
          a.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
        }
        val t2 = task {
          b.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
        }
        val SCk = t1.join ++ t2.join
        val SCkF = SCk.filter(o)
        subcaden_candidatas(k * 2, SCkF)
      }
    }

    val Alfab = alfabeto.map(Seq(_))
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }


  def prc_turboMejorado(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {

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

  def prc_turboMejoradoPar(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {

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
        val (a, b) = SC.splitAt(SC.length / 2)
        val t1 = task {
          a.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
        }
        val t2 = task {
          b.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
        }
        val SCk = t1.join ++ t2.join
        val SCactual = filtrar(SCk, SC)
        val SCkFiltrado = SCactual.filter(o)
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }

    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }


  def adicionar(s: Seq[Char], t: Trie): Trie = {
    def adicionarRecursivo(subsec: Seq[Char], subtrie: Trie): Trie = {
      subsec match {
        case Seq() => subtrie // La subsecuencia es vacía, no hay nada que adicionar
        case head +: tail =>
          subtrie match {
            case Nodo(c, marcada, hijos) =>
              val subtrieHijoOpt = hijos.find(t => raiz(t) == head)
              val nuevoHijo = subtrieHijoOpt match {
                case Some(hijo) => adicionarRecursivo(tail, hijo)
                case None => adicionarRecursivo(tail, Hoja(head, marcada = false))
              }
              Nodo(c, marcada, hijos.filter(t => raiz(t) != head) :+ nuevoHijo)
            case Hoja(_, _) => subtrie // Llegamos a una hoja y aún quedan caracteres en la subsecuencia, no se puede adicionar
          }
      }
    }

    adicionarRecursivo(s, t)
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    def perteneceRecursivo(subsec: Seq[Char], subtrie: Trie): Boolean = {
      subsec match {
        case Seq() => true // La subsecuencia es vacía, por lo tanto, pertenece
        case head +: tail =>
          subtrie match {
            case Nodo(_, _, hijos) =>
              val subtrieHijoOpt = hijos.find(t => raiz(t) == head)
              subtrieHijoOpt.exists(hijo => perteneceRecursivo(tail, hijo))
            case Hoja(_, _) => false // Llegamos a una hoja y aún quedan caracteres en la subsecuencia, no pertenece
          }
      }
    }

    perteneceRecursivo(s, t)
  }

  def arbolSufijos(ss: Seq[Seq[Char]], t: Trie): Trie = {
    def funcion_aux(ss: Seq[Seq[Char]], t: Trie): Trie = {
      if (ss.isEmpty) t
      else funcion_aux(ss.tail, adicionar(ss.head, t))
    }

    funcion_aux(ss, Nodo(' ', false, List()))
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lT) => lT.map(t => raiz(t))
      case Hoja(c, _) => Seq(c)
    }
  }


  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }
/*
  def prc_turboacelerada(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {
    def reconstruirCadenaTurboMejoradaRecursivo(tamano: Int, o: Oraculo, t: Trie): Seq[Char] = {
      if (tamano == 0) Seq()
      else {
        val cabezasTrie = cabezas(t)
        val cabezasTrieFiltradas = cabezasTrie.filter(c => o(cabezasTrie))
        val cabeza = cabezasTrieFiltradas.head
        val subtrie = t match {
          case Nodo(_, _, hijos) => hijos.find(t => raiz(t) == cabeza).get
          case Hoja(_, _) => t
        }
        cabeza +: reconstruirCadenaTurboMejoradaRecursivo(tamano - 1, o, subtrie)
      }
    }

    reconstruirCadenaTurboMejoradaRecursivo(tamano, o, arbolSufijos(alfabeto.map(c => Seq(c)), Nodo(' ', false, List())))
  }
*/

  def main(args: Array[String]): Unit = {

    val secuencia = Seq('a', 'c', 'a', 'g')
    val tamano = 4
    val secuenciaRandom = secuenciaaleatoria(tamano)

    val o: Oraculo = (s: Seq[Char]) => {
      secuenciaRandom.containsSlice(s)
    }
    //val tiempo1 = comparar_algoritmos(prc_ingenuo)(alfabeto, tamano, o)
    //val tiempo1p = comparar_algoritmos(prc_ingenuoPar)(alfabeto, tamano, o)

    val tiempo2 = comparar_algoritmos(prc_mejorado)(alfabeto, tamano, o)
    val tiempo2p = comparar_algoritmos(prc_mejoradoPar)(alfabeto, tamano, o)

    val tiempo3 = comparar_algoritmos(prc_turbo)(alfabeto, tamano, o)
    val tiempo3p = comparar_algoritmos(prc_turboPar)(alfabeto, tamano, o)

    val tiempo4 = comparar_algoritmos(prc_turboMejorado)(alfabeto, tamano, o)
    val tiempo4p = comparar_algoritmos(prc_turboMejoradoPar)(alfabeto, tamano, o)

    //val tiempo5 = comparar_algoritmos(prc_turboacelerada)(alfabeto, tamano, o)

    //resultados
    //val cadena = prc_ingenuo(alfabeto, tamano, o)
    //println(s" ingenuo Cadena encontrada:         $cadena")
    //val cadena_Par = prc_ingenuoPar(alfabeto, tamano, o)
    //println(s" ingenuoPar Cadena encontrada:      $cadena_Par")

    val cadenaM = prc_mejorado(alfabeto, tamano, o)
    println(s" mejorado Cadena encontrada:         $cadenaM")
    val cadenaM_Par = prc_mejoradoPar(alfabeto, tamano, o)
    println(s" mejoradoPar Cadena encontrada:      $cadenaM_Par")

    val cadenaT = prc_turbo(alfabeto, tamano, o)
    println(s" turbo Cadena encontrada:            $cadenaT")
    val cadenaT_Par = prc_turboPar(alfabeto, tamano, o)
    println(s" turboPar Cadena encontrada:         $cadenaT_Par")

    val cadenaTM = prc_turboMejorado(alfabeto, tamano, o)
    println(s" turbo Mejorado Cadena encontrada:   $cadenaTM")
    val cadenaTM_Par = prc_turboMejoradoPar(alfabeto, tamano, o)
    println(s" turbo Mejorado Par Cadena encontrada:   $cadenaTM_Par")

    //val cadenaTa = prc_turboacelerada(alfabeto, tamano, o)
    //println(s" turbo Mejorado Par Cadena encontrada:   $cadenaTa")


    //println(s"Tiempo de ejecucion prc_ingenuo:        $tiempo1 ms")
    //println(s"Tiempo de ejecucion prc_ingenuoPar:     $tiempo1p ms")

    println(s"Tiempo de ejecucion prc_mejorado:       $tiempo2 ms")
    println(s"Tiempo de ejecucion prc_mejoradoPar:    $tiempo2p ms")

    println(s"Tiempo de ejecucion prc_turbo:          $tiempo3 ms")
    println(s"Tiempo de ejecucion prc_turboPar:       $tiempo3p ms")

    println(s"Tiempo de ejecucion prc_turbomejorado:  $tiempo4 ms")
    println(s"Tiempo de ejecucion prc_turbomejoradoPar:  $tiempo4p ms")


    //println(s"Secuencia aleatoria:  $secuenciaRandom")
    //println(s"Tiempo de ejecucion prc_turboacelerada:  $tiempo5 ms")


  }
}

