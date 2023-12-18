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
  def secuenciaaleatoria(tamano: Int): Seq[Char] = {
    val random = new Random()
    (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length)))//En cada posición se pega una letra de forma aleatoria
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
          val cadenas = a.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          cadenas.filter(o)
        }
        val t2 = task {
          val cadenas = b.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          cadenas.filter(o)
        }
        val SCkF = t1.join ++ t2.join
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
          val cadenas = a.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          filtrar(cadenas, SC)
        }
        val t2 = task {
          val cadenas = b.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          filtrar(cadenas, SC)
        }
        val SCactual = t1.join ++ t2.join
        val SCkFiltrado = SCactual.filter(o)
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }

    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }
  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lT) => lT.map(t => raiz(t))
      case Hoja(c, _) => Seq(c)
    }
  }
  def pertenece(s: Seq[Char], t: Trie): Boolean = (s, t) match {
    case (caracter :: cola, Nodo(_, _, hijos)) =>
      pertenece(cola, hijos.find(raiz(_) == caracter).getOrElse(Hoja(' ', false)))
    case (Nil, Nodo(_, marcada, _)) => marcada
    case (Nil, Hoja(_, marcada)) => marcada
    case _ => false
  }


  def adicionar(s: Seq[Char], t: Trie): Trie = {
    def agregarRama(arbolActual: Trie, remaining: Seq[Char]): Trie = {
      (arbolActual, remaining) match {
        case (Nodo(car, marcada, hijos), head :: tail) if cabezas(Nodo(car, marcada, hijos)).contains(head) =>
          // Recorre recursivamente el árbol hasta llegar al camino deseado
          val hijosActualizados = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, tail)
            else hijo
          }
          Nodo(car, marcada, hijosActualizados)
        case (Hoja(car, marcada), head :: tail) =>
          // Convierte la hoja en un Nodo con el nuevo "subárbol" como hijo
          Nodo(car, marcada, List(adicionar(tail, Hoja(head, true))))
        case (Nodo(car, marcada, hijos), head :: tail) =>
          // Agrega el nuevo nodo a la lista de hijos cuando el camino se detiene en un Nodo
          Nodo(car, marcada, hijos :+ adicionar(tail, Nodo(head, false, List())))
        case (Nodo(car, false, hijos), Nil) =>
          // Modifica el valor de marcada a true si no hay camino por recorrer pero los elementos de la cadena están en el arbol.
          Nodo(car, marcada = true, hijos)
        case (_, _) =>
          arbolActual
      }
    }

    agregarRama(t, s)
  }


  def arbolSufijos(secuenciaDeCadenas: Seq[Seq[Char]]): Trie = {
    def funcion_aux(ss: Seq[Seq[Char]], t: Trie): Trie = {
      if (ss.isEmpty) t
      else funcion_aux(ss.tail, adicionar(ss.head, t))
    }
    funcion_aux(secuenciaDeCadenas, Nodo(' ', false, List()))
  }
  def prc_turboacelerada(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {

    def filtrar(cadenaActual: Seq[Seq[Char]], cadenaAnterior: Seq[Seq[Char]] , k:Int ): Seq[Seq[Char]] = {
      if (cadenaActual.head.length > 2) {
        val t = arbolSufijos(cadenaAnterior)
        cadenaActual.filter{s1 => (0 to s1.length/2).forall{ i => pertenece(s1.slice(i,i+k),t) }}
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

        val SCactual = filtrar(SCk, SC, k)
        val SCkFiltrado = SCactual.filter(o)
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }
    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }

  def prc_turboaceleradaPar(alfabeto: Seq[Char], tamano: Int, o: Oraculo): Seq[Char] = {

    def filtrar(cadenaActual: Seq[Seq[Char]], cadenaAnterior: Seq[Seq[Char]], k: Int): Seq[Seq[Char]] = {
      if (cadenaActual.head.length > 2) {
        val t = arbolSufijos(cadenaAnterior)
        cadenaActual.filter { s1 => (0 to s1.length/2).forall { i => pertenece(s1.slice(i, i + k), t) } }
      } else cadenaActual
    }

    def subcaden_candidatas(k: Int, SC: Seq[Seq[Char]]): Seq[Seq[Char]] = {
      if (k >= tamano) SC
      else {
        val (a, b) = SC.splitAt(SC.length / 2)
        val t1 = task {
          val cadenas = a.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          filtrar(cadenas, SC,k).filter(o)
        }
        val t2 = task {
          val cadenas = b.flatMap { s1 => SC.flatMap { s2 => Seq(s1 ++ s2) } }
          filtrar(cadenas, SC,k).filter(o)
        }
        val SCactual = t1.join ++ t2.join
        val SCkFiltrado = SCactual
        subcaden_candidatas(k * 2, SCkFiltrado)
      }
    }

    val Alfab = alfabeto.map(Seq(_)).filter(o)
    val SC = subcaden_candidatas(1, Alfab)
    SC.head
  }
  def main(args: Array[String]): Unit = {

    val secuencia = Seq('a', 'c', 'a', 'g')
    val tamano = 32
    val secuenciaRandom = secuenciaaleatoria(tamano)

    val o: Oraculo = (s: Seq[Char]) => {
      secuenciaRandom.containsSlice(s)
    }
    //val tiempo1 = comparar_algoritmos(prc_ingenuo)(alfabeto, tamano, o)
    //val tiempo1p = comparar_algoritmos(prc_ingenuoPar)(alfabeto, tamano, o)

    //val tiempo2 = comparar_algoritmos(prc_mejorado)(alfabeto, tamano, o)
    //val tiempo2p = comparar_algoritmos(prc_mejoradoPar)(alfabeto, tamano, o)

    //val tiempo3 = comparar_algoritmos(prc_turbo)(alfabeto, tamano, o)
    //val tiempo3p = comparar_algoritmos(prc_turboPar)(alfabeto, tamano, o)

    //val tiempo4 = comparar_algoritmos(prc_turboMejorado)(alfabeto, tamano, o)
    //val tiempo4p = comparar_algoritmos(prc_turboMejoradoPar)(alfabeto, tamano, o)

    val tiempo5 = comparar_algoritmos(prc_turboacelerada)(alfabeto, tamano, o)
    val tiempo5p = comparar_algoritmos(prc_turboaceleradaPar)(alfabeto, tamano, o)

    //resultados
    //val cadena = prc_ingenuo(alfabeto, tamano, o)
    //println(s" ingenuo Cadena encontrada:         $cadena")
    //val cadena_Par = prc_ingenuoPar(alfabeto, tamano, o)
    //println(s" ingenuoPar Cadena encontrada:      $cadena_Par")

    //val cadenaM = prc_mejorado(alfabeto, tamano, o)
    //println(s" mejorado Cadena encontrada:         $cadenaM")
    //val cadenaM_Par = prc_mejoradoPar(alfabeto, tamano, o)
    //println(s" mejoradoPar Cadena encontrada:      $cadenaM_Par")

    //val cadenaT = prc_turbo(alfabeto, tamano, o)
    //println(s" turbo Cadena encontrada:            $cadenaT")
    //val cadenaT_Par = prc_turboPar(alfabeto, tamano, o)
    //println(s" turboPar Cadena encontrada:         $cadenaT_Par")

    //val cadenaTM = prc_turboMejorado(alfabeto, tamano, o)
    //println(s" turbo Mejorado Cadena encontrada:   $cadenaTM")
    //val cadenaTM_Par = prc_turboMejoradoPar(alfabeto, tamano, o)
    //println(s" turbo Mejorado Par Cadena encontrada:   $cadenaTM_Par")

    val cadenaTa = prc_turboacelerada(alfabeto, tamano, o)
    println(s" turbo Mejorado Par Cadena encontrada:   $cadenaTa")
    val cadenaTa_Par = prc_turboaceleradaPar(alfabeto, tamano, o)
    println(s" turbo Mejorado Par Cadena encontrada:   $cadenaTa_Par")


    //println(s"Tiempo de ejecucion prc_ingenuo:        $tiempo1 ms")
    //println(s"Tiempo de ejecucion prc_ingenuoPar:     $tiempo1p ms")

    //println(s"Tiempo de ejecucion prc_mejorado:       $tiempo2 ms")
    //println(s"Tiempo de ejecucion prc_mejoradoPar:    $tiempo2p ms")

    //println(s"Tiempo de ejecucion prc_turbo:          $tiempo3 ms")
    //println(s"Tiempo de ejecucion prc_turboPar:       $tiempo3p ms")

    //println(s"Tiempo de ejecucion prc_turbomejorado:  $tiempo4 ms")
    //println(s"Tiempo de ejecucion prc_turbomejoradoPar:  $tiempo4p ms")

    println (s"Secuencia aleatoria: $secuenciaRandom")
    println(s"Tiempo de ejecucion prc_turboacelerada:  $tiempo5 ms")
    println(s"Tiempo de ejecucion prc_turboaceleradaPar:  $tiempo5p ms")



  }
}

