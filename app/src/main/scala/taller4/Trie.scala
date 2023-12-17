package taller4

abstract class Trie

case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie

case class Hoja(car: Char, marcada: Boolean) extends Trie



