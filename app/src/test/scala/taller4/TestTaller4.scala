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

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    val alfabeto = Seq('a', 'c', 'g', 't')
    type Oraculo = Seq[Char] => Boolean

    def secuenciaaleatoria(tamano: Int): Seq[Char] = {
        val random = new Random()
        (1 to tamano).map(_ => alfabeto(random.nextInt(alfabeto.length))) //En cada posición se pega una letra de forma aleatoria
    }

    val secuencia4 = secuenciaaleatoria(4)
    val o4: Oraculo = (s:Seq[Char])=>{
        secuencia4.containsSlice(s)
    }
    val secuencia8 = secuenciaaleatoria(8)
    val o8: Oraculo = (s:Seq[Char])=>{
        secuencia8.containsSlice(s)
    }
    val secuencia16 = secuenciaaleatoria(16)
    val o16: Oraculo = (s:Seq[Char])=>{
        secuencia16.containsSlice(s)
    }
    val secuencia32 = secuenciaaleatoria(32)
    val o32: Oraculo = (s:Seq[Char])=>{
        secuencia32.containsSlice(s)
    }
    test("prc_ingenuo4"){
        assert(secuencia4 == Taller4.prc_ingenuo(alfabeto, 4, o4))
    }
    test("prc_ingenuo8"){
        assert(secuencia8 == Taller4.prc_ingenuo(alfabeto, 8, o8))
    }
    test("prc_ingenuoPar4") {
        assert(secuencia4 == Taller4.prc_ingenuoPar(alfabeto, 4, o4))
    }
    test("prc_ingenuoPar8") {
        assert(secuencia8 == Taller4.prc_ingenuoPar(alfabeto, 8, o8))
    }



    test("prc_mejorado4"){
        assert(secuencia4 == Taller4.prc_mejorado(alfabeto, 4, o4))
    }
    test("prc_mejorado8"){
        assert(secuencia8 == Taller4.prc_mejorado(alfabeto, 8, o8))
    }
    test("prc_mejorado16"){
        assert(secuencia16 == Taller4.prc_mejorado(alfabeto, 16, o16))
    }
    test("prc_mejorado32"){
        assert(secuencia32 == Taller4.prc_mejorado(alfabeto, 32, o32))
    }
    test("prc_mejoradoPar4") {
        assert(secuencia4 == Taller4.prc_mejoradoPar(alfabeto, 4, o4))
    }
    test("prc_mejoradoPar8") {
        assert(secuencia8 == Taller4.prc_mejoradoPar(alfabeto, 8, o8))
    }
    test("prc_mejoradoPar16") {
        assert(secuencia16 == Taller4.prc_mejoradoPar(alfabeto, 16, o16))
    }
    test("prc_mejoradoPar32") {
        assert(secuencia32 == Taller4.prc_mejoradoPar(alfabeto, 32, o32))
    }



    test("prc_turbo4"){
        assert(secuencia4 == Taller4.prc_turbo(alfabeto, 4, o4))
    }
    test("prc_turbo8"){
        assert(secuencia8 == Taller4.prc_turbo(alfabeto, 8, o8))
    }
    test("prc_turbo16"){
        assert(secuencia16 == Taller4.prc_turbo(alfabeto, 16, o16))
    }
    test("prc_turbo32"){
        assert(secuencia32 == Taller4.prc_turbo(alfabeto, 32, o32))
    }
    test("prc_turboPar4") {
        assert(secuencia4 == Taller4.prc_turboPar(alfabeto, 4, o4))
    }
    test("prc_turboPar8") {
        assert(secuencia8 == Taller4.prc_turboPar(alfabeto, 8, o8))
    }
    test("prc_turboPar16") {
        assert(secuencia16 == Taller4.prc_turboPar(alfabeto, 16, o16))
    }
    test("prc_turboPar32") {
        assert(secuencia32 == Taller4.prc_turboPar(alfabeto, 32, o32))
    }



    test("prc_turbomejorado4"){
        assert(secuencia4 == Taller4.prc_turboMejorado(alfabeto, 4, o4))
    }
    test("prc_turboMejorado8"){
        assert(secuencia8 == Taller4.prc_turboMejorado(alfabeto, 8, o8))
    }
    test("prc_turboMejorado16"){
        assert(secuencia16 == Taller4.prc_turboMejorado(alfabeto, 16, o16))
    }
    test("prc_turboMejorado32"){
        assert(secuencia32 == Taller4.prc_turboMejorado(alfabeto, 32, o32))
    }
    test("prc_turboMejoradoPar4") {
        assert(secuencia4 == Taller4.prc_turboMejoradoPar(alfabeto, 4, o4))
    }
    test("prc_turboMejoradoPar8") {
        assert(secuencia8 == Taller4.prc_turboMejoradoPar(alfabeto, 8, o8))
    }
    test("prc_turboMejoradoPar16") {
        assert(secuencia16 == Taller4.prc_turboMejoradoPar(alfabeto, 16, o16))
    }
    test("prc_turboMejoradoPar32") {
        assert(secuencia32 == Taller4.prc_turboMejoradoPar(alfabeto, 32, o32))
    }



    test("prc_turboAcelerada4"){
        assert(secuencia4 == Taller4.prc_turboacelerada(alfabeto, 4, o4))
    }
    test("prc_turboAcelerada8"){
        assert(secuencia8 == Taller4.prc_turboacelerada(alfabeto, 8, o8))
    }
    test("prc_turboAcelerada16"){
        assert(secuencia16 == Taller4.prc_turboacelerada(alfabeto, 16, o16))
    }
    test("prc_turboAcelerada32"){
        assert(secuencia32 == Taller4.prc_turboacelerada(alfabeto, 32, o32))
    }
    test("prc_turboAceleradaPar4") {
        assert(secuencia4 == Taller4.prc_turboaceleradaPar(alfabeto, 4, o4))
    }
    test("prc_turboAceleradaPar8") {
        assert(secuencia8 == Taller4.prc_turboaceleradaPar(alfabeto, 8, o8))
    }
    test("prc_turboAceleradaPar16") {
        assert(secuencia16 == Taller4.prc_turboaceleradaPar(alfabeto, 16, o16))
    }
    test("prc_turboAceleradaPar32") {
        assert(secuencia32 == Taller4.prc_turboaceleradaPar(alfabeto, 32, o32))
    }



}
