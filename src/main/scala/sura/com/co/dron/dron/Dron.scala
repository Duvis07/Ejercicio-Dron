package sura.com.co.dron.dron

import sura.com.co.dron.dron.MovimientosValidos.Movimiento

case class Dron(arriba: Int, abajo: Int, izquierda: Int, derecha: Int, atras: Int, adelante: Int) {

}

object Dron {
  def apply(): Dron = Dron(0, 0, 0, 0, 0, 0)

  def realizarMovimientos(dronPosicionInicial: Dron, movimientos: List[Movimiento]): Dron = {

    movimientos.headOption
      .fold(dronPosicionInicial) {
        case MovimientosValidos.ARRIBA =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverArriba(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case MovimientosValidos.ABAJO =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverAbajo(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case MovimientosValidos.IZQUIERDA =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverIzquierda(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case MovimientosValidos.DERECHA =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverDerecha(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case MovimientosValidos.ADELANTE =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverAdelante(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case MovimientosValidos.ATRAS =>
          val movimientosFaltantes: List[Movimiento] = movimientos.drop(1)
          val nuevoDron = MoverDron.moverAtras(dronPosicionInicial)
          realizarMovimientos(nuevoDron, movimientosFaltantes)
        case _ => dronPosicionInicial
      }
  }
}
