package sura.com.co.dron.dron


// se crea un trait para que se pueda implementar en cualquier clase
trait MovimientosDron {

  def moverArriba(posicionDron: Dron): Dron

  def moverAbajo(posicionDron: Dron): Dron

  def moverDerecha(posicionDron: Dron): Dron

  def moverIzquierda(posicionDron: Dron): Dron

  def moverAdelante(posicionDron: Dron): Dron

  def moverAtras(posicionDron: Dron): Dron

}

// se crea un objeto que implementa el trait MovimientosDron y se sobreescriben los metodos de la clase MovimientosDron
object MoverDron extends MovimientosDron {
  override def moverArriba(posicionDron: Dron): Dron = {

    // se crea una constante para el movimiento maximo hacia arriba
    val movimientoMaximoArriba = 100

    // se obtienen las posiciones actuales del drone
    val posicionInicialArriba = posicionDron.arriba
    val posicionInicialAbajo = posicionDron.abajo

    // se valida que la posicion inicial del dron sea menor al movimiento maximo hacia arriba
    if (posicionInicialArriba.<(movimientoMaximoArriba)) {
      println("Movimiento dron hacia arriba")
      val (nuevaPosicionArriba: Int, nuevaPosicionAbajo: Int) = realizarMovimientoSuma(posicionInicialArriba, posicionInicialAbajo)

      posicionDron.copy(arriba = nuevaPosicionArriba, abajo = nuevaPosicionAbajo)
    } else {
      println(s"Se omite movimiento hacia arriba porque ha superado el máximo de posiciones permitidas ${movimientoMaximoArriba}")
      posicionDron
    }
  }
 //Metodo para realizar el movimiento hacia abajo
  override def moverAbajo(posicionDron: Dron): Dron = {
    val movimientoMaximoAbajo = -50

    val posicionInicialArriba = posicionDron.arriba
    val posicionInicialAbajo = posicionDron.abajo

    // se valida que la posicion inicial del dron sea mayor al movimiento maximo hacia abajo
    if (posicionInicialAbajo.>(movimientoMaximoAbajo)) {
      println("Movimiento dron hacia abajo")

      val (nuevaPosicionAbajo: Int, nuevaPosicionArriba: Int) = realizarMovimientoResta(posicionInicialAbajo, posicionInicialArriba)

      posicionDron.copy(arriba = nuevaPosicionArriba, abajo = nuevaPosicionAbajo)
    } else {
      println(s"Se omite movimiento hacia abajo abajo porque ha superado el máximo de posiciones permitidas ${movimientoMaximoAbajo}")
      posicionDron
    }
  }

  //Metodo para realizar el movimiento hacia derecha
  override def moverDerecha(posicionDron: Dron): Dron = {
    println("Movimiento dron hacia derecha")

    val posicionInicialDerecha = posicionDron.derecha
    val posicionInicialIzquierda = posicionDron.izquierda

    // se realiza el movimiento hacia derecha y se valida que la posicion inicial del dron sea menor al movimiento maximo hacia derecha
    val (nuevaPosicionDerecha: Int, nuevaPosicionIzquierda: Int) = realizarMovimientoSuma(posicionInicialDerecha, posicionInicialIzquierda)

    posicionDron.copy(derecha = nuevaPosicionDerecha, izquierda = nuevaPosicionIzquierda)
  }

  //Metodo para realizar el movimiento hacia izquierda
  override def moverIzquierda(posicionDron: Dron): Dron = {
    println("Movimiento dron hacia izquierda")

    val posicionInicialDerecha = posicionDron.derecha
    val posicionInicialIzquierda = posicionDron.izquierda

    // se realiza el movimiento hacia izquierda y se valida que la posicion inicial del dron sea mayor al movimiento maximo hacia izquierda
    val (nuevaPosicionIzquierda: Int, nuevaPosicionDerecha: Int) = realizarMovimientoResta(posicionInicialIzquierda, posicionInicialDerecha)

    posicionDron.copy(derecha = nuevaPosicionDerecha, izquierda = nuevaPosicionIzquierda)
  }

  //Metodo para realizar el movimiento hacia adelante
  override def moverAdelante(posicionDron: Dron): Dron = {
    println("Movimiento dron hacia adelante")

    // se obtienen las posiciones actuales del drone
    val posicionInicialAdelante = posicionDron.adelante
    val posicionInicialAtras = posicionDron.atras

    // se realiza el movimiento hacia adelante y se valida que la posicion inicial del dron sea menor al movimiento maximo hacia adelante
    val (nuevaPosicionAdelante: Int, nuevaPosicionAtras: Int) = realizarMovimientoSuma(posicionInicialAdelante, posicionInicialAtras)

    posicionDron.copy(adelante = nuevaPosicionAdelante, atras = nuevaPosicionAtras)
  }

  //Metodo para realizar el movimiento hacia atras
  override def moverAtras(posicionDron: Dron): Dron = {
    println("Movimiento dron hacia atras")

    val posicionInicialAdelante = posicionDron.adelante
    val posicionInicialAtras = posicionDron.atras

    // se realiza el movimiento hacia atras y se valida que la posicion inicial del dron sea mayor al movimiento maximo hacia atras
    val (nuevaPosicionAtras: Int, nuevaPosicionAdelante: Int) = realizarMovimientoResta(posicionInicialAtras, posicionInicialAdelante)

    posicionDron.copy(adelante = nuevaPosicionAdelante, atras = nuevaPosicionAtras)
  }

  //Metodo para sumar el movimiento del dron en una posicion especifica
  private def realizarMovimientoSuma(movimientoPrincipalRealizar: Int, movimientoContrarioValidar: Int) = {


    // se valida que el movimiento contrario sea menor a cero para realizar el movimiento contrario y si no se deja el movimiento contrario en cero
    val nuevoMovimientoContrario: Int = if (movimientoContrarioValidar.<(0)) sumarMovimiento(movimientoContrarioValidar) else movimientoContrarioValidar

    // se valida que el movimiento contrario sea igual a cero para realizar el movimiento principal y si no se deja el movimiento principal en cero
    val nuevoMovimientoPrincipal: Int = if (movimientoContrarioValidar == 0) sumarMovimiento(movimientoPrincipalRealizar) else movimientoPrincipalRealizar

    (nuevoMovimientoPrincipal, nuevoMovimientoContrario)
  }

  //Metodo para restar el movimiento del dron en una posicion especifica
  private def realizarMovimientoResta(movimientoPrincipalRealizar: Int, movimientoContrarioValidar: Int) = {

    // se valida que el movimiento contrario sea mayor a cero para realizar el movimiento contrario y si no se deja el movimiento contrario en cero
    val nuevoMovimientoContrario: Int = if (movimientoContrarioValidar.>(0)) restarMovimiento(movimientoContrarioValidar) else movimientoContrarioValidar

    // se valida que el movimiento contrario sea igual a cero para realizar el movimiento principal y si no se deja el movimiento principal en cero
    val nuevoMovimientoPrincipal: Int = if (movimientoContrarioValidar == 0) restarMovimiento(movimientoPrincipalRealizar) else movimientoPrincipalRealizar

    (nuevoMovimientoPrincipal, nuevoMovimientoContrario)
  }

  //Metodos para sumar y restar el movimiento del dron en una posicion especifica que es 5 para cada movimiento
  private def sumarMovimiento(posicion: Int) = posicion + 5

  private def restarMovimiento(posicion: Int) = posicion - 5
}
