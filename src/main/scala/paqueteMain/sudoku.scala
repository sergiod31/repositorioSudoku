package paqueteMain

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession


object sudoku {

  def main(args: Array[String]): Unit = {
    Logger.getLogger("org").setLevel(Level.ERROR)
    val spark: SparkSession = SparkSession.builder()
      .master("local")
      .appName("prueba")
      .getOrCreate();

    var tablero = new Tablero

    tablero.inicializarTablero()
    tablero.imprimirTablero(tablero.casillas)

  }

  class Tablero {
    //
    //        0    1    2         ┌───>   i
    //      ┌────┬────┬────┐      │
    //   0  │ 0  │ 1  │ 2  │      V
    //      ├────┼────┼────┤
    //   1  │ 3  │ 4  │ 5  │      j
    //      ├────┼────┼────┤
    //   2  │ 6  │ 7  │ 8  │
    //      └────┴────┴────┘
    //
    //  casillas solucion
    val casillas: Array[Array[Int]] = Array[Array[Int]](9)(9)
    //
    //                 primero fila, luego columna
    val sectores: Array[Array[Array[Int]]] = Array[Array[Array[Int]]](9)(3)(3)
    val filas: Array[Array[Int]] = Array[Array[Int]](9)(9)
    val columnas: Array[Array[Int]] = Array[Array[Int]](9)(9)
    //
    //  casillas para el jugador
    val casillasJugador: Array[Array[Int]] = Array[Array[Int]](9)(9)
    //
    val sectoresJugador: Array[Array[Array[Int]]] = Array[Array[Array[Int]]](9)(3)(3)
    val filasJugador: Array[Array[Int]] = Array[Array[Int]](9)(9)
    val columnasJugador: Array[Array[Int]] = Array[Array[Int]](9)(9)
    //
    inicializarVariables()


    def inicializarVariables(): Unit = {
      var i: Int = 0
      var j: Int = 0
      //
      // copio las referencias a las filas
      filas.foreach(_ => {
        filas(i) = casillas(i)
        i += 1
      })

      // copio las referencias a las columnas
      i = 0
      j = 0
      for (j <- columnas.indices) {
        val columna: Array[Int] = Array[Int](9)
        for (i <- casillas.indices) {
          columna(i) = casillas(j)(i)
        }
        columnas(j) = columna
      }

      // copio las referencias a los sectores
      i = 0
      j = 0
      for (k <- sectores.indices) { // recorro los sectores
        val auxSector: Array[Array[Int]] = Array[Array[Int]](3)(3)
        for (j <- sectores(k).indices) { // recorro las filas
          val auxFila: Array[Int] = Array[Int](3)
          for (i <- sectores(k)(j).indices) { // recorro las casillas
            auxFila(i) = casillas(i)(j) // copio la casilla
          }
          auxSector(j) = auxFila // copio la fila
        }
        sectores(k) = auxSector // copio el sector
      }

      filas.foreach(_ => {
        filasJugador(i) = casillasJugador(i)
        i += 1
      })

      // copio las referencias a las columnas
      i = 0
      j = 0
      for (j <- columnasJugador.indices) {
        val columna: Array[Int] = Array[Int](9)
        for (i <- casillasJugador.indices) {
          columna(i) = casillasJugador(j)(i)
        }
        columnasJugador(j) = columna
      }

      // copio las referencias a los sectores
      i = 0
      j = 0
      for (k <- sectoresJugador.indices) { // recorro los sectores
        val auxSector: Array[Array[Int]] = Array[Array[Int]](3)(3)
        for (j <- sectoresJugador(k).indices) { // recorro las filas
          val auxFila: Array[Int] = Array[Int](3)
          for (i <- sectoresJugador(k)(j).indices) { // recorro las casillas
            auxFila(i) = casillasJugador(i)(j) // copio la casilla
          }
          auxSector(j) = auxFila // copio la fila
        }
        sectoresJugador(k) = auxSector // copio el sector
      }
    }

    //
    def comprobarVictoria(): Boolean = {
      for (i <- 0 to 8; j <- 0 to 8) {
        if (casillasJugador(i)(j) != casillas(i)(j)) {
          return false
        }
      }
      true
    }

    // comprueba que el numero no este ya presente en la fila
    def checkNumInFila(num: Int, fila: Int): Boolean = {
      filasJugador(fila).foreach(e => {
        if (e.equals(num)) {
          return false
        }
      })
      true
    }

    // comprueba que el numero no este ya presente en la columna
    def checkNumInColumna(num: Int, columna: Int): Boolean = {
      columnasJugador(columna).foreach(e => {
        if (e.equals(num)) {
          return false
        }
      })
      true
    }

    // comprueba que el numero no este ya presente en el sector
    def checkNumInSector(num: Int, sector: Int): Boolean = {
      sectoresJugador(sector).foreach(fila => {
        fila.foreach(casilla => {
          if (casilla.equals(num)) {
            return false
          }
        })
      })
      true
    }

    // obtiene, de una fila + columna, a que sector pertenece
    def getSector(fila: Int, columna: Int): Int = {
      (fila / 3) + ((columna / 3) * 3)
    }

    /*
     * genero los 81 numeros para el tablero
     */
    def inicializarTablero(): Unit = {
      val rand = scala.util.Random
      val tableroAux: Array[Array[Byte]] = Array[Array[Byte]](9)(9)
      for (i <- tableroAux.indices) {
        for (j <- tableroAux(i).indices) {
          tableroAux(i)(j) = 511.toByte
        }
      }

      def actualizarFila(fila: Int, num: Int): Unit = {
        for (j <- tableroAux(fila).indices) { // recorro la fila pedida
          // elimino 'num' del byte SOLO si esta
          if ((tableroAux(fila)(j) & num.toByte) == num.toByte) {
            tableroAux(fila)(j) -= scala.math.pow(2, num - 1).toByte
          }
        }
      }

      def actualizarColumna(columna: Int, num: Int): Unit = {
        for (i <- tableroAux.indices) { // recorro la columna pedida
          // elimino 'num' del byte SOLO si esta
          if ((tableroAux(i)(columna) & num.toByte) == num.toByte) {
            tableroAux(i)(columna) -= scala.math.pow(2, num - 1).toByte
          }
        }
      }

      def actualizarSector(sector: Int, num: Int): Unit = {
        for (i <- sector % 3 * 3 to sector % 3 * 3 + 2) { // fila del sector
          for (j <- sector / 3 * 3 to sector / 3 * 3 + 2) { // columna del sector
            if ((tableroAux(i)(j) & num.toByte) == num.toByte) {
              tableroAux(i)(j) -= scala.math.pow(2, num - 1).toByte
            }
          }
        }
      }

      def inicializarCasilla(i: Int, j: Int): Unit = {
        // elijo un numero random
        var valido = false

        // inicializo un byte con solo un bit a 1, aleatorio entre los bits 0  y 9 menos significativos,
        // significando ...000 000 001 que se quiere colocar un 1 y ...100 000 000 que se quiere colocar un 9
        var mascara = (scala.math.pow(2, rand.nextInt(9))).asInstanceOf[Int].toByte
        if (!valido) {
          if ((tableroAux(i)(j) & mascara) != 0) { // si la mascara es igual a 0, es que el numero no esta disponible
            valido = true
          }
          if ((mascara & 1) == 1) { // es impar
            mascara >>= 1
            mascara += 256.toByte // recoloco el bit que se iba a perder a la izq del tod0
          } else { // es par
            mascara >>= 1
          }
        }
        // tengo un numero valido para colocar
        var numero: Int = mascara.asInstanceOf[Int]
        casillas(i)(j) = numero

        // ahora toca quitar ese numero de las filas, columnas y sectores a los que afecta
        actualizarFila(i, numero)
        actualizarColumna(j, numero)
        val sector = (i / 3) + (j / 3 * 3)
        actualizarSector(sector, numero)
      }


      for (i <- casillas.indices) {
        for (j <- casillas(i).indices) {
          inicializarCasilla(i, j)
        }
      }

    }

    def imprimirTablero(tablero: Array[Array[Int]]): Unit = {
      println("┌────┬────┬────┬────┬────┬────┬────┬────┬────┐  ")
      println(s"│ ${tablero(0)(0)}  │ ${tablero(0)(1)}  │ ${tablero(0)(2)}  │ ${tablero(0)(3)}  │ ${tablero(0)(4)}  │ ${tablero(0)(5)}  │  ${tablero(0)(6)} │ ${tablero(0)(7)}  │ ${tablero(0)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(1)(0)}  │ ${tablero(1)(1)}  │ ${tablero(1)(2)}  │ ${tablero(1)(3)}  │ ${tablero(1)(4)}  │ ${tablero(1)(5)}  │  ${tablero(1)(6)} │ ${tablero(1)(7)}  │ ${tablero(1)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(2)(0)}  │ ${tablero(2)(1)}  │ ${tablero(2)(2)}  │ ${tablero(2)(3)}  │ ${tablero(2)(4)}  │ ${tablero(2)(5)}  │  ${tablero(2)(6)} │ ${tablero(2)(7)}  │ ${tablero(2)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(3)(0)}  │ ${tablero(3)(1)}  │ ${tablero(3)(2)}  │ ${tablero(3)(3)}  │ ${tablero(3)(4)}  │ ${tablero(3)(5)}  │  ${tablero(3)(6)} │ ${tablero(3)(7)}  │ ${tablero(3)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(4)(0)}  │ ${tablero(4)(1)}  │ ${tablero(4)(2)}  │ ${tablero(4)(3)}  │ ${tablero(4)(4)}  │ ${tablero(4)(5)}  │  ${tablero(4)(6)} │ ${tablero(4)(7)}  │ ${tablero(4)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(5)(0)}  │ ${tablero(5)(1)}  │ ${tablero(5)(2)}  │ ${tablero(5)(3)}  │ ${tablero(5)(4)}  │ ${tablero(5)(5)}  │  ${tablero(5)(6)} │ ${tablero(5)(7)}  │ ${tablero(5)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(6)(0)}  │ ${tablero(6)(1)}  │ ${tablero(6)(2)}  │ ${tablero(6)(3)}  │ ${tablero(6)(4)}  │ ${tablero(6)(5)}  │  ${tablero(6)(6)} │ ${tablero(6)(7)}  │ ${tablero(6)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(7)(0)}  │ ${tablero(7)(1)}  │ ${tablero(7)(2)}  │ ${tablero(7)(3)}  │ ${tablero(7)(4)}  │ ${tablero(7)(5)}  │  ${tablero(7)(6)} │ ${tablero(7)(7)}  │ ${tablero(7)(8)}  │")
      println("├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"│ ${tablero(8)(0)}  │ ${tablero(8)(1)}  │ ${tablero(8)(2)}  │ ${tablero(8)(3)}  │ ${tablero(8)(4)}  │ ${tablero(8)(5)}  │  ${tablero(8)(6)} │ ${tablero(8)(7)}  │ ${tablero(8)(8)}  │")
      println("└────┴────┴────┴────┴────┴────┴────┴────┴────┘")
    }
  }


}
