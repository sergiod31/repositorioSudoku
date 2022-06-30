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
    // tablero.imprimirTablero(tablero.casillas)

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
    val casillas: Array[Array[Int]] = Array.ofDim[Int](9, 9)
    //
    //  casillas para el jugador
    val casillasJugador: Array[Array[Int]] = Array.ofDim[Int](9, 9)
    //

    def deByteAInt(num: Int): Int = {
      //  000 100 000 -> 6
      if (num == 1) {
        return 1
      }
      if (num == 2) {
        return 2
      }
      if (num == 4) {
        return 3
      }
      if (num == 8) {
        return 4
      }
      if (num == 16) {
        return 5
      }
      if (num == 32) {
        return 6
      }
      if (num == 64) {
        return 7
      }
      if (num == 128) {
        return 8
      }
      if (num == 256) {
        return 9
      }
      if (num == 0) {
        return 0
      }
      -1
    }

    def deIntAByte(num: Int): Int = {
      //  6 -> 000 100 000
      scala.math.pow(2, num - 1).asInstanceOf[Int]
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

    // obtiene, de una fila + columna, a que sector pertenece
    def getSector(fila: Int, columna: Int): Int = {
      (fila / 3) + ((columna / 3) * 3)
    }

    /*
     * genero los 81 numeros para el tablero
     */
    def inicializarTablero(): Unit = {
      val rand = scala.util.Random

      // relleno tablero de '111 111 111'
      for (i <- casillas.indices) {
        for (j <- casillas(i).indices) {
          casillas(i)(j) = 511
        }
      }

      def actualizarFilaColumnaYSector(fila: Int, columna: Int, num: Int): Unit = {
        // actualizo la fila
        for (j <- casillas(fila).indices) { // recorro la fila pedida
          // me salto la casilla objetivo original
          if (j != columna && ((casillas(fila)(j) & num) > 0)) {
            casillas(fila)(j) -= num

            // si al actualizar, un numero secundario colapsa a 1, 2, 3...
            if (esNumeroFinal(casillas(fila)(j))) {
              actualizarFilaColumnaYSector(fila, j, casillas(fila)(j))
            }
          }
        }
        // actualizo la columna
        for (i <- casillas.indices) { // recorro la columna pedida
          // me salto la casilla objetivo original
          if (i != fila && ((casillas(i)(columna) & num) > 0)) {
            casillas(i)(columna) -= num

            // si al actualizar, un numero secundario colapsa a 1, 2, 3...
            if (esNumeroFinal(casillas(i)(columna))) {
              actualizarFilaColumnaYSector(i, columna, casillas(i)(columna))
            }
          }
        }

        // actualizo el sector
        // con cuidado de no actualizar las casillas ya actualizadas por "filas" y por "columnas"
        val sector: Int = getSector(fila, columna)
        for (i <- sector % 3 * 3 to sector % 3 * 3 + 2) { // fila del sector
          for (j <- sector / 3 * 3 to sector / 3 * 3 + 2) { // columna del sector
            if (i != fila && j != columna && ((casillas(i)(j) & num) > 0)) {
              casillas(i)(j) -= num

              // si al actualizar, un numero secundario colapsa a 1, 2, 3...
              if (esNumeroFinal(casillas(i)(j))) {
                actualizarFilaColumnaYSector(i, j, casillas(i)(j))
              }
            }
          }
        }
      }

      def esNumeroFinal(numero: Int): Boolean = {
        if (numero == 1 ||
          numero == 2 ||
          numero == 4 ||
          numero == 8 ||
          numero == 16 ||
          numero == 32 ||
          numero == 64 ||
          numero == 128 ||
          numero == 256) {
          return true
        }
        false
      }

      def obtenerSiguienteCasilla(tablero: Array[Array[Int]]): Array[Int] = {
        var iCasilla = 0
        var jCasilla = 0
        var posibilidadesMin = 9

        def obtenerSumaBits(casilla: Int): Int = {
          var aux = casilla
          var contador = 0
          while (aux > 0) {
            if ((aux & 1) > 0) {
              contador += 1
            }
            aux >>= 1
          }
          contador
        }

        val tableroAux: Array[Array[Int]] = Array.ofDim[Int](9, 9)
        for (i <- tableroAux.indices) {
          for (j <- tableroAux(i).indices) {
            val numero: Int = casillas(i)(j)
            var suma = 10
            if (numero != 1 &&
              numero != 2 &&
              numero != 4 &&
              numero != 8 &&
              numero != 16 &&
              numero != 32 &&
              numero != 64 &&
              numero != 128 &&
              numero != 256) {
              suma = obtenerSumaBits(numero)

              if (suma < posibilidadesMin) {
                posibilidadesMin = suma
                iCasilla = i
                jCasilla = j
              }
            }
          }
        }
        Array(iCasilla, jCasilla)
      }

      def inicializarCasilla(i: Int, j: Int): Unit = {
        // me aseguro que la casilla no estaba ya inicializada
        if (casillas(i)(j) == 1 ||
          casillas(i)(j) == 2 ||
          casillas(i)(j) == 4 ||
          casillas(i)(j) == 8 ||
          casillas(i)(j) == 16 ||
          casillas(i)(j) == 32 ||
          casillas(i)(j) == 64 ||
          casillas(i)(j) == 128 ||
          casillas(i)(j) == 256) {
          return
        }

        // inicializo un byte con solo un bit a 1, aleatorio entre los bits 0  y 9 menos significativos,
        // significando ...000 000 001 que se quiere colocar un 1 y ...100 000 000 que se quiere colocar un 9
        var mascara = scala.math.pow(2, rand.nextInt(9)).asInstanceOf[Int]
        var valido = false
        var contadorVueltas: Int = 0 // por algun motivo, a veces no se consigue inicializar el tablero
        // y se queda en este loop indefinidamente. si pasa, se vuelve a intentar
        while (!valido) {
          if ((casillas(i)(j) & mascara) != 0) { // distinto de 0, el numero esta disponible
            valido = true
            casillas(i)(j) = mascara
          } else {
            if (mascara == 1) { // es 000 000 001
              mascara = 256 // recoloco el bit que se iba a perder a la izq del tod0
              contadorVueltas += 1
              if (contadorVueltas > 2) {
                inicializarTablero()
              }
            } else { // es par, roto hacia la derecha en 1
              mascara >>= 1
            }
          }
        }
        // tengo un numero valido para colocar
        //casillas(i)(j) = scala.math.pow(2, mascara - 1).asInstanceOf[Int]

        // ahora toca quitar ese numero de las filas, columnas y sectores a los que afecta
        //val numero: Int = deByteAInt(mascara)
        val numero: Int = (mascara)
        actualizarFilaColumnaYSector(i, j, numero)
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////
      //
      //  aqui se llena el tablero
      //
      //

      // sector 1
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          inicializarCasilla(i, j)
        }
      }

      // sector 2
      for (i <- 3 until 6) {
        for (j <- 3 until 6) {
          inicializarCasilla(i, j)
        }
      }

      // sector 3
      for (i <- 6 until 9) {
        for (j <- 6 until 9) {
          inicializarCasilla(i, j)
        }
      }
      imprimirTablero(casillas)

      println("---------------------------------")

      for (i <- 0 until 54) {
        println(s"ite: ${i}")
        val sig = obtenerSiguienteCasilla(casillas)
        println(s"casilla: ${sig(0)}, ${sig(1)}")
        inicializarCasilla(sig(0), sig(1))
        imprimirTablero(traducirTablero(casillas))
      }











      // cambio los numeros de banderas de bits a los numeros de verdad

      imprimirTablero(traducirTablero(casillas))

    }

    def traducirTablero(tablero: Array[Array[Int]]): Array[Array[Int]] = {
      var tableroAux: Array[Array[Int]] = Array.ofDim[Int](9, 9)
      for (i <- tableroAux.indices) {
        for (j <- tableroAux(i).indices) {
          tableroAux(i)(j) = deByteAInt(tablero(i)(j))
        }
      }
      tableroAux
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
