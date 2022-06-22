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

      def generar10NumsRandoms(): Array[Int] = {
        val nums: Array[Int] = Array[Int](10)
        var numsAux: Array[Int] = Array[Int](10)
        for (x <- numsAux.indices) { // genero un array de length 10, con ints de 1 a 9
          numsAux(x) = x + 1
        }
        for (x <- nums.indices) {
          val posAle: Int = rand.nextInt(numsAux.length)
          nums(x) = numsAux(posAle)
          val arrayAux: Array[Int] = Array[Int](numsAux.length - 1)
          var ite: Int = 0
          for (i <- numsAux.indices) { // elimino el numero utilizado
            if (i != posAle) {
              arrayAux(ite) = numsAux(i)
              ite += 1
            }
          }
          numsAux = arrayAux
        }
        numsAux
      }

      // metodo aux para "inicializarTablero" (inserta num en posicion i, j)
      def insertarNumero(i: Int, j: Int) = {


        // elijo un numero random
        var valido = false
        var numRandom: Int = rand.nextInt(9) + 1 // inicializo numRandom con un numero entre 1 y 9
        var mascara = (scala.math.pow(2, numRandom - 1)).asInstanceOf[Int].toByte
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
          casillas(i)(j) = numRandom
        }
        // tengo un numero valido para colocar


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
      }

      // me aseguro que los tableros estan populados con nulls
      for (i <- casillas.indices) {
        for (j <- casillas(i).indices) {
          casillas(i)(j) = null
          casillasJugador(i)(j) = null
        }
      }
      // creo un tablero auxiliar

      for (i <- tableroAux.indices) {
        for (j <- tableroAux(i).indices) {
          for (k <- tableroAux(i)(j).indices) { // cada casilla es un array [1 - 9]
            tableroAux(i)(j)(k) = k + 1
          }
        }
      }

    }


  }


}
