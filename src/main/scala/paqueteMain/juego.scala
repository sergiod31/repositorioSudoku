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

    def inicializarTablero(): Unit = {

    }
  }


}
