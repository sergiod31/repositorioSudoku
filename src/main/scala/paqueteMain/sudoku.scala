package paqueteMain

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession

import scala.collection.mutable


object sudoku {

  def main(args: Array[String]): Unit = {
    Logger.getLogger("org").setLevel(Level.ERROR)
    val spark: SparkSession = SparkSession.builder()
      .master("local")
      .appName("prueba")
      .getOrCreate();

    val tablero = new Tablero
    val dificultadMaxima = 75


    def jugar(): Unit = {
      tablero.inicializarTablero(tablero.casillasJugador)

      println("")
      println("")
      println("")
      println("     ░██████╗██╗░░░██╗██████╗░░█████╗░██╗░░██╗██╗░░░██╗")
      println("     ██╔════╝██║░░░██║██╔══██╗██╔══██╗██║░██╔╝██║░░░██║")
      println("     ╚█████╗░██║░░░██║██║░░██║██║░░██║█████═╝░██║░░░██║")
      println("     ░╚═══██╗██║░░░██║██║░░██║██║░░██║██╔═██╗░██║░░░██║")
      println("     ██████╔╝╚██████╔╝██████╔╝╚█████╔╝██║░╚██╗╚██████╔╝")
      println("     ╚═════╝░░╚═════╝░╚═════╝░░╚════╝░╚═╝░░╚═╝░╚═════╝░")
      println("")
      println("")
      println("")

      while (nuevaPartida()) {
        println("nueva partida!")
      }
    }

    def pedirDificultad(): Int = {
      //
      // TESTEADO: funciona perfectamente, no tocar
      //
      // miro que la dificultad elegida sea un numero
      // y sea entre 1 y ${dificultadMaxima}

      print(s"     Elija dificultad (1 - ${dificultadMaxima}): ")
      var entradaCorrecta = false
      var dificultad = -1
      while (!entradaCorrecta) {
        entradaCorrecta = true
        val entradaChars: Array[Char] = scala.io.StdIn.readLine().toCharArray
        if (entradaChars.length > 2 ||
          entradaChars.length <= 0) {
          entradaCorrecta = false
          println(s"Introduzca un número del 1 al ${dificultadMaxima} por favor")
          print(s"     Elija dificultad (1 - ${dificultadMaxima}): ")
        } else {
          // ha introducido 1 ó 2 caracteres
          for (ite <- entradaChars.indices) {
            if (entradaChars(ite) < 48 || entradaChars(ite) > 57) {
              entradaCorrecta = false
            }
          }
          if (entradaCorrecta) {
            // ha introducido 1 o 2 digitos numericos
            // los paso a numero
            if (entradaChars.length == 1) {
              dificultad = entradaChars(0).toInt - 48
            } else {
              // supongo entradaChars.length como == 2
              dificultad = (entradaChars(0).toInt - 48) * 10 + (entradaChars(1).toInt - 48)
              if (dificultad > dificultadMaxima) {
                entradaCorrecta = false
                println(s"Introduzca un número del 1 al ${dificultadMaxima} por favor")
                print(s"     Elija dificultad (1 - ${dificultadMaxima}): ")
              }
            }
          } else {
            println(s"Introduzca un número del 1 al ${dificultadMaxima} por favor")
            print(s"     Elija dificultad (1 - ${dificultadMaxima}): ")
          }
        }
      }
      dificultad
    }

    def pedirFilaColumnaNum(): Array[Int] = {
      val casilla: Array[Int] = Array.ofDim[Int](3)

      var entradaCorrecta = false
      while (!entradaCorrecta) {
        entradaCorrecta = true
        print("Fila: ")
        val fila = scala.io.StdIn.readLine().toCharArray
        print("Columna: ")
        val columna = scala.io.StdIn.readLine().toCharArray
        print("Número: ")
        val num = scala.io.StdIn.readLine().toCharArray

        if (fila.length != 1 || columna.length != 1 || num.length != 1) {
          entradaCorrecta = false
        } else {
          if (fila(0) <= 48 || fila(0) > 57 ||
            columna(0) <= 48 || columna(0) > 57 ||
            num(0) <= 48 || num(0) > 57) {
            entradaCorrecta = false
          }
        }
        casilla(0) = fila(0).toInt - 48 - 1
        casilla(1) = columna(0).toInt - 48 - 1
        casilla(2) = num(0).toInt - 48
      }
      casilla
    }

    def pedirNuevaPartida(): Boolean = {
      println("¿Nueva partida (Y/N)?")
      val respuesta: Array[Char] = scala.io.StdIn.readLine().toCharArray
      if (respuesta(0) != 'Y' && respuesta(0) != 'y' &&
        respuesta(0) != 'N' && respuesta(0) != 'n') {
        // input erroneo
        return pedirNuevaPartida()
      }
      if (respuesta(0) == 'Y' ||
        respuesta(0) == 'y') {
        return true
      }
      false
    }

    // si al acabar quiere volver a jugar, retorna true, si no, false
    def nuevaPartida(): Boolean = {

      // pido dificultad
      var dificultad = pedirDificultad()

      // inicializo tableros
      tablero.inicializarTableroJugador(dificultad)
      tablero.imprimirTableroJugador(tablero.casillasJugador)

      // mientras la partida continue:
      while (!tablero.comprobarVictoria(tablero.casillasJugador)) {

        // pido (i, j)
        var casilla: Array[Int] = pedirFilaColumnaNum()

        // intento colocar el numero pedido
        while (!tablero.colocarNumero(tablero.casillasJugador, tablero.casillasJugadorBin, casilla(0), casilla(1), casilla(2))) {
          println("No se puede colocar ese número, intente con otro o en otra casilla")
          casilla = pedirFilaColumnaNum()
        }

        // he colocado un nuevo numero
        tablero.imprimirTableroJugador(tablero.casillasJugador)

        // compruebo ha llegado a un callejon sin salida
        if (!tablero.comprobarDerrota(tablero.casillasJugadorBin)) {

          // ha perdido
          println("No se puede continuar")
          println("")
          return pedirNuevaPartida()
        }
      }
      //
      pedirNuevaPartida()
    }

    //
    //jugar()

    val test = new Test
    test.ejecutarTest()


    ////////////////////////////////////////////////////////////////////////////


  }

  class Test {
    def tablero: Tablero = new Tablero

    def crearTableroTest(): Array[Array[Int]] = {
      val tablero: Array[Array[Int]] = Array.ofDim[Int](9, 9)

      val fila0: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val fila1: Array[Int] = Array(7, 8, 9, 1, 2, 3, 4, 5, 6)
      val fila2: Array[Int] = Array(4, 5, 6, 7, 8, 9, 1, 2, 3)
      val fila3: Array[Int] = Array(2, 3, 1, 5, 6, 4, 8, 9, 7)
      val fila4: Array[Int] = Array(8, 9, 7, 2, 3, 1, 5, 6, 4)
      val fila5: Array[Int] = Array(5, 6, 4, 8, 9, 7, 2, 3, 1)
      val fila6: Array[Int] = Array(3, 1, 2, 6, 4, 5, 9, 7, 8)
      val fila7: Array[Int] = Array(9, 7, 8, 3, 1, 2, 6, 4, 5)
      val fila8: Array[Int] = Array(6, 4, 5, 9, 7, 8, 3, 1, 2)

      for (i <- fila0.indices) {
        tablero(0)(i) = fila0(i)
      }
      for (i <- fila1.indices) {
        tablero(1)(i) = fila1(i)
      }
      for (i <- fila2.indices) {
        tablero(2)(i) = fila2(i)
      }
      for (i <- fila3.indices) {
        tablero(3)(i) = fila3(i)
      }
      for (i <- fila4.indices) {
        tablero(4)(i) = fila4(i)
      }
      for (i <- fila5.indices) {
        tablero(5)(i) = fila5(i)
      }
      for (i <- fila6.indices) {
        tablero(6)(i) = fila6(i)
      }
      for (i <- fila7.indices) {
        tablero(7)(i) = fila7(i)
      }
      for (i <- fila8.indices) {
        tablero(8)(i) = fila8(i)
      }

      tablero
    }

    def crearTableroBinTest(): Array[Array[Int]] = {
      val tablero: Array[Array[Int]] = Array.ofDim[Int](9, 9)

      val fila0: Array[Int] = Array(1, 2, 4, 8, 16, 32, 64, 128, 256)
      val fila1: Array[Int] = Array(64, 128, 256, 1, 2, 4, 8, 16, 32)
      val fila2: Array[Int] = Array(8, 16, 32, 64, 128, 256, 1, 2, 4)
      val fila3: Array[Int] = Array(2, 4, 1, 16, 32, 8, 128, 256, 64)
      val fila4: Array[Int] = Array(128, 256, 64, 2, 4, 1, 16, 32, 8)
      val fila5: Array[Int] = Array(16, 32, 8, 128, 256, 64, 2, 4, 1)
      val fila6: Array[Int] = Array(4, 1, 2, 32, 8, 16, 256, 64, 128)
      val fila7: Array[Int] = Array(256, 64, 128, 4, 1, 2, 32, 8, 16)
      val fila8: Array[Int] = Array(32, 8, 16, 256, 64, 128, 4, 1, 2)

      for (i <- fila0.indices) {
        tablero(0)(i) = fila0(i)
      }
      for (i <- fila1.indices) {
        tablero(1)(i) = fila1(i)
      }
      for (i <- fila2.indices) {
        tablero(2)(i) = fila2(i)
      }
      for (i <- fila3.indices) {
        tablero(3)(i) = fila3(i)
      }
      for (i <- fila4.indices) {
        tablero(4)(i) = fila4(i)
      }
      for (i <- fila5.indices) {
        tablero(5)(i) = fila5(i)
      }
      for (i <- fila6.indices) {
        tablero(6)(i) = fila6(i)
      }
      for (i <- fila7.indices) {
        tablero(7)(i) = fila7(i)
      }
      for (i <- fila8.indices) {
        tablero(8)(i) = fila8(i)
      }

      tablero
    }

    /////////////////////////////////////////////

    // funciona
    def test_deByteAInt(): Boolean = {
      val numeros: Array[Int] = Array(1, 2, 4, 8, 16, 32, 64, 128, 256)
      val soluciones: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

      val resultados: Array[Int] = new Array[Int](9)

      for (i <- numeros.indices) {
        resultados(i) = tablero.deByteAInt(numeros(i))
      }

      for (i <- soluciones.indices) {
        if (soluciones(i) != resultados(i)) {
          return false
        }
      }
      true
    }

    // funciona
    def test_deIntAByte(): Boolean = {
      val numeros: Array[Int] = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val soluciones: Array[Int] = Array(1, 2, 4, 8, 16, 32, 64, 128, 256)

      val resultados: Array[Int] = new Array[Int](9)

      for (i <- numeros.indices) {
        resultados(i) = tablero.deIntAByte(numeros(i))
      }

      for (i <- soluciones.indices) {
        if (soluciones(i) != resultados(i)) {
          return false
        }
      }
      true
    }

    // funciona
    def test_comprobarVictoria(): Boolean = {
      val tablero1: Array[Array[Int]] = crearTableroTest()
      val tablero2: Array[Array[Int]] = crearTableroTest()
      tablero2(0)(0) = 0

      tablero.comprobarVictoria(tablero1) && !tablero.comprobarVictoria(tablero2)
    }

    // funciona
    def test_comprobarDerrota(): Boolean = {
      val tableroTest: Array[Array[Int]] = crearTableroBinTest()
      val tableroTest2: Array[Array[Int]] = crearTableroBinTest()
      tableroTest(0)(0) = 0

      !tablero.comprobarDerrota(tableroTest) && tablero.comprobarDerrota(tableroTest2)
    }

    // funciona
    def test_comprobarNumeroValido(): Boolean = {
      val tablaTest: Array[Array[Int]] = crearTableroBinTest()
      val tablaTest2: Array[Array[Int]] = crearTableroBinTest()
      val tablaTest3: Array[Array[Int]] = crearTableroBinTest()

      tablaTest(0)(0) = 1
      tablaTest2(0)(0) = 3
      tablaTest3(0)(0) = 2

      if (tablero.comprobarNumeroValido(tablaTest, 0, 0, 1) &&
        tablero.comprobarNumeroValido(tablaTest2, 0, 0, 1) &&
        !tablero.comprobarNumeroValido(tablaTest3, 0, 0, 1)) {
        return true
      }
      false
    }

    // funciona
    def test_colocarNumero(): Boolean = {
      val tableroTest: Array[Array[Int]] = crearTableroTest()
      val tableroBinTest: Array[Array[Int]] = crearTableroBinTest()

      val tableroTest2: Array[Array[Int]] = crearTableroTest()

      tableroTest(0)(0) = 0
      tableroBinTest(0)(0) = 1

      val correcto1: Boolean = tablero.colocarNumero(tableroTest, tableroBinTest, 0, 0, 1)

      if (correcto1) {
        return true
      }
      false
    }

    // funciona
    def test_getSector(): Boolean = {
      val solucionesTest: Array[Int] = Array(0, 0, 0, 3, 2, 1, 4, 5, 5, 6, 7, 8)
      val resultados: Array[Int] = new Array[Int](12)

      //                                         0       0      0        3       2       1       4       5       5       6       7       8
      val casillas: Array[(Int, Int)] = Array((0, 0), (0, 1), (0, 2), (3, 0), (0, 6), (0, 3), (3, 3), (3, 6), (3, 8), (6, 0), (6, 3), (6, 6))

      for (i <- casillas.indices) {
        resultados(i) = tablero.getSector(casillas(i)._1, casillas(i)._2)
      }
      for (i <- resultados.indices) {
        if (resultados(i) != solucionesTest(i)) {
          return false
        }
      }
      true
    }

    // funciona
    def test_actualizarFilaColumnaYSector(): Boolean = {
      val tableroBinTest: Array[Array[Int]] = Array.ofDim[Int](9, 9) // test del metodo
      val tableroBinTest2: Array[Array[Int]] = Array.ofDim[Int](9, 9) // objetivo
      //
      for (i <- tableroBinTest.indices; j <- tableroBinTest(i).indices) {
        tableroBinTest(i)(j) = 511
        tableroBinTest2(i)(j) = 511
      }

      tableroBinTest2(0)(0) = 511
      tableroBinTest2(0)(1) -= 1
      tableroBinTest2(0)(2) -= 1
      tableroBinTest2(1)(0) -= 1
      tableroBinTest2(1)(1) -= 1
      tableroBinTest2(1)(2) -= 1
      tableroBinTest2(2)(0) -= 1
      tableroBinTest2(2)(1) -= 1
      tableroBinTest2(2)(2) -= 1
      //
      for (j <- 3 until 9) {
        tableroBinTest2(0)(j) -= 1
        tableroBinTest2(j)(0) -= 1
      }

      tablero.actualizarFilaColumnaYSector(0, 0, 1, tableroBinTest)

      for (i <- tableroBinTest.indices; j <- tableroBinTest(i).indices) {
        if (tableroBinTest(i)(j) != tableroBinTest2(i)(j)) {
          print(s"\n\t(${i})(${j}) -> tableroBinTest: ${tableroBinTest(i)(j)}, tableroBinTest2: ${tableroBinTest2(i)(j)}\t\t")
          return false
        }
      }
      true
    }

    def test_inicializarTablero(): Boolean = {
      println("\n")
      try {
        val tableroTest: Array[Array[Int]] = tablero.inicializarTablero(Array.ofDim[Int](9, 9))
        // tablero.imprimirTableroJugador(tableroTest)
        if (tablero.comprobarVictoria(tableroTest)) {
          return true
        }
      }
      false
    }

    def test_inicializarTableroJugador(): Boolean = {
      false
    }


    // funciona
    def test_traducirTablero(): Boolean = {
      val tabla: Array[Array[Int]] = crearTableroTest()
      val tablaBin: Array[Array[Int]] = crearTableroBinTest()

      val tablaTest: Array[Array[Int]] = tablero.traducirTablero(tablaBin)

      for (i <- tabla.indices; j <- tabla(i).indices) {
        if (tabla(i)(j) != tablaTest(i)(j)) {
          print(s"\n\t(${i})(${j}) -> tabla: ${tabla(i)(j)}, tablaTest: ${tablaTest(i)(j)}\t\t")
          return false
        }
      }
      true
    }

    def ejecutarTest() {

      println("PASANDO TESTS:")
      println("")
      print("test_deByteAInt: ")
      if (test_deByteAInt()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_deIntAByte: ")
      if (test_deIntAByte()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_comprobarVictoria: ")
      if (test_comprobarVictoria()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_comprobarDerrota: ")
      if (test_comprobarDerrota()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_comprobarNumeroValido: ")
      if (test_comprobarNumeroValido()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_colocarNumero: ")
      if (test_colocarNumero()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_getSector: ")
      if (test_getSector()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_actualizarFilaColumnaYSector: ")
      if (test_actualizarFilaColumnaYSector()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_inicializarTablero: ")
      if (test_inicializarTablero()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_inicializarTableroJugador: ")
      if (test_inicializarTableroJugador()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
      print("test_traducirTablero: ")
      if (test_traducirTablero()) {
        println("TRUE")
      } else {
        println("FALSE <<<<<<<<<<<<<<<<<<<")
      }
    }
  }

  class Tablero {
    val dimension: Int = 9
    //
    //        0    1    2         ┌───>   i: fila
    //      ┌────┬────┬────┐      │
    //   0  │ 0  │ 1  │ 2  │      V
    //      ├────┼────┼────┤
    //   1  │ 3  │ 4  │ 5  │      j: columna
    //      ├────┼────┼────┤
    //   2  │ 6  │ 7  │ 8  │
    //      └────┴────┴────┘
    //
    //  casillas solucion
    val casillas: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
    //
    //  casillas para el jugador
    var casillasJugador: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
    //
    // casillas que comprueban si se puede seguir jugando
    var casillasJugadorBin: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
    //

    def deByteAInt(num: Int): Int = {
      //  000 100 000 -> 6
      //  000 001 000 -> 4
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

      // que no se de esto nunca, por dios
      if (num == 0) {
        return 0
      }
      -1
    }

    def deIntAByte(num: Int): Int = {
      //  6 -> 000 100 000
      scala.math.pow(2, num - 1).asInstanceOf[Int]
    }

    def comprobarVictoria(tablero: Array[Array[Int]]): Boolean = {
      // busco horizontalmente
      for (i <- 0 until 9) {
        val mapa: mutable.HashMap[Int, Int] = mutable.HashMap()
        for (j <- 0 until 9) {
          if (tablero(i)(j) == 0) {
            return false
          }
          mapa += (tablero(i)(j) -> j)
        }
        // si hay menos de 9 elementos, es que hay alguno repetido
        if (mapa.size < 9) {
          return false
        }
      }
      // busco verticalmente
      for (j <- 0 until 9) {
        val mapa: mutable.HashMap[Int, Int] = mutable.HashMap()
        for (i <- 0 until 9) {
          if (tablero(i)(j) == 0) {
            return false
          }
          mapa += (tablero(i)(j) -> i)
        }
        // si hay menos de 9 elementos, es que hay alguno repetido
        if (mapa.size < 9) {
          return false
        }
      }

      // busco en cada sector
      for (sector <- 0 until 9) {
        val mapa: mutable.HashMap[Int, (Int, Int)] = mutable.HashMap()
        for (i <- (sector / 3 * 3) until (sector / 3 * 3) + 3) {
          for (j <- (sector % 3 * 3) until (sector % 3 * 3 + 3)) {
            if (tablero(i)(j) == 0) {
              return false
            }
            mapa += (tablero(i)(j) -> (i, j))
          }
        }
        if (mapa.size < 9) {
          return false
        }
      }
      true
    }

    // false -> derrota
    def comprobarDerrota(tablero: Array[Array[Int]]): Boolean = {
      for (i <- tablero.indices; j <- tablero(i).indices) {
        if (tablero(i)(j) == 0) {
          return false
        }
      }
      true
    }

    // la tabla que se le pasa es "bin"
    def comprobarNumeroValido(tablaBin: Array[Array[Int]], fila: Int, columna: Int, num: Int): Boolean = {
      if ((tablaBin(fila)(columna) & deIntAByte(num)) > 0) {
        return true
      }
      false
    }

    // comprueba si se puede colocar el numero, actualiza el tablero en contador binario y actualiza el tablero
    def colocarNumero(tablero: Array[Array[Int]], tableroBin: Array[Array[Int]], fila: Int, columna: Int, num: Int): Boolean = {
      if (!comprobarNumeroValido(tableroBin, fila, columna, num)) {
        return false
      }

      // se puede colocar
      actualizarFilaColumnaYSector(fila, columna, num, tableroBin)

      tablero(fila)(columna) = num
      true
    }

    // obtiene, de una fila + columna, a que sector pertenece
    // va de [0 - 8]
    def getSector(fila: Int, columna: Int): Int = {
      ((fila / 3) * 3) + (columna / 3)
    }

    // el tablero en contador binario!
    def actualizarFilaColumnaYSector(fila: Int, columna: Int, num: Int, tablero: Array[Array[Int]]): Unit = {

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

      // actualizo la fila
      for (j <- tablero(fila).indices) { // recorro la fila pedida
        // me salto la casilla objetivo original y los numeros ya finales
        if (j != columna && ((tablero(fila)(j) & num) > 0)) {
          tablero(fila)(j) -= num

          // si al actualizar, un numero secundario colapsa a 1, 2, 3...
          if (esNumeroFinal(tablero(fila)(j))) {
            actualizarFilaColumnaYSector(fila, j, tablero(fila)(j), tablero)
          }
        }
      }
      // actualizo la columna
      for (i <- tablero.indices) { // recorro la columna pedida
        // me salto la casilla objetivo original y los numeros ya finales
        if (i != fila && ((tablero(i)(columna) & num) > 0)) {
          tablero(i)(columna) -= num

          // si al actualizar, un numero secundario colapsa a 1, 2, 3...
          if (esNumeroFinal(tablero(i)(columna))) {
            actualizarFilaColumnaYSector(i, columna, tablero(i)(columna), tablero)
          }
        }
      }

      // actualizo el sector
      // con cuidado de no actualizar las casillas ya actualizadas por "filas" y por "columnas"
      val sector: Int = getSector(fila, columna)

      for (i <- sector % 3 * 3 to sector % 3 * 3 + 2) { // fila del sector
        for (j <- sector / 3 * 3 to sector / 3 * 3 + 2) { // columna del sector

          // me salto la casilla objetivo original, toda la fila, la columna y los numeros ya finales
          if (i != fila && j != columna && ((tablero(i)(j) & num) > 0)) {
            tablero(i)(j) -= num

            // si al actualizar, un numero secundario colapsa a un numero
            if (esNumeroFinal(tablero(i)(j))) {
              actualizarFilaColumnaYSector(i, j, tablero(i)(j), tablero)
            }
          }
        }
      }
    }

    /*
     * genero los 81 numeros para el tablero
     */
    def inicializarTablero(tablero: Array[Array[Int]]): Array[Array[Int]] = {
      val rand = scala.util.Random
      val tableroBin: Array[Array[Int]] = Array.ofDim[Int](9, 9)
      // relleno tablero de '111 111 111'
      for (i <- tableroBin.indices) {
        for (j <- tableroBin(i).indices) {
          tableroBin(i)(j) = 511
        }
      }

      def obtenerSiguienteCasilla(): Array[Int] = {
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

        for (i <- tableroBin.indices) {
          for (j <- tableroBin(i).indices) {
            val numero: Int = tableroBin(i)(j)
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
        if (tableroBin(i)(j) == 1 ||
          tableroBin(i)(j) == 2 ||
          tableroBin(i)(j) == 4 ||
          tableroBin(i)(j) == 8 ||
          tableroBin(i)(j) == 16 ||
          tableroBin(i)(j) == 32 ||
          tableroBin(i)(j) == 64 ||
          tableroBin(i)(j) == 128 ||
          tableroBin(i)(j) == 256) {
          return
        }

        // inicializo un byte con solo un bit a 1, aleatorio entre los bits 0  y 9 menos significativos,
        // significando ...000 000 001 que se quiere colocar un 1 y ...100 000 000 que se quiere colocar un 9
        var mascara = scala.math.pow(2, rand.nextInt(9)).asInstanceOf[Int]
        var valido = false

        var contadorVueltas: Int = 0
        // por algun motivo, a veces no se consigue inicializar el tablero
        // y se queda en este loop indefinidamente. si pasa, se vuelve a intentar
        while (!valido) {

          if ((tableroBin(i)(j) & mascara) != 0) {

            // distinto de 0, el numero esta disponible
            valido = true
            tableroBin(i)(j) = mascara
            tablero(i)(j) = deByteAInt(mascara)

            // ahora toca quitar ese numero de las filas, columnas y sectores a los que afecta
            val numero: Int = (mascara)
            actualizarFilaColumnaYSector(i, j, numero, tableroBin)
          } else {

            // el numero de la mascara no esta disponible
            if (mascara == 1) { // es 000 000 001
              mascara = 256 // recoloco el bit que se iba a perder a la izq del tod0
              contadorVueltas += 1
              if (contadorVueltas > 2) {
                //imprimirTablero()
                //return
                println("imprimiendo tablero justo antes de volver a intentarlo")
                imprimirTableroJugador(tableroBin)
                inicializarTablero(tableroBin)
              }
            } else { // es par, roto hacia la derecha en 1
              mascara >>= 1
            }
          }
        }
      }

      /////////////////////////////////////////////////////////////////////////////////////////////////
      //
      //  aqui se llena el tablero
      //
      //

      // sector 1
      for (i <- 0 until 3) {
        for (j <- 0 until 3) {
          println(s"inicializando casilla (${i}, ${j})")
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

      // inicializo el resto de las casillas. lo normal es que alrededor de 20 ciclos queden vacios
      for (_ <- 0 until 54) {
        val sig = obtenerSiguienteCasilla()
        inicializarCasilla(sig(0), sig(1))
      }
      tablero
    }

    def inicializarTableroJugador(dificultad: Int): Unit = {
      casillasJugador = traducirTablero(casillas)

      val rand = scala.util.Random

      val mapa = new mutable.HashMap[Int, Array[Int]]
      for (_ <- 0 until dificultad) {
        // se podria cambiar por un do while
        var i: Int = rand.nextInt(9)
        var j: Int = rand.nextInt(9)
        var casilla: Array[Int] = Array[Int](i, j)
        while (mapa.contains(i * 10 + j)) {
          i = rand.nextInt(9)
          j = rand.nextInt(9)
          casilla = Array[Int](i, j)
        }
        mapa += (i * 10 + j -> casilla)
      }
      // ya tengo que casillas eliminar

      mapa.foreach({
        case (_, value) => casillasJugador(value(0))(value(1)) = 0
      })

      // inicializo casillasJugadorBin
      for (i <- casillasJugadorBin.indices; j <- casillasJugadorBin(i).indices) {
        casillasJugadorBin(i)(j) = 511
      }

      // actualizo casillasJugadorBin segun casillasJugador
      for (i <- casillasJugador.indices; j <- casillasJugador(i).indices) {
        if (casillasJugador(i)(j) != 0) {
          actualizarFilaColumnaYSector(i, j, deIntAByte(casillasJugador(i)(j)), casillasJugadorBin)
        }
      }
    }

    def traducirTablero(tablero: Array[Array[Int]]): Array[Array[Int]] = {
      val tableroAux: Array[Array[Int]] = Array.ofDim[Int](9, 9)
      for (i <- tableroAux.indices) {
        for (j <- tableroAux(i).indices) {
          tableroAux(i)(j) = deByteAInt(tablero(i)(j))
        }
      }
      tableroAux
    }

    def imprimirTablero(): Unit = {
      val tablero = casillas
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

    def imprimirTableroJugador(tablero: Array[Array[Int]]): Unit = {
      val tableroAux: Array[Array[String]] = Array.ofDim[String](9, 9)

      for (i <- tableroAux.indices; j <- tableroAux(i).indices) {
        tableroAux(i)(j) = tablero(i)(j).toString
        if (tablero(i)(j) == 0) {
          tableroAux(i)(j) = " "
        }
      }
      println("")
      println("    1    2    3    4    5    6    7    8    9")
      println("  ┌────┬────┬────┬────┬────┬────┬────┬────┬────┐  ")
      println(s"1 │ ${tablero(0)(0)}  │ ${tablero(0)(1)}  │ ${tablero(0)(2)}  │ ${tablero(0)(3)}  │ ${tablero(0)(4)}  │ ${tablero(0)(5)}  │  ${tablero(0)(6)} │ ${tablero(0)(7)}  │ ${tablero(0)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"2 │ ${tablero(1)(0)}  │ ${tablero(1)(1)}  │ ${tablero(1)(2)}  │ ${tablero(1)(3)}  │ ${tablero(1)(4)}  │ ${tablero(1)(5)}  │  ${tablero(1)(6)} │ ${tablero(1)(7)}  │ ${tablero(1)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"3 │ ${tablero(2)(0)}  │ ${tablero(2)(1)}  │ ${tablero(2)(2)}  │ ${tablero(2)(3)}  │ ${tablero(2)(4)}  │ ${tablero(2)(5)}  │  ${tablero(2)(6)} │ ${tablero(2)(7)}  │ ${tablero(2)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"4 │ ${tablero(3)(0)}  │ ${tablero(3)(1)}  │ ${tablero(3)(2)}  │ ${tablero(3)(3)}  │ ${tablero(3)(4)}  │ ${tablero(3)(5)}  │  ${tablero(3)(6)} │ ${tablero(3)(7)}  │ ${tablero(3)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"5 │ ${tablero(4)(0)}  │ ${tablero(4)(1)}  │ ${tablero(4)(2)}  │ ${tablero(4)(3)}  │ ${tablero(4)(4)}  │ ${tablero(4)(5)}  │  ${tablero(4)(6)} │ ${tablero(4)(7)}  │ ${tablero(4)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"6 │ ${tablero(5)(0)}  │ ${tablero(5)(1)}  │ ${tablero(5)(2)}  │ ${tablero(5)(3)}  │ ${tablero(5)(4)}  │ ${tablero(5)(5)}  │  ${tablero(5)(6)} │ ${tablero(5)(7)}  │ ${tablero(5)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"7 │ ${tablero(6)(0)}  │ ${tablero(6)(1)}  │ ${tablero(6)(2)}  │ ${tablero(6)(3)}  │ ${tablero(6)(4)}  │ ${tablero(6)(5)}  │  ${tablero(6)(6)} │ ${tablero(6)(7)}  │ ${tablero(6)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"8 │ ${tablero(7)(0)}  │ ${tablero(7)(1)}  │ ${tablero(7)(2)}  │ ${tablero(7)(3)}  │ ${tablero(7)(4)}  │ ${tablero(7)(5)}  │  ${tablero(7)(6)} │ ${tablero(7)(7)}  │ ${tablero(7)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"9 │ ${tablero(8)(0)}  │ ${tablero(8)(1)}  │ ${tablero(8)(2)}  │ ${tablero(8)(3)}  │ ${tablero(8)(4)}  │ ${tablero(8)(5)}  │  ${tablero(8)(6)} │ ${tablero(8)(7)}  │ ${tablero(8)(8)}  │")
      println("  └────┴────┴────┴────┴────┴────┴────┴────┴────┘")
    }

    def imprimirTableroJugadorBin(): Unit = {
      println("TABLERO JUGADOR BIN:")
      for (i <- casillasJugadorBin.indices) {
        for (j <- casillasJugadorBin(i).indices) {
          print(s"${casillasJugadorBin(i)(j).toBinaryString} - ")
        }
        println("")
      }
    }
  }
}
