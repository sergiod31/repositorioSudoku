package paqueteMain

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession

import scala.collection.mutable


object sudoku {

  def main(args: Array[String]): Unit = {
    Logger.getLogger("org").setLevel(Level.ERROR);

    val tablero = new Tablero
    val dificultadMaxima = 75


    def jugar(): Unit = {

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

      while (nuevaPartida()) { // mientras al acabar, el usuario quiera volver a jugar
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

      tablero.inicializarTablero()

      // pido dificultad
      val dificultad = pedirDificultad()

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

    jugar()
  }

  class Tablero {
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
    // tamaño del cuadrado (por defecto, 9x9)
    val dimension: Int = 9
    //
    //  casillas solucion
    val casillas: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
    //
    //  casillas para el jugador
    var casillasJugador: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
    //
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

    //
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
        if (tablero(i)(j) != 0) {
          return true
        }
      }
      false
    }

    // la tabla que se le pasa es "bin"
    def comprobarNumeroValido(tablaBin: Array[Array[Int]], fila: Int, columna: Int, num: Int): Boolean = {
      println(s"comprobarNumeroValido - (${fila}, ${columna}) -> ${tablaBin(fila)(columna)}")
      if ((tablaBin(fila)(columna) & deIntAByte(num)) > 0) {
        return true
      }
      false
    }


    // comprueba si se puede colocar el numero, actualiza el tablero en contador binario y actualiza el tablero
    def colocarNumero(tablero: Array[Array[Int]], tableroBin: Array[Array[Int]], fila: Int, columna: Int, num: Int): Boolean = {

      def actualizarFilaColumnaYSector(fila: Int, columna: Int, num: Int, tableroBin: Array[Array[Int]]): Unit = {
        val sector = getSectorBien(fila, columna)

        // actualizo fila
        for (j <- tableroBin.indices) {
          if ((tableroBin(fila)(j) & num) > 0) { // es una casilla con esa posibilidad de num
            tableroBin(fila)(j) -= deIntAByte(num)
          }
        }

        // actualizo columna
        for (i <- tableroBin(fila).indices) {
          if ((tableroBin(i)(columna) & num) > 0) { // es una casilla con esa posibilidad de num
            tableroBin(i)(columna) -= deIntAByte(num)
          }
        }

        // actualizo sector
        for (i <- sector % 3 * 3 to sector % 3 * 3 + 2;
             j <- sector / 3 * 3 to sector / 3 * 3 + 2) {
          if ((tableroBin(i)(columna) & num) > 0) { // es una casilla con esa posibilidad de num
            tableroBin(i)(j) -= deIntAByte(num)
          }
        }
      }


      if (!comprobarNumeroValido(tableroBin, fila, columna, num)) {
        // TODO: borrar
        println("numero no valido")
        println(s"se intenta colocar ${num} en (${fila},${columna})")
        return false
      }

      // se puede colocar
      actualizarFilaColumnaYSector(fila, columna, num, tableroBin)
      tablero(fila)(columna) = num
      true
    }


    // obtiene, de una fila + columna, a que sector pertenece
    // TODO: en realidad deberia ser  ((fila / 3) * 3) + (columna / 3), pero bien puesto, todo casca porque patata :/
    def getSector(fila: Int, columna: Int): Int = {
      (fila / 3) + ((columna / 3) * 3)
      // ((fila / 3) * 3) + (columna / 3)
    }

    def getSectorBien(fila: Int, columna: Int): Int = {
      ((fila / 3) * 3) + (columna / 3)
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

      def traducirTablero(tableroBin: Array[Array[Int]]): Array[Array[Int]] = {
        val tableroAux: Array[Array[Int]] = Array.ofDim[Int](dimension, dimension)
        for (i <- tableroBin.indices; j <- tableroBin(i).indices) {
          tableroAux(i)(j) = deByteAInt(tableroBin(i)(j))
        }
        tableroAux
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

      // inicializo el resto de las casillas. lo normal es que alrededor de 20 ciclos queden vacios
      for (_ <- 0 until 54) {
        val sig = obtenerSiguienteCasilla()
        inicializarCasilla(sig(0), sig(1))
      }
      casillasJugador = traducirTablero(casillas)
    }

    // con el tablero de numeros del 1 al 9 ya inicializado,
    // lo preparo para jugar quitandole casillas y
    // creando otro tableroBin para controlar si el usuario intenta meter un numero valido o no
    def inicializarTableroJugador(dificultad: Int): Unit = {

      //
      def actualizarFilaColumnaYSectorBin(fila: Int, columna: Int, num: Int): Unit = {

        // actualizo columna
        for (i <- casillasJugadorBin.indices) {
          if ((casillasJugadorBin(i)(columna) & num) > 0) { // si contiene el num
            casillasJugadorBin(i)(columna) -= num
          }
        }

        // actualizo fila
        for (j <- casillasJugadorBin(fila).indices) {
          if ((casillasJugadorBin(fila)(j) & num) > 0) {
            casillasJugadorBin(fila)(j) -= num
          }
        }

        // actualizo sector
        val sector: Int = getSectorBien(fila, columna)
        for (j <- sector % 3 * 3 to sector % 3 * 3 + 2; // fila del sector
             i <- sector / 3 * 3 to sector / 3 * 3 + 2) { // columna del sector
          if ((casillasJugadorBin(i)(j) & num) > 0) { // si esa casilla contiene el numero a eliminar
            casillasJugadorBin(i)(j) -= num
          }
        }
      }

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
      // ya tengo las casillas a eliminar

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
          actualizarFilaColumnaYSectorBin(i, j, deIntAByte(casillasJugador(i)(j)))
          casillasJugadorBin(i)(j) = 0
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
      println(s"1 │ ${tableroAux(0)(0)}  │ ${tableroAux(0)(1)}  │ ${tableroAux(0)(2)}  │ ${tableroAux(0)(3)}  │ ${tableroAux(0)(4)}  │ ${tableroAux(0)(5)}  │  ${tableroAux(0)(6)} │ ${tableroAux(0)(7)}  │ ${tableroAux(0)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"2 │ ${tableroAux(1)(0)}  │ ${tableroAux(1)(1)}  │ ${tableroAux(1)(2)}  │ ${tableroAux(1)(3)}  │ ${tableroAux(1)(4)}  │ ${tableroAux(1)(5)}  │  ${tableroAux(1)(6)} │ ${tableroAux(1)(7)}  │ ${tableroAux(1)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"3 │ ${tableroAux(2)(0)}  │ ${tableroAux(2)(1)}  │ ${tableroAux(2)(2)}  │ ${tableroAux(2)(3)}  │ ${tableroAux(2)(4)}  │ ${tableroAux(2)(5)}  │  ${tableroAux(2)(6)} │ ${tableroAux(2)(7)}  │ ${tableroAux(2)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"4 │ ${tableroAux(3)(0)}  │ ${tableroAux(3)(1)}  │ ${tableroAux(3)(2)}  │ ${tableroAux(3)(3)}  │ ${tableroAux(3)(4)}  │ ${tableroAux(3)(5)}  │  ${tableroAux(3)(6)} │ ${tableroAux(3)(7)}  │ ${tableroAux(3)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"5 │ ${tableroAux(4)(0)}  │ ${tableroAux(4)(1)}  │ ${tableroAux(4)(2)}  │ ${tableroAux(4)(3)}  │ ${tableroAux(4)(4)}  │ ${tableroAux(4)(5)}  │  ${tableroAux(4)(6)} │ ${tableroAux(4)(7)}  │ ${tableroAux(4)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"6 │ ${tableroAux(5)(0)}  │ ${tableroAux(5)(1)}  │ ${tableroAux(5)(2)}  │ ${tableroAux(5)(3)}  │ ${tableroAux(5)(4)}  │ ${tableroAux(5)(5)}  │  ${tableroAux(5)(6)} │ ${tableroAux(5)(7)}  │ ${tableroAux(5)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"7 │ ${tableroAux(6)(0)}  │ ${tableroAux(6)(1)}  │ ${tableroAux(6)(2)}  │ ${tableroAux(6)(3)}  │ ${tableroAux(6)(4)}  │ ${tableroAux(6)(5)}  │  ${tableroAux(6)(6)} │ ${tableroAux(6)(7)}  │ ${tableroAux(6)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"8 │ ${tableroAux(7)(0)}  │ ${tableroAux(7)(1)}  │ ${tableroAux(7)(2)}  │ ${tableroAux(7)(3)}  │ ${tableroAux(7)(4)}  │ ${tableroAux(7)(5)}  │  ${tableroAux(7)(6)} │ ${tableroAux(7)(7)}  │ ${tableroAux(7)(8)}  │")
      println("  ├────┼────┼────┼────┼────┼────┼────┼────┼────┤")
      println(s"9 │ ${tableroAux(8)(0)}  │ ${tableroAux(8)(1)}  │ ${tableroAux(8)(2)}  │ ${tableroAux(8)(3)}  │ ${tableroAux(8)(4)}  │ ${tableroAux(8)(5)}  │  ${tableroAux(8)(6)} │ ${tableroAux(8)(7)}  │ ${tableroAux(8)(8)}  │")
      println("  └────┴────┴────┴────┴────┴────┴────┴────┴────┘")
    }
  }


}
