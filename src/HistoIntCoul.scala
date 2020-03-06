package recherche
import com.tncy.top.image.ImageWrapper;

class HistoIntCoul(var src: String, var mot: String, var dst: String) {
  var sourceFileName: String = src; /*Nom du fichier image*/

  var patternFileName: String = mot; /*Nom du fichier motif*/

  var wrappedImageSource: ImageWrapper = new ImageWrapper(sourceFileName); /*Charger le fichier image source*/

  var wrappedImagePattern: ImageWrapper = new ImageWrapper(patternFileName) /*Charger le fichier image motif*/

  var image2DSource: Array[Array[Int]] = wrappedImageSource.getImage(); /*Tableau 2 dimensions, chaque case est un pixel de la source*/

  var image2DPattern: Array[Array[Int]] = wrappedImagePattern.getImage(); /*Tableau 2 dimensions, chaque case est un pixel du motif*/

  //Compare les valeurs de deux tableaux, renvoie true si toute les valeurs sont identiques, false sinon
  def compare(t1: Array[Int], t2: Array[Int]): Boolean = {
    if (t1.length == t2.length) {
      for (i <- 0 to t1.length - 1) {
        if (t1(i) != t2(i)) {
          //println(i + " est faux " + t1(i) + " != " + t2(i))
          return false
        }
      }
      return true
    } else {
      println("taille t1 = " + t1.length + "taille t2 = " + t2.length)
      return false
    }
  }
  //encadre une zone de la taille du motif dans l'image
  def encadre(l: Int, c: Int) {
    for (i <- 0 to wrappedImagePattern.width - 1) {
      image2DSource(l)(c + i) = Integer.parseInt("FF0000", 16)
      image2DSource(l + wrappedImagePattern.height - 1)(c + i) = Integer.parseInt("FF0000", 16)
    }
    for (j <- 0 to wrappedImagePattern.height - 1) {
      image2DSource(l + j)(c) = Integer.parseInt("FF0000", 16)
      image2DSource(l + j)(c + wrappedImagePattern.width - 1) = Integer.parseInt("FF0000", 16)
    }

  }
  //permet de créer l'histogramme intégrale des valeurs rouges de la source
  def creaHistoIntR(): Array[Array[Array[Int]]] = {
    var hI: Array[Array[Array[Int]]] = Array.fill(wrappedImageSource.height + 1, wrappedImageSource.width + 1, 256)(0)
    var hexaPixel: String = ""
    var hexaPixelList: List[Char] = Nil
    var redPixel: List[Char] = Nil
    var intRed: Int = 0
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      redPixel = image2DSource(x - 1)(y - 1).toHexString.toList.takeRight(6).take(2)
      intRed = (16 * Integer.parseInt(redPixel.head.toString, 16)) + (Integer.parseInt(redPixel.tail.head.toString, 16))
      for (i <- x to wrappedImageSource.height; j <- y to wrappedImageSource.width) {
        hI(i)(j)(intRed) += 1
      }
    }
    return hI
  }
  
  //permet de créer l'histogramme intégrale des valeurs vertes de la source
  def creaHistoIntG(): Array[Array[Array[Int]]] = {
    var hI: Array[Array[Array[Int]]] = Array.fill(wrappedImageSource.height + 1, wrappedImageSource.width + 1, 256)(0)
    var hexaPixel: String = ""
    var hexaPixelList: List[Char] = Nil
    var greenPixel: List[Char] = Nil
    var intGreen: Int = 0
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      greenPixel = image2DSource(x - 1)(y - 1).toHexString.toList.takeRight(4).take(2)
      intGreen = (16 * Integer.parseInt(greenPixel.head.toString, 16)) + (Integer.parseInt(greenPixel.tail.head.toString, 16))
      for (i <- x to wrappedImageSource.height; j <- y to wrappedImageSource.width) {
        hI(i)(j)(intGreen) += 1
      }
    }
    return hI
  }
  
  //permet de créer l'histogramme intégrale des valeurs bleus de la source
  def creaHistoIntB(): Array[Array[Array[Int]]] = {
    var hI: Array[Array[Array[Int]]] = Array.fill(wrappedImageSource.height + 1, wrappedImageSource.width + 1, 256)(0)
    var hexaPixel: String = ""
    var hexaPixelList: List[Char] = Nil
    var bluePixel: List[Char] = Nil
    var intBlue: Int = 0
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      bluePixel = image2DSource(x - 1)(y - 1).toHexString.toList.takeRight(2)
      intBlue = (16 * Integer.parseInt(bluePixel.head.toString, 16)) + (Integer.parseInt(bluePixel.tail.head.toString, 16))
      for (i <- x to wrappedImageSource.height; j <- y to wrappedImageSource.width) {
        hI(i)(j)(intBlue) += 1
      }
    }
    return hI
  }
  //permet de créer l'histogramme des valeurs rouges du motif
  def histoMotifR(): Array[Int] = {
    var hM: Array[Int] = Array.fill(256)(0)
    for (x <- 0 to wrappedImagePattern.height - 1; y <- 0 to wrappedImagePattern.width - 1) {
      var hexaPixel: String = ""
      var hexaPixelList: List[Char] = Nil
      var redPixel: List[Char] = Nil
      var intRed: Int = 0
      redPixel = image2DPattern(x)(y).toHexString.toList.takeRight(6).take(2)
      intRed = (16 * Integer.parseInt(redPixel.head.toString, 16)) + (Integer.parseInt(redPixel.tail.head.toString, 16))
      hM(intRed) += 1
    }
    return hM
  }
  //permet de créer l'histogramme des valeurs vertes du motif
  def histoMotifG(): Array[Int] = {
    var hM: Array[Int] = Array.fill(256)(0)
    for (x <- 0 to wrappedImagePattern.height - 1; y <- 0 to wrappedImagePattern.width - 1) {
      var hexaPixel: String = ""
      var hexaPixelList: List[Char] = Nil
      var greenPixel: List[Char] = Nil
      var intGreen: Int = 0
      greenPixel = image2DPattern(x)(y).toHexString.toList.takeRight(4).take(2)
      intGreen = (16 * Integer.parseInt(greenPixel.head.toString, 16)) + (Integer.parseInt(greenPixel.tail.head.toString, 16))
      hM(intGreen) += 1
    }
    return hM
  }
  //permet de créer l'histogramme des valeurs bleues du motif
  def histoMotifB(): Array[Int] = {
    var hM: Array[Int] = Array.fill(256)(0)
    for (x <- 0 to wrappedImagePattern.height - 1; y <- 0 to wrappedImagePattern.width - 1) {
      var hexaPixel: String = ""
      var hexaPixelList: List[Char] = Nil
      var bluePixel: List[Char] = Nil
      var intBlue: Int = 0
      bluePixel = image2DPattern(x)(y).toHexString.toList.takeRight(2)
      intBlue = (16 * Integer.parseInt(bluePixel.head.toString, 16)) + (Integer.parseInt(bluePixel.tail.head.toString, 16))
      hM(intBlue) += 1
    }
    return hM
  }
  //permet de rechercher le motif dans l'image
  def chercheMotif() {
    var tstR = creaHistoIntR()
    var tstG = creaHistoIntG()
    var tstB = creaHistoIntB()
    println("ok")
    var motCurrR: Array[Int] = tstR(wrappedImagePattern.height)(wrappedImagePattern.width).clone()
    var motCurrG: Array[Int] = tstG(wrappedImagePattern.height)(wrappedImagePattern.width).clone()
    var motCurrB: Array[Int] = tstB(wrappedImagePattern.height)(wrappedImagePattern.width).clone()
    var t2R = histoMotifR()
    var t2G = histoMotifG()
    var t2B = histoMotifB()
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      if (x - 1 + wrappedImagePattern.height <= wrappedImageSource.height && y - 1 + wrappedImagePattern.width <= wrappedImageSource.width) {
        motCurrR = tstR(x + wrappedImagePattern.height - 1)(y + wrappedImagePattern.width - 1).clone()
        motCurrG = tstG(x + wrappedImagePattern.height - 1)(y + wrappedImagePattern.width - 1).clone()
        motCurrB = tstB(x + wrappedImagePattern.height - 1)(y + wrappedImagePattern.width - 1).clone()
        for (i <- 0 to motCurrR.length - 1) { //entre 0 et 255
          motCurrR(i) = motCurrR(i) + tstR(x - 1)(y - 1)(i)
          motCurrG(i) = motCurrG(i) + tstG(x - 1)(y - 1)(i)
          motCurrB(i) = motCurrB(i) + tstB(x - 1)(y - 1)(i)

          motCurrR(i) = motCurrR(i) - tstR(x - 1)(y + wrappedImagePattern.width - 1)(i)
          motCurrG(i) = motCurrG(i) - tstG(x - 1)(y + wrappedImagePattern.width - 1)(i)
          motCurrB(i) = motCurrB(i) - tstB(x - 1)(y + wrappedImagePattern.width - 1)(i)

          motCurrR(i) = motCurrR(i) - tstR(x + wrappedImagePattern.height - 1)(y - 1)(i)
          motCurrG(i) = motCurrG(i) - tstG(x + wrappedImagePattern.height - 1)(y - 1)(i)
          motCurrB(i) = motCurrB(i) - tstB(x + wrappedImagePattern.height - 1)(y - 1)(i)
        }
        if (compare(motCurrR, t2R) && compare(motCurrG, t2G) && compare(motCurrB, t2B)) {
          encadre(x - 1, y - 1)
        }

      }
    }

    wrappedImageSource.saveImage(dst)
  }

}