package recherche
import com.tncy.top.image.ImageWrapper;

class HistoInt(var src: String, var mot: String, var dst: String) {
  var sourceFileName: String = src; /*Nom du fichier image*/

  var patternFileName: String = mot; /*Nom du fichier motif*/

  var wrappedImageSource: ImageWrapper = new ImageWrapper(sourceFileName); /*Charger le fichier image source*/

  var wrappedImagePattern: ImageWrapper = new ImageWrapper(patternFileName) /*Charger le fichier image motif*/

  var image2DSource: Array[Array[Int]] = wrappedImageSource.getImage(); /*Tableau 2 dimensions, chaque case est un pixel de la source*/

  var image2DPattern: Array[Array[Int]] = wrappedImagePattern.getImage(); /*Tableau 2 dimensions, chaque case est un pixel du motif*/

  var hI: Array[Array[Array[Int]]] = Array.fill(wrappedImageSource.height + 1, wrappedImageSource.width + 1, 256)(0)

  var hM: Array[Int] = Array.fill(256)(0)

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
  //permet de créer l'histogramme intégrale de la source
  def creaHistoInt(): Array[Array[Array[Int]]] = {
    hI = Array.fill(wrappedImageSource.height + 1, wrappedImageSource.width + 1, 256)(0)
    var hexaPixel: String = ""
    var hexaPixelList: List[Char] = Nil
    var greyPixel: List[Char] = Nil
    var intGris: Int = 0
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      hexaPixel = image2DSource(x - 1)(y - 1).toHexString
      hexaPixelList = hexaPixel.toList
      greyPixel = hexaPixelList.takeRight(2)
      intGris = (16 * Integer.parseInt(greyPixel.head.toString, 16)) + (Integer.parseInt(greyPixel.tail.head.toString, 16))
      for (i <- x to wrappedImageSource.height; j <- y to wrappedImageSource.width) {
        hI(i)(j)(intGris) += 1
      }
    }
    return hI
  }
//permet de créer l'histogramme du motif
  def histoMotif(): Array[Int] = {
    for (x <- 0 to wrappedImagePattern.height - 1; y <- 0 to wrappedImagePattern.width - 1) {
      var hexaPixel: String = ""
      var hexaPixelList: List[Char] = Nil
      var greyPixel: List[Char] = Nil
      var intGris: Int = 0
      hexaPixel = image2DPattern(x)(y).toHexString
      hexaPixelList = hexaPixel.toList
      greyPixel = hexaPixelList.takeRight(2)
      intGris = (16 * Integer.parseInt(greyPixel.head.toString, 16)) + (Integer.parseInt(greyPixel.tail.head.toString, 16))
      hM(intGris) += 1
    }
    return hM
  }
 //permet de rechercher le motif dans l'image
  def chercheMotif() {
    var tst = creaHistoInt()
    println("ok")
    var motCurr: Array[Int] = tst(wrappedImagePattern.height)(wrappedImagePattern.width).clone()
    var t2 = histoMotif()
    for (x <- 1 to wrappedImageSource.height; y <- 1 to wrappedImageSource.width) {
      if (x - 1 + wrappedImagePattern.height <= wrappedImageSource.height && y - 1 + wrappedImagePattern.width <= wrappedImageSource.width) {
        motCurr = tst(x + wrappedImagePattern.height - 1)(y + wrappedImagePattern.width - 1).clone()
        for (i <- 0 to motCurr.length - 1) { //entre 0 et 255
          motCurr(i) = motCurr(i) + tst(x - 1)(y - 1)(i)

          motCurr(i) = motCurr(i) - tst(x - 1)(y + wrappedImagePattern.width - 1)(i)

          motCurr(i) = motCurr(i) - tst(x + wrappedImagePattern.height - 1)(y - 1)(i)

        }
        if (compare(motCurr, t2)) {
          encadre(x - 1, y - 1)
        }

      }
    }

    wrappedImageSource.saveImage(dst)
  }

}