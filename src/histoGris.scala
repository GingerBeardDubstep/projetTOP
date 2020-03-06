package recherche

//importation de l'API pour pouvoir l'utilisée dans le programme
import com.tncy.top.image.ImageWrapper;

class histoGris(var src: String, var mot: String, var dst: String) {

  /*construction du tableau de pixel de l'image et du motif ainsi que leur
  *caractéristique (hauteur et largeur) grâce à l'API*/
  var sourceFileName: String = src;
  var patternFileName: String = mot;
  var ResulatFileName: String = dst;
  var sourceWrappedImage: ImageWrapper = new ImageWrapper(sourceFileName);
  var patternWrappedImage: ImageWrapper = new ImageWrapper(patternFileName);
  var imageSource: Array[Array[Int]] = sourceWrappedImage.getImage();
  var imagePattern: Array[Array[Int]] = patternWrappedImage.getImage();
  var hSource: Int = sourceWrappedImage.height /*Variable de hauteur de la source*/
  var lSource: Int = sourceWrappedImage.width /*Variable de largeur de la source*/
  var hPattern: Int = patternWrappedImage.height /*Variable de hauteur du motif*/
  var lPattern: Int = patternWrappedImage.width /*Variable de largeur du motif*/

  /*modifie le tableau de l'image source afin de réaliser plus tard une
  *image comportant les résultats trouvés entouré de rouge*/
  def entoure(ligne: Int, col: Int) {
    for (i <- 0 to lPattern - 1) {
      imageSource(ligne)(col + i) = Integer.parseInt("FF0000", 16)
      imageSource(ligne + hPattern - 1)(col + i) = Integer.parseInt("FF0000", 16)
    }
    for (j <- 0 to hPattern - 1) {
      imageSource(ligne + j)(col) = Integer.parseInt("FF0000", 16)
      imageSource(ligne + j)(col + lPattern - 1) = Integer.parseInt("FF0000", 16)
    }
  }
  
  //créer l'histogramme, des valeurs de gris, du tableau de pixel en entré entre certaines coordonnées
  def histGris(tab: Array[Array[Int]], lDepart: Int, lFin: Int, cDepart: Int, cFin: Int): Array[Int] = {
    var histogramme: Array[Int] = Array.fill(256)(0)
    var greyPixel: List[Char] = Nil
    var intGris: Int = 0
    for (row <- lDepart to lFin; col <- cDepart to cFin) {
      greyPixel = tab(row)(col).toHexString.toList.takeRight(2)
      intGris = (16 * Integer.parseInt(greyPixel.head.toString, 16)) + (Integer.parseInt(greyPixel.tail.head.toString, 16))
      histogramme(intGris) += 1
    }
    return histogramme
  }

  //initialisation des variables utilisé par rechHisto()
  var histoSource: Array[Int] = histGris(imageSource, 0, hSource - 1, 0, lSource - 1)
  var histoPattern: Array[Int] = histGris(imagePattern, 0, hPattern - 1, 0, lPattern - 1)
  var histoTest: Array[Int] = Array.fill(256)(0)
  var ligne: Int = 0
  var col: Int = 0

  /*fonction récursive recherchant le motif dans l'image par histogrammes comparatifs 
   * (le résultat de la recherche sera visible grâce à la création de l'image de résultat)*/
  def rechHisto() {
    //initialisation des variables locales
    var test1: Boolean = true
    var test2: Boolean = true
    var mem: Int = 0
    var nbOccurMax: Int = 100000

    def aux(l: Int, c: Int) {
      ligne = l
      col = c
      if (col > lSource - lPattern) {
        ligne += 1
        col = 0
      }
      if (ligne <= hSource - hPattern) {
        test2 = true
        histoTest = histGris(imageSource, ligne, ligne + hPattern - 1, col, col + lPattern - 1)
        for (n <- 0 to 255) {
          if (histoTest(n) != histoPattern(n)) {
            test2 = false
          }
        }
        if (test2) {
          println("une occurence trouvée")
          nbOccurMax -= 1
          entoure(ligne, col)
        }
        if (nbOccurMax > 0) {
          aux(ligne, col + 1)
        }
      }
    }
    /*test préalable à l'exécution de la recherche qui vérifie que l'histogramme 
    *du motif peut se trouver dans celui de l'image*/
    for (n <- 0 to 255) {
      if (histoPattern(n) > histoSource(n)) {
        test1 = false
      } else if (histoPattern(n) != 0) {
        mem = histoSource(n) / histoPattern(n)
        if (mem < nbOccurMax) {
          nbOccurMax = mem
        }
      }
    }
    if (test1) {
      println("Il y a " + nbOccurMax + " occurence possible au maximum dans l'image")
      aux(0, 0)
    }
    //enregistrement du résultat
    println("exécution terminer")
    sourceWrappedImage.saveImage(ResulatFileName)
  }

}