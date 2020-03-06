package transfo

//importation de l'API pour pouvoir l'utilisée dans le programme
import com.tncy.top.image.ImageWrapper;

class gris(var src: String, var dst: String) {
  //construction du tableau de pixel de l'image grâce à l'API
  var fileName: String = src
  var outputFile: String = dst
  var wrappedImage: ImageWrapper = new ImageWrapper(fileName);
  var image2D: Array[Array[Int]] = wrappedImage.getImage();

  //transforme un entier en tableau d'hexa
  def tabHexa(hexa: Int): Array[Char] = {
    var string = hexa.toHexString
    var arrayhexa = string.toArray
    return arrayhexa
  }

  //transforme une list de binaire en un entier
  def binToInt(L: List[Char]): Int = {
    var binaireString = L.mkString
    return Integer.parseInt(binaireString, 2)
  }

  //renvoie la valeur du code d'un pixel définie suivant la valeur de gris
  def codePixel(valGris: Int): Int = {
    var binGris = valGris.toBinaryString //convertit l'entier en une phrase binaire
    var listBinGris = binGris.toList //convertit la phrase en liste
    var taille = listBinGris.length
    for (i <- taille to 7) { //met en forme pour avoir un octet
      listBinGris ::= '0'
    }
    //met en forme pour avoir les 3 octets rgb identique(=gris)
    listBinGris = listBinGris ::: listBinGris ::: listBinGris   
    return binToInt(listBinGris)
  }

  //fonction qui transforme la valeur d'un pixel couleur en une valeur de pixel gris
  def transformeGris() {
    var codeHexa: Array[Char] = new Array[Char](8)
    var r: Int = 0
    var v: Int = 0
    var b: Int = 0
    var gris: Int = 0
    for (row <- 0 to (wrappedImage.height - 1)) {
      for (col <- 0 to (wrappedImage.width - 1)) {
        codeHexa = tabHexa(image2D(row)(col))
        r = (16 * Integer.parseInt(codeHexa(2).toString, 16)) + Integer.parseInt(codeHexa(3).toString, 16)
        v = (16 * Integer.parseInt(codeHexa(4).toString, 16)) + Integer.parseInt(codeHexa(5).toString, 16)
        b = (16 * Integer.parseInt(codeHexa(6).toString, 16)) + Integer.parseInt(codeHexa(7).toString, 16)
        /*donne la valeur de gris en entier en utilisant les coefficient 
        *utilisé par la recommandation 709(couleurs naturels)*/
        gris = (0.299 * r + 0.587 * v + 0.114 * b).intValue()
        image2D(row)(col) = codePixel(gris)
      }
    }
    wrappedImage.saveImage(outputFile)
  }

}