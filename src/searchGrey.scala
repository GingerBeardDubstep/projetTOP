package recherche
import com.tncy.top.image.ImageWrapper;

class searchGrey(var src: String, var mot: String, var dst: String) {

  var sourceFileName: String = src; /*Nom du fichier image*/
  var patternFileName: String = mot; /*Nom du fichier motif*/
  var wrappedImageSource: ImageWrapper = new ImageWrapper(sourceFileName); /*Charger le fichier image source*/
  var wrappedImagePattern: ImageWrapper = new ImageWrapper(patternFileName) /*Charger le fichier image motif*/
  var image2DSource: Array[Array[Int]] = wrappedImageSource.getImage(); /*Tableau 2 dimensions repr�sentant la source*/
  var image2DPattern: Array[Array[Int]] = wrappedImagePattern.getImage(); /*Tableau 2 dimensions repr�sentant le motif*/
  var hSource: Int = wrappedImageSource.height /*Variable de hauteur de la source*/
  var lSource: Int = wrappedImageSource.width /*Variable de largeur de la source*/
  var hPattern: Int = wrappedImagePattern.height /*Variable de hauteur du motif*/
  var lPattern: Int = wrappedImagePattern.width /*Variable de largeur du motif*/
  var nbPixelPattern: Int = (hPattern - 1) * (lPattern - 1) /*Nombre de pixels du motif*/

  var firstMatchLigne: Int = 0 /*Ligne du premier pixel � match*/
  var firstMatchCol: Int = 0 /*Colone du 1er pixel � match*/
  var lignePat: Int = 0 /*compteur de ligne du pattern*/
  var colonnePat: Int = 1 /*compteur de colonne du pattern*/

  def recherchePixel(ligneS: Int, colS: Int, ligneP: Int, colP: Int): Boolean = { /*retourne true si les pixels sont �gaux*/
    return (image2DSource(ligneS)(colS) == image2DPattern(ligneP)(colP))
  }

  def motifInSource() {
    def aux(cmptL: Int, cmptC: Int, cmptM: Int) {
      var ligne: Int = cmptL
      var colonne: Int = cmptC /*copie des compteurs pour le cas ou on change de ligne : on r�affecte les var*/

      if (cmptM == nbPixelPattern) { /*Cas ou le pattern a �t� d�tect� en entier dans la source*/
        for (i <- 0 to lPattern - 1) {
          image2DSource(firstMatchLigne)(firstMatchCol + i) = Integer.parseInt("FF0000", 16)
          image2DSource(firstMatchLigne + hPattern - 1)(firstMatchCol + i) = Integer.parseInt("FF0000", 16)
        }
        for (j <- 0 to hPattern - 1) {
          image2DSource(firstMatchLigne + j)(firstMatchCol) = Integer.parseInt("FF0000", 16)
          image2DSource(firstMatchLigne + j)(firstMatchCol + lPattern - 1) = Integer.parseInt("FF0000", 16) /*on colorie en rouge*/
        } /*le contour du pattern*/
        lignePat = 0
        colonnePat = 1
        aux(firstMatchLigne, firstMatchCol + 1, 0)
      } else if (!(ligne > (hSource - hPattern) & cmptM == 0)) { /*Cas d'arret*/
        /*R�cursion*/
        if (cmptM == 0) { /*algo si on n a pas encore trouv� le 1er pixel de patern*/
          if (colonne > (lSource - lPattern)) {
            ligne += 1
            colonne = 0 /*changement de variables si on depasse les colones*/
          }
          if (recherchePixel(ligne, colonne, 0, 0)) { /*Si le 1 er pixel est trouv�*/
            firstMatchLigne = ligne
            firstMatchCol = colonne /*on sauvegarde la position du pixel*/
            aux(ligne, colonne + 1, 1) /*recherche au pixel suivant avec cmptM=1*/
          } else {
            aux(ligne, colonne + 1, 0) /*recherche au pixel suivant avec cmptM=0*/
          }
        } else { /*Si on a deja trouv� un ou plus pixels d'affil�e dans l image*/
          if (colonne >= (firstMatchCol + lPattern)) {
            ligne += 1
            colonne = firstMatchCol /*changement de variables si on depasse les colones*/
          }
          if (!recherchePixel(ligne, colonne, lignePat, colonnePat)) {
            lignePat = 0
            colonnePat = 1
            aux(firstMatchLigne, firstMatchCol + 1, 0)
          } else {
            colonnePat += 1
            if (colonnePat >= lPattern) {
              colonnePat = 0
              lignePat += 1
            }
            aux(ligne, colonne + 1, cmptM + 1)
          }
        }
      }
    }
    aux(0, 0, 0)

    wrappedImageSource.saveImage(dst)

  }

}