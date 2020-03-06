import transfo.gris
import recherche.histoGris
import recherche.HistoInt
import recherche.histoCoul
import recherche.HistoIntCoul
import recherche.searchGrey

object main extends App {
  var value: Int = 0
  var choix: Int = 0
  var coul: Boolean = false
  while (value == 0) {
    println("Choix :")
    println("\n" + "Que voulez faire ? \n" + "A) Passer une image couleur vers une image en noir et blanc\n" + "B) Faire une recherche d'un motif dans une image\n" + "C) Effectuer test\n")
    var reponse = scala.io.StdIn.readLine()

    reponse match {
      case "A"  => value = 1
      case "B"  => value = 2
      case "C"  => value = 3
      case whoa => value = 0
    }
  }

  println("Veillez rentrer le chemin de l'image source : \n")
  var chSource = scala.io.StdIn.readLine()
  println("Veillez rentrer le chemin de l'image transformée : \n")
  var chDest = scala.io.StdIn.readLine();

  if (value == 1) {
    var tgris = new gris(chSource, chDest);
    tgris.transformeGris()
    println("la transformation en gris à réussi, l'image est ici : " + chDest)
  } else if (value == 2) {
    println("Veillez rentrer le chemin de l'image motif : \n")
    var chMotif = scala.io.StdIn.readLine();
    while (choix == 0) {
      println("Que voulez vous faire ? \n" + "A) Recherche Pixel par Pixel \n" + "B) Recherche par Histogramme Comparé\n" + "C) Recherche par Histogramme Intégral\n")
      var ch = scala.io.StdIn.readLine();

      ch match {
        case "A"  => choix = 1
        case "B"  => choix = 2
        case "C"  => choix = 3
        case whoa => choix = 0
      }
      if (choix == 1) {
        var sG = new searchGrey(chSource, chMotif, chDest)
        sG.motifInSource()
        println("la recherche à réussi, l'image est ici : " + chDest)
      } else if (choix == 2) {
        println("Que voulez vous faire ? \n" + "A) Motif et image en noir et blanc \n" + "B) Motif et image en couleur\n" + "(par défaut recherche faite en noir et blanc)\n")
        var bw = scala.io.StdIn.readLine();
        bw match {
          case "A"  => coul = false
          case "B"  => coul = true
          case whoa => coul = false
        }

        if (!coul) {
          var histoGris = new histoGris(chSource, chMotif, chDest)
          histoGris.rechHisto()
          println("la recherche à réussi, l'image est ici : " + chDest)
        } else {
          var histoCoul = new histoCoul(chSource, chMotif, chDest)
          histoCoul.rechHisto()
          println("la recherche à réussi, l'image est ici : " + chDest)
        }
      } else {
        println("Que voulez vous faire ? \n" + "A) Motif et image en noir et blanc \n" + "B) Motif et image en couleur\n" + "(par défaut recherche faite en noir et blanc)\n")
        var bw = scala.io.StdIn.readLine();
        bw match {
          case "A"  => coul = false
          case "B"  => coul = true
          case whoa => coul = false
        }

        if (!coul) {
          var hI = new HistoInt(chSource, chMotif, chDest)
          hI.chercheMotif()
          println("la recherche à réussi, l'image est ici : " + chDest)
        } else {
          var hIC = new HistoIntCoul(chSource, chMotif, chDest)
          hIC.chercheMotif()
          println("la recherche à réussi, l'image est ici : " + chDest)
        }
      }
    }
  } else {
  var nmotgr="./mtgr.png"
    println("Veillez rentrer le chemin de l'image motif (en couleur) : \n")
    var chMotif = scala.io.StdIn.readLine();

    var tgris = new gris(chSource, "./sg.png");
    tgris.transformeGris()
    println("la transformation en gris à réussi, l'image est ici : " + "./sg.png")

    var mgris = new gris(chMotif, nmotgr);
    mgris.transformeGris()
    println("la transformation en gris à réussi, l'image est ici : " + nmotgr)

    var sG = new searchGrey(chSource, chMotif, "./ta.png")
    sG.motifInSource()
    println("la recherche pixel par pixel à réussi, l'image est ici : " + "./ta.png")

    var histoGris = new histoGris("./sg.png", nmotgr, "./tb.png")
    histoGris.rechHisto()
    println("la recherche à réussi, l'image est ici : " + "./tb.png")

    var histoCoul = new histoCoul(chSource, chMotif, "./tc.png")
    histoCoul.rechHisto()
    println("la recherche à réussi, l'image est ici : " + "./tc.png")

    var hI = new HistoInt("./sg.png", nmotgr, "./td.png")
    hI.chercheMotif()
    println("la recherche à réussi, l'image est ici : " + "./td.png")

    var hIC = new HistoIntCoul(chSource, chMotif, "./ta.png")
    hIC.chercheMotif()
    println("la recherche à réussi, l'image est ici : " + "./te.png")
  }
}