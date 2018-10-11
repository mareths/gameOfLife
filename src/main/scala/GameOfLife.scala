object GameOfLife {
  def convertirCompte(ligneComplet: String, ligneCompte: String, pos: Option[Int] = Some(0)): String = {
    if (pos.get >= ligneCompte.length) ""
    else {
      var cell: String = {
        if (ligneComplet.charAt(pos.get).equals(".")) {
          ligneCompte.charAt(pos.get) match {
            case '3' => "*"
            case _ => "."
          }
        } else {
          ligneCompte.charAt(pos.get) match {
            case '2' => "*"
            case '3' => "*"
            case _ => "."
          }
        }
      }
      cell.concat(convertirCompte(ligneComplet, ligneCompte, Some(pos.get + 1)))
    }
  }

  def compteLesCellulesVivantesVoisines(ligneComplet: String, pos: Option[Int] = Some(0)): String = {
    if (pos.get >= ligneComplet.length) ""
    else {
      var count: Int =
        pos.get match {
          case p if p > 0 & p <= ligneComplet.length  =>
            ligneComplet.charAt(p-1) match {
              case '.' => 0
              case '*' => 1
              case _ => 0
            }
          case _ => 0
        }
        count += {
          pos.get match {
            case p if p >= 0 & p < ligneComplet.length -1 =>
              ligneComplet.charAt(p+1) match {
                case '.' => 0
                case '*' => 1
                case _ => 0
              }
            case _ => 0
          }
        }
      count.toString.concat(compteLesCellulesVivantesVoisines(ligneComplet, Some(pos.get + 1)))
    }
  }


  def compteLesCellulesVivantes(entree: String): String = {
    if (entree.isEmpty) ""
    else {
      val count: String =
        entree.head match {
          case '.' => "0"
          case '*' => "1"
          case _ => ""
        }
      count.concat(compteLesCellulesVivantes(entree.drop(1)))
    }
  }

  def afficheCaractereSuivant(entree: String, pos: Some[Int]): Char = {
    pos.get match {
      case p if p >= 0 & p < entree.length => afficheCaractere(entree, Some(pos.get + 1))
      case p if p < entree.length + 1 => '.'
      case _ => '.'
    }
  }

  def afficheCaracterePrecedent(entree: String, pos: Some[Int]): Char = {
    pos.get match {
      case p if p > 1 & p <= entree.length + 1 => afficheCaractere(entree, Some(pos.get - 1))
      case p if p < entree.length + 1 => '.'
      case _ => '.'
    }
  }

  def afficheCaractere(entree: String, pos: Option[Int] = Some(1)): Char = {
    pos.get match {
      case p if p > 0 & p <= entree.length => entree.charAt(p - 1)
      case p if p < entree.length => '.'
      case _ => '.'
    }
  }

  def affiche(chaineAAfficher: String): String = {
    chaineAAfficher
  }

}
