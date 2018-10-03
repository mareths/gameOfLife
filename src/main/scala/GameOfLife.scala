object GameOfLife {
  def afficheCaractere(entree: String, pos: Option[Int] = Some(1)): Char = {
    pos.get match {
      case p if p > 0 & p <= entree.length => entree.charAt(p-1)
      case p if p < entree.length => '.'
      case _ => '.'
    }
  }

  def affiche(chaineAAfficher: String): String = {
    chaineAAfficher
  }

}
