import org.scalatest.FlatSpec


class GameOfLifeTest extends FlatSpec {
  "Quand on donne en entrée une chaine de caractère" should
  "retourne la chaine de carcatere" in {
    val chaineAAfficher: String = "Salut"
    assert(GameOfLife.affiche(chaineAAfficher).equals(chaineAAfficher.toString))
  }

}
