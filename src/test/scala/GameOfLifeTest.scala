import org.scalatest.FlatSpec


class GameOfLifeTest extends FlatSpec {
  "Quand on donne en entrée une chaine de caractère" should
  "retourne la chaine de caractere" in {
    val chaineAAfficher: String = "Salut"
    assert(GameOfLife.affiche(chaineAAfficher).equals(chaineAAfficher.toString))
  }

  "Quand on donne en entrée une chaine de caractère" should
  "retourne la premiere lettre de la chaine de caractere" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree).equals('S'))
  }

  "Quand on donne en entrée une chaine de caractère" should
    "retourne la lettre de la chaine de caractere à la position demandé" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree, Some(3)).equals('l'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 0" should
    "retourne le caractère '.'" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree, Some(0)).equals('.'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 1" should
    "retourne le premier caractère" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree, Some(1)).equals('S'))
  }

  "Quand on donne en entrée une chaine de caractère et une position égale à la longueur de la chaine de caractère" should
    "retourne le dernier caractère" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree, Some(5)).equals('t'))
  }

  "Quand on donne en entrée une chaine de caractère et une position superieure à la longueur de la chaine de caractère" should
    "retourne le caractère '.'" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractere(entree, Some(6)).equals('.'))
  }

}
