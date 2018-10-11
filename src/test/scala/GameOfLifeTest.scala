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

  "Quand on donne en entrée une chaine de caractère et une position à 3" should
    "retourne le caractère precedent" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaracterePrecedent(entree, Some(3)).equals('a'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 3" should
    "retourne le caractère suivant" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractereSuivant(entree, Some(3)).equals('u'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 5" should
    "ne peux pas retourner le caractère suivant" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractereSuivant(entree, Some(5)).equals('.'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 1" should
    "ne peux pas retourner le caractère precedent" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaracterePrecedent(entree, Some(1)).equals('.'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 4" should
    "retourne le caractère suivant" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaractereSuivant(entree, Some(4)).equals('t'))
  }

  "Quand on donne en entrée une chaine de caractère et une position à 2" should
    "retourne le caractère precedent" in {
    val entree: String = "Salut"
    assert(GameOfLife.afficheCaracterePrecedent(entree, Some(2)).equals('S'))
  }

  "Quand on donne en entrée la chaine de caractère \"....\"" should
    "retourne la chaine de carcatere \"0000\"" in {
    val entree: String = "...."
    assert(GameOfLife.compteLesCellulesVivantes(entree).equals("0000"))
  }

  "Quand on donne en entrée la chaine de caractère \".*..\"" should
    "retourne la chaine de carcatere \"0100\"" in {
    val entree: String = ".*.."
    assert(GameOfLife.compteLesCellulesVivantes(entree).equals("0100"))
  }

  "Quand on donne en entrée la chaine de caractère \"....\"" should
    "retourne la chaine de carcatere \"0000\" qui indique qu'il n'y a aucune cellules vivantes voisines sur la ligne" in {
    val entree: String = "...."
    assert(GameOfLife.compteLesCellulesVivantesVoisines(entree).equals("0000"))
  }

  "Quand on donne en entrée la chaine de caractère \".*..\"" should
    "retourne la chaine de carcatere \"0100\" qui indique qu'à 2 endroits il y a une cellules voisine vivante" in {
    val entree: String = ".*.."
    assert(GameOfLife.compteLesCellulesVivantesVoisines(entree).equals("1010"))
  }

  "Quand on donne en entrée la chaine de caractère \"0000\" et une chaine de caractère \"....\"" should
    "traduit et retourne la chaine de carcatere \"....\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = "...."
    val ligneCompte: String = "0000"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals("...."))
  }

  "Quand on donne en entrée la chaine de caractère \"0000\" et une chaine de caractère \".*..\"" should
    "traduit et retourne la chaine de carcatere \"....\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = ".*.."
    val ligneCompte: String = "0000"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals("...."))
  }

  "Quand on donne en entrée la chaine de caractère \"0100\" et une chaine de caractère \".*..\"" should
    "traduit et retourne la chaine de carcatere \"....\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = ".*.."
    val ligneCompte: String = "0100"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals("...."))
  }

  "Quand on donne en entrée la chaine de caractère \"0200\" et une chaine de caractère \".*..\"" should
    "traduit et retourne la chaine de carcatere \".*..\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = ".*.."
    val ligneCompte: String = "0200"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals(".*.."))
  }

  "Quand on donne en entrée la chaine de caractère \"0300\" et une chaine de caractère \".*..\"" should
    "traduit et retourne la chaine de carcatere \".*..\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = ".*.."
    val ligneCompte: String = "0300"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals(".*.."))
  }

  "Quand on donne en entrée la chaine de caractère \"0400\" et une chaine de caractère \".*..\"" should
    "traduit et retourne la chaine de carcatere \"....\" qui indique qu'aucune cellule ne va revivre" in {
    val entree: String = ".*.."
    val ligneCompte: String = "0400"
    assert(GameOfLife.convertirCompte(entree, ligneCompte).equals("...."))
  }

}
