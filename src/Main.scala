object Main {

  // -----------Zad 1---------------
  val dniTygodnia: List[String] = List("Poniedziałek","Wtorek","Środa","Czwartek","Piątek","Sobota","Niedziela")
  def dopasujDniTygodnia(str: String): String = str match {
    case "Poniedziałek" => "Praca"
    case "Wtorek" => "Praca"
    case "Środa" => "Praca"
    case "Czwartek" => "Praca"
    case "Piątek" => "Praca"
    case "Sobota" => "Weekend"
    case "Niedziela" => "Weekend"
    case _ => "<angry computing noises>"
  }
  // -------------------------------

  // -----------Zad 2---------------
  class KontoBankowe() {
    //Konstruktor ustawiający stan konta na 0 - Wykorzystany został do tego primary constructor
    private var _stanKonta:Int = 0;
    def stanKonta = _stanKonta

    def wplata(kwota: Int) {
      if (kwota > 0){
        _stanKonta += kwota
      }
    }
    def wyplata(kwota: Int): Unit ={
      if (kwota <= stanKonta)
      {
        _stanKonta -= kwota
      }
    }
    def this(stanKonta: Int){
      this()
      this._stanKonta = stanKonta
    }
  }
  // -------------------------------

  // -----------Zad 3---------------
  case class Osoba(var imie:String, var nazwisko: String)
  {
  }

  def przywitaj (osoba: Osoba):String = osoba match {
    case Osoba("Jan", "Kowalski") => "Dżem dobry"
    case Osoba("Chojrak", "Pies") => "Głupi kundel!"
    case Osoba("Edd", "Głupi")=> "Jednobrewy!"
    case Osoba(_,_) => "<Angry computing noises>"
  }
  // -------------------------------

  // -----------Zad 4---------------
  def zastosujTrzykrotnie(fun: (Int) => Int, arg: Int): Int = {
    var wynik = fun(arg)
    wynik = fun(wynik)
    wynik = fun(wynik)
    wynik
  }
  // -------------------------------

  // -----------Zad 5---------------
  class Osoba2(){
    private var _imie = ""
    private var _nazwisko = ""
    var _podatek: Double = 0.05
    def imie = _imie
    def nazwisko = _nazwisko
    def podatek = _podatek
  }

  trait Pracownik extends Osoba2 {
    var pensja: Double = 100;
    override def podatek: Double = 0.2*pensja
  }
  trait Student extends Osoba2 {
    override def podatek: Double = 0;
  }
  trait Nauczyciel extends Pracownik {
    override def podatek: Double = 0.1 * pensja
  }
  // -------------------------------
  def main(args: Array[String]): Unit = {

    // -----Wywołanie zad 1-----
    println("Zadanie 1")
    for(i<- dniTygodnia){
      println(i + " - " +dopasujDniTygodnia(i))
    }
    println("burger - " + dopasujDniTygodnia("burger"))
    println()
    // ------------------------

    // -----Wywołanie zad 2-----
    val konto1 = new KontoBankowe()
    val konto2 = new KontoBankowe(1000)

    println("Zadanie 2")

    println("Stan początkowy pierwszego konta: "+konto1.stanKonta)
    println("Stan początkowy drugiego konta: "+konto2.stanKonta)

    println("Wpłata 100 na oba konta");
    konto1.wplata(100)
    konto2.wplata(100)

    println("Wypłata 50 z obu kont");
    konto1.wyplata(50)
    konto2.wyplata(50)

    println("Stan aktualny pierwszego konta: "+konto1.stanKonta)
    println("Stan aktualny drugiego konta: "+konto2.stanKonta)
    // ------------------------

    // -----Wywołanie zad 3-----
    val osoba1 = Osoba("Jan","Kowalski")
    val osoba2 = Osoba("Chojrak","Pies")
    val osoba3 = Osoba("Edd","Głupi")
    val osoba4 = Osoba("Andy","Anderson")

    println("Zadanie 3")
    println(przywitaj(osoba1))
    println(przywitaj(osoba2))
    println(przywitaj(osoba3))
    println(przywitaj(osoba4))
    // ------------------------

    // -----Wywołanie zad 4-----
    def inkrementacja(x: Int) = x + 1
    var wynik = zastosujTrzykrotnie(inkrementacja, 3)
    println("Zadanie 4")
    println("Trzykrotne zastosowanie funkcji f(x) = x + 1, dla x = 3: "+ wynik);
    // ------------------------

    // -----Wywołanie zad 5-----
    val osoba = new Osoba2;
    val student = new Osoba2 with Student
    val pracownik = new Osoba2 with Pracownik
    val nauczyciel = new Osoba2 with Nauczyciel

    val pracownik_nauczyciel = new Osoba2 with Pracownik with Nauczyciel
    val nauczyciel_pracownik = new Osoba2 with Nauczyciel with Pracownik

    println("Zadanie 5")

    println("Osoba - podatek: "+ osoba.podatek)
    println("student - podatek: "+ student.podatek)
    println("pracownik - podatek: "+ pracownik.podatek)
    println("nauczyciel - podatek: "+ nauczyciel.podatek)
    println("pracownik-nauczyciel - podatek: "+ pracownik_nauczyciel.podatek)
    println("nauczyciel-pracownik - podatek: "+ nauczyciel_pracownik.podatek)
    // Wygląda na to, że jeżeli trait Nauczyciel dziedziczy z traita Pracownik,
    // to kolejność dodawania tych traitów przy tworzeniu obiektu nie ma znaczenia.
    // ------------------------
  }
}

