package sh.echo.dominion.model.cards

import sh.echo.dominion.model.cards.special._
import sh.echo.dominion.model.cards.base._

object CardLists {
  def get() = this
  
  val Special = List(Copper, Silver, Gold, Estate, Duchy, Province, Curse)
  val Base = List(Adventurer, Bureaucrat, Cellar, Chancellor, Chapel,
    CouncilRoom, Feast, Festival, Gardens, Laboratory, Library, Market,
    Militia, Mine, Moat, Moneylender, Remodel, Smithy, Spy, Thief, ThroneRoom,
    Village, Witch, Woodcutter, Workshop)
    
  def fromName(name: String) = {
    val shortName = name.toLowerCase.filterNot(_ == ' ')
    Special.find(_.name.toLowerCase.filterNot(_ == ' ') == shortName) match {
      case x: Some[Card] => x.get
      case _ =>
        Base.find(_.name.toLowerCase.filterNot(_ == ' ') == shortName) match {
          case x: Some[Card] => x.get
          case _ => null
        }
    }
  }
}