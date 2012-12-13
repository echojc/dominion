package sh.echo.dominion.model.cards

import sh.echo.dominion.model.Game

abstract class Card() extends Comparable[Card] {
  val get: Card = this
  
  def play() {}
  val isPlayable = false
  
  val count: Int
  val name: String
  val cost: Int
  val description: String = ""
  val sortPriority: Int
    
  override def toString = name
  override def compareTo(other: Card) = {
    if (sortPriority != other.sortPriority) sortPriority - other.sortPriority
    else if (cost != other.cost) cost - other.cost
    else name.compareTo(other.name)
  }
  
  protected val treasureSortPriority = 0
  protected val victorySortPriority = 1
  protected val curseSortPriority = 2
  protected val actionSortPriority = 4
}

abstract class Action(val name: String, val cost: Int, override val description: String) extends Card {
  override val isPlayable = true
  override val count = 10
  override val sortPriority = actionSortPriority
}
trait Attack {
  def attack()
}
trait Reaction {
  def react(card: Card with Attack)
}

abstract class Treasure(val name: String, val cost: Int, val value: Int) extends Card {
  override val isPlayable = true
  override val sortPriority = treasureSortPriority
  override def play {
    Game.updateTreasure(value)
  }
}

abstract class Victory(val name: String, val cost: Int) extends Card {
  def value: Int
  override val sortPriority = victorySortPriority
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else 12
  }
}

abstract class Curse(val name: String, val cost: Int, val value: Int) extends Card {
  override val count = (Game.players.size - 1) * 10
  override val sortPriority = curseSortPriority
}
