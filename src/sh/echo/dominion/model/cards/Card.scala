package sh.echo.dominion.model.cards

import sh.echo.dominion.model.Game
import sh.echo.dominion.model.View

abstract class Card(val name: String, val cost: Int) extends Comparable[Card] {
  val get: Card = this
  
  def play() {}
  val isPlayable = false
  
  val count: Int
  val description: String = ""
  val sortPriority: Int = Card.noSortPriority
    
  override def toString = name
  override def compareTo(other: Card) = {
    if (sortPriority != other.sortPriority) sortPriority - other.sortPriority
    else if (cost != other.cost) cost - other.cost
    else name.compareTo(other.name)
  }
}

object Card {
  val noSortPriority = -1
  val treasureSortPriority = 0
  val victorySortPriority = 1
  val curseSortPriority = 2
  val kingdomSortPriority = 4
}

trait Action extends Card {
  override val isPlayable = true
  override val count = 10
  override val sortPriority = Card.kingdomSortPriority
}

trait Attack extends Card {
  def attack()
}

trait Reaction extends Card {
  def react(e: Reaction.Event)
  def canReactTo(e: Reaction.Event): Boolean
}

object Reaction {
  sealed abstract class Event
  case object Attack extends Event
}

trait Treasure extends Card {
  val value: Int
  override val isPlayable = true
  override val sortPriority = Card.treasureSortPriority
  override def play {
    Game.updateTreasure(value)
  }
}

trait Victory extends Card {
  def value: Int
  override val sortPriority = Card.victorySortPriority
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else 12
  }
}

trait Curse extends Card {
  val value: Int
  override val count = (Game.players.size - 1) * 10
  override val sortPriority = Card.curseSortPriority
}
