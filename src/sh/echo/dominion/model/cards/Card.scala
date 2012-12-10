package sh.echo.dominion.model.cards

import sh.echo.dominion.model.Game

abstract class Card() {
  val get: Card = this
  
  def play() {}
  val isPlayable = false
  
  val count: Int
  val name: String
  val cost: Int
  val description: String = ""
}

abstract class Action(val name: String, val cost: Int, override val description: String) extends Card {
  override val isPlayable = true
  override val count = 10
}
trait Attack {
  def attack()
}
trait Reaction {
  def react(card: Card with Attack)
}

abstract class Treasure(val name: String, val cost: Int, val value: Int) extends Card {
  override val isPlayable = true
  override def play {
    Game.treasureCount += value
  }
}

abstract class Victory(val name: String, val cost: Int) extends Card {
  def value: Int
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else 12
  }
}

abstract class Curse(val name: String, val cost: Int, val value: Int) extends Card {
  override val count = (Game.players.size - 1) * 10
}
