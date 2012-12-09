package sh.echo.dominion.model.cards

import sh.echo.dominion.model.Game

sealed abstract class Card() {
  def play {}
  val isPlayable = false
  
  val count: Int
  val name: String
  val cost: Int
  val description: String = ""
}

abstract case class Action(name: String, cost: Int, override val description: String) extends Card {
  override val isPlayable = true
  override val count = 10
}

abstract case class Treasure(name: String, cost: Int, value: Int) extends Card {
  override val isPlayable = true
  override def play {
    Game.treasureCount += value
  }
}

abstract case class Victory(name: String, cost: Int, override val description: String = "") extends Card {
  def value: Int
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else 12
  }
}

abstract case class Curse(name: String, cost: Int, value: Int) extends Card {
  override val count = (Game.players.size - 1) * 10
}

abstract class Set {
  val all: List[Card]
}