package sh.echo.dominion.model.cards

import sh.echo.dominion.model.Game

object Base extends Set{
  override val all = List(Festival, Gardens)
  
  object Festival extends Action("Festival", 5, "+2 Actions, +1 Buy, +$2.") {
    override def play {
      Game.actionCount += 2
      Game.buyCount += 1
      Game.treasureCount += 2
    }
  }
  
  object Gardens extends Victory("Gardens", 4, "Worth 1vp for every 10 cards in your deck (rounded down).") {
    override def value = Game.currentPlayer.deck.size / 10
  }
}