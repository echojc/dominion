package sh.echo.dominion.model.cards.special

import sh.echo.dominion.model.Game
import sh.echo.dominion.model.cards._

object Copper extends Treasure("Copper", 0, 1) {
  override val count = 60 - (Game.players.size * 7)
}

object Silver extends Treasure("Silver", 3, 2) {
  override val count = 40
}

object Gold extends Treasure("Gold", 6, 3) {
  override val count = 30
}

object Estate extends Victory("Estate", 2) {
  override val value = 1
}

object Duchy extends Victory("Duchy", 5) {
  override val value = 3
}

object Province extends Victory("Province", 8) {
  override val value = 6
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else if (playerCount == 3) 12
    else playerCount * 3
  }
}

object Curse extends Curse("Curse", 0, -1)
