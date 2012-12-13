package sh.echo.dominion.model.cards.special

import sh.echo.dominion.model.Game
import sh.echo.dominion.model.cards._

object Copper extends Card("Copper", 0) with Treasure {
  override val value = 1
  override val count = 60 - (Game.players.size * 7)
}

object Silver extends Card("Silver", 3) with Treasure {
  override val value = 2
  override val count = 40
}

object Gold extends Card("Gold", 6) with Treasure {
  override val value = 3
  override val count = 30
}

object Estate extends Card("Estate", 2) with Victory {
  override val value = 1
}

object Duchy extends Card("Duchy", 5) with Victory {
  override val value = 3
}

object Province extends Card("Province", 8) with Victory {
  override val value = 6
  override val count = {
    val playerCount = Game.players.size
    if (playerCount == 2) 8
    else if (playerCount == 3) 12
    else playerCount * 3
  }
}

object Curse extends Card("Curse", 0) with Curse {
  override val value = -1
}
