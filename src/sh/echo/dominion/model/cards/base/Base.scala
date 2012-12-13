package sh.echo.dominion.model.cards.base

import sh.echo.dominion.model.Game
import sh.echo.dominion.model.Game._
import sh.echo.dominion.model.cards._
import sh.echo.dominion.model.cards.special._
import sh.echo.dominion.model.cards.special.Silver
import sh.echo.dominion.model.cards.special.Curse
import sh.echo.dominion.model.cards.special.Copper
import sh.echo.dominion.model.Player

object Adventurer extends Card("Adventurer", 6) with Action {
  override val description = "Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards into your hand and discard the other revealed cards."
  override def play() {
    val cardsToTake = 2 min (currentPlayer.deck ++ currentPlayer.discard).count { _.isInstanceOf[Treasure] }
    
    while (revealedCards.count(_.isInstanceOf[Treasure]) < cardsToTake) {
      val card = currentPlayer.takeFromDeck(1)(0)
      reveal(card)
    }
    currentPlayer.addToHand(revealedCards.filter(_.isInstanceOf[Treasure]))
    currentPlayer.addToDiscard(revealedCards.filterNot(_.isInstanceOf[Treasure]))
    revealClear();
  }
}

object Bureaucrat extends Card("Bureaucrat", 4) with Action with Attack {
  override val description = "Gain a Silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards)."
  override def play() {
    gain(Silver, Player.PILE_DECK)
  }
  def attack() {
    attackPlayers(_ => {
      if (currentPlayer.hand.exists(_.isInstanceOf[Victory])) {
        val card = selectFromHand(_.isInstanceOf[Victory], 1, true)
        reveal(card)
        currentPlayer.addToDeck(card)
        revealClear()
      } else {
        reveal(currentPlayer.hand)
        revealClear()
      }
    })
  }
}

object Cellar extends Card("Cellar", 2) with Action {
  override val description = "+1 Action. Discard any number of cards. +1 Card per card discarded."
  override def play() {
    updateAction(1)
    val cards = selectFromHand()
    currentPlayer.addToDiscard(cards)
    currentPlayer.drawToHand(cards.size)
  }
}

object Chancellor extends Card("Chancellor", 3) with Action {
  override val description = "+$2. You may immediately put your deck into your discard pile."
  override def play() {
    updateTreasure(2)
    if (ask("Put deck into discard pile?")) {
      val deck = currentPlayer.takeFromDeck(currentPlayer.deck.size)
      currentPlayer.addToDiscard(deck)
    }
  }
}

object Chapel extends Card("Chapel", 2) with Action {
  override val description = "Trash up to 4 cards from your hand."
  override def play() {
    val cards = selectFromHand(max = 4)
    trash(cards)
  }
}

object CouncilRoom extends Card("Council Room", 5) with Action {
  override val description = "+4 Cards, +1 Buy. Each other player draws a card."
  override def play() {
    currentPlayer.drawToHand(4)
    updateBuy(1)
    cyclePlayers(_ => {
      currentPlayer.drawToHand(1)
    })
  }
}

object Feast extends Card("Feast", 4) with Action {
  override val description = "Trash this card. Gain a card costing up to $5."
  override def play() {
    if (playedCards.head == Feast) {
      playedCards = playedCards.tail
      trash(Feast)
    }
    selectAndGainFromSupply(_.cost <= 5)
  }
}

object Festival extends Card("Festival", 5) with Action {
  override val description = "+2 Actions, +1 Buy, +$2."
  override def play() {
    updateAction(2)
    updateBuy(1)
    updateTreasure(2)
  }
}

object Gardens extends Card("Gardens", 4) with Victory {
  override val description = "Worth 1vp for every 10 cards in your deck (rounded down)."
  override def value = Game.currentPlayer.deck.size / 10
  override val sortPriority = Card.kingdomSortPriority
}

object Laboratory extends Card("Laboratory", 5) with Action {
  override val description = "+2 Cards, +1 Action."
  override def play() {
    currentPlayer.drawToHand(2)
    updateAction(1)
  }
}

object Library extends Card("Library", 5) with Action {
  override val description = "Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing."
  override def play() {
    while (currentPlayer.hand.size < 7 && (currentPlayer.deck.size + currentPlayer.discard.size) > 0) {
      val card = currentPlayer.takeFromDeck(1)(0)
      val setAsideCard = card.isInstanceOf[Action] && ask("Set this aside?")
      if (setAsideCard) reveal(card)
      else currentPlayer.addToHand(List(card))
    }
    currentPlayer.addToDiscard(revealedCards)
    revealClear()
  }
}

object Market extends Card("Market", 5) with Action {
  override val description = "+1 Card, +1 Action, +1 Buy, +$1."
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(1)
    updateBuy(1)
    updateTreasure(1)
  }
}

object Militia extends Card("Militia", 4) with Action with Attack {
  override val description = "+$2. Each other player discards down to 3 cards in his hand."
  override def play() {
    updateTreasure(2)
  }
  def attack() {
    attackPlayers(_ => {
      if (currentPlayer.hand.size > 3) {
        val cards = selectFromHand(max = currentPlayer.hand.size - 3, exact = true)
        currentPlayer.addToDiscard(cards)
      }
    })
  }
}

object Mine extends Card("Mine", 5) with Action {
  override val description = "Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand."
  override def play() {
    if (currentPlayer.hand.exists(_.isInstanceOf[Treasure])) {
      val card = selectFromHand(_.isInstanceOf[Treasure], 1)(0)
      trash(card)
      selectAndGainFromSupply(c => c.isInstanceOf[Treasure] && c.cost <= card.cost + 3, Player.PILE_HAND)
    }
  }
}

object Moat extends Card("Moat", 2) with Action with Reaction {
  override val description = "+2 Cards. When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack."
  override def play() {
    currentPlayer.drawToHand(2)
  }
  def react(e: Reaction.Event) {
    Game.playerTemporaryImmuneToAttack = true
  }
  def canReactTo(e: Reaction.Event) = {
    e match {
      case Reaction.Attack => true
      case _ => false
    }
  }
}

object Moneylender extends Card("Moneylender", 4) with Action {
  override val description = "Trash a Copper card from your hand. If you do, +$3."
  override def play() {
    if (currentPlayer.hand.contains(Copper)) {
      selectFromHand(_ == Copper, 1, true, true)
      trash(Copper)
      updateTreasure(3)
    }
  }
}

object Remodel extends Card("Remodel", 4) with Action {
  override val description = "Trash a card from your hand. Gain a card costing up to $2 more than the trashed card."
  override def play() {
    if (currentPlayer.hand != Nil) {
      val card = selectFromHand(max = 1, exact = true)(0)
      trash(card)
      selectAndGainFromSupply(_.cost <= card.cost + 2)
    }
  }
}

object Smithy extends Card("Smithy", 4) with Action {
  override val description = "+3 Cards."
  override def play() {
    currentPlayer.drawToHand(3)
  }
}

object Spy extends Card("Spy", 4) with Action with Attack {
  override val description = "+1 Card, +1 Action. Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice."
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(1)
  }
  def attack() {
    attackPlayers(origPlayerIndex => {
      val card = currentPlayer.takeFromDeck(1)
      reveal(card)
      if (ask("Discard this?", origPlayerIndex)) {
        currentPlayer.addToDiscard(card)
      } else {
        currentPlayer.addToDeck(card)
      }
      revealClear()
    })
  }
}

object Thief extends Card("Thief", 4) with Action with Attack {
  override val description = "Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards."
  override def play() {}
  def attack() {
    var trashedCards: List[Card] = Nil
    attackPlayers(origPlayerIndex => {
      val cards = currentPlayer.takeFromDeck(2)
      reveal(cards)
      val count = revealedCards.count(_.isInstanceOf[Treasure])
      if (count > 0) {
        val card = (
          if (count == 1) revealedCards.filter(_.isInstanceOf[Treasure])
          else selectFromList(revealedCards, 1, true, players(origPlayerIndex))
        )(0)
        trashedCards ::= card
        revealedCards diff List(card)
        trash(card)
      }
      currentPlayer.addToDiscard(revealedCards)
      revealClear()
    })
    if (trashedCards != Nil) {
      val cardsToGain = selectFromList(trashedCards)
      gain(cardsToGain)
    }
  }
}

object ThroneRoom extends Card("Throne Room", 4) with Action {
  override val description = "Choose an Action card in your hand. Play it twice."
  override def play() {
    if (currentPlayer.hand.count { _.isInstanceOf[Action] } > 0) {
      val card = selectFromHand(_.isInstanceOf[Action], 1, true)(0)
      Game.play(currentPlayer.name, card)
      card.play()
    }
  }
}

object Village extends Card("Village", 3) with Action {
  override val description = "+1 Cards, +2 Action."
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(2)
  }
}

object Witch extends Card("Witch", 5) with Action with Attack {
  override val description = "+2 Cards. Each other player gains a Curse card."
  override def play() {
    currentPlayer.drawToHand(2)
  }
  def attack() {
    attackPlayers(_ => {
      gain(Curse)
    })
  }
}

object Woodcutter extends Card("Woodcutter", 3) with Action {
  override val description = "+1 Buy, +$2."
  override def play() {
    updateBuy(1)
    updateTreasure(2)
  }
}

object Workshop extends Card("Workshop", 3) with Action {
  override val description = "Gain a card costing up to $4."
  override def play() {
    selectAndGainFromSupply(_.cost <= 4)
  }
}