package sh.echo.dominion.model.cards.base

import sh.echo.dominion.model.Game
import sh.echo.dominion.model.Game._
import sh.echo.dominion.model.cards._
import sh.echo.dominion.model.cards.special._
import sh.echo.dominion.model.cards.special.Silver
import sh.echo.dominion.model.cards.special.Curse
import sh.echo.dominion.model.cards.special.Copper

object Adventurer extends Action("Adventurer", 6, "Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards into your hand and discard the other revealed cards.") {
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

object Bureaucrat extends Action("Bureaucrat", 4, "Gain a Silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).") with Attack {
  override def play() {
    gainToDeck(Silver)
  }
  def attack() {
    cyclePlayers(_ => {
      if (currentPlayer.hand.exists(_.isInstanceOf[Victory])) {
        val card = selectFromHand(_.isInstanceOf[Victory], 1, true)
        currentPlayer.addToDeck(card)
      } else {
        reveal(currentPlayer.hand)
        revealClear()
      }
    })
  }
}

object Cellar extends Action("Cellar", 2, "+1 Action. Discard any number of cards. +1 Card per card discarded.") {
  override def play() {
    updateAction(1)
    val cards = selectFromHand()
    currentPlayer.addToDiscard(cards)
    currentPlayer.drawToHand(cards.size)
  }
}

object Chancellor extends Action("Chancellor", 3, "+$2. You may immediately put your deck into your discard pile.") {
  override def play() {
    updateTreasure(2)
    if (ask("Put deck into discard pile?")) {
      val deck = currentPlayer.takeFromDeck(currentPlayer.deck.size)
      currentPlayer.addToDiscard(deck)
    }
  }
}

object Chapel extends Action("Chapel", 2, "Trash up to 4 cards from your hand.") {
  override def play() {
    val cards = selectFromHand(max = 4)
    trash(cards)
  }
}

object CouncilRoom extends Action("Council Room", 5, "+4 Cards, +1 Buy. Each other player draws a card.") {
  override def play() {
    currentPlayer.drawToHand(4)
    updateBuy(1)
    cyclePlayers(_ => {
      currentPlayer.drawToHand(1)
    })
  }
}

object Feast extends Action("Feast", 4, "Trash this card. Gain a card costing up to $5.") {
  override def play() {
    if (playedCards.head == Feast) {
      playedCards = playedCards.tail
      trash(Feast)
    }
    selectAndGainFromSupply(_.cost <= 5)
  }
}

object Festival extends Action("Festival", 5, "+2 Actions, +1 Buy, +$2.") {
  override def play() {
    updateAction(2)
    updateBuy(1)
    updateTreasure(2)
  }
}

object Gardens extends Victory("Gardens", 4) {
  override val description = "Worth 1vp for every 10 cards in your deck (rounded down)."
  override def value = Game.currentPlayer.deck.size / 10
}

object Laboratory extends Action("Laboratory", 5, "+2 Cards, +1 Action.") {
  override def play() {
    currentPlayer.drawToHand(2)
    updateAction(1)
  }
}

object Library extends Action("Library", 5, "Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing.") {
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

object Market extends Action("Market", 5, "+1 Card, +1 Action, +1 Buy, +$1.") {
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(1)
    updateBuy(1)
    updateTreasure(1)
  }
}

object Militia extends Action("Militia", 4, "+$2. Each other player discards down to 3 cards in his hand.") with Attack {
  override def play() {
    updateTreasure(2)
  }
  def attack() {
    cyclePlayers(_ => {
      if (currentPlayer.hand.size > 3) {
        val cards = selectFromHand(max = currentPlayer.hand.size - 3, exact = true)
        currentPlayer.addToDiscard(cards)
      }
    })
  }
}

object Mine extends Action("Mine", 5, "Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.") {
  override def play() {
    if (currentPlayer.hand.exists(_.isInstanceOf[Treasure])) {
      val card = selectFromHand(_.isInstanceOf[Treasure], 1)(0)
      trash(card)
      selectAndGainFromSupply(c => c.isInstanceOf[Treasure] && c.cost <= card.cost + 3)
    }
  }
}

object Moat extends Action("Moat", 2, "+2 Cards. When another player plays an Attack card, you may reveal this from your hand. If you do, you are unaffected by that Attack.") with Reaction {
  override def play() {
    currentPlayer.drawToHand(2)
  }
  def react(card: Card with Attack) {}
}

object Moneylender extends Action("Moneylender", 4, "Trash a Copper card from your hand. If you do, +$3.") {
  override def play() {
    if (currentPlayer.hand.contains(Copper)) {
      selectFromHand(_ == Copper, 1, true, true)
      trash(Copper)
      updateTreasure(3)
    }
  }
}

object Remodel extends Action("Remodel", 4, "Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.") {
  override def play() {
    if (currentPlayer.hand != Nil) {
      val card = selectFromHand(max = 1, exact = true)(0)
      trash(card)
      selectAndGainFromSupply(_.cost <= card.cost + 2)
    }
  }
}

object Smithy extends Action("Smithy", 4, "+3 Cards.") {
  override def play() {
    currentPlayer.drawToHand(3)
  }
}

object Spy extends Action("Spy", 4, "+1 Card, +1 Action. Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.") with Attack {
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(1)
  }
  def attack() {
    cyclePlayers(origPlayerIndex => {
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

object Thief extends Action("Thief", 4, "Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.") with Attack {
  override def play() {}
  def attack() {
    var trashedCards: List[Card] = Nil
    cyclePlayers(origPlayerIndex => {
      val cards = currentPlayer.takeFromDeck(2)
      reveal(cards)
      val count = revealedCards.count(_.isInstanceOf[Treasure])
      if (count > 0) {
        val card = (
          if (count == 1) revealedCards.filter(_.isInstanceOf[Treasure])
          else selectFromList(revealedCards, 1, true)
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

object ThroneRoom extends Action("Throne Room", 4, "Choose an Action card in your hand. Play it twice.") {
  override def play() {
    if (currentPlayer.hand.count { _.isInstanceOf[Action] } > 0) {
      val card = selectFromHand(_.isInstanceOf[Action], 1, true)(0)
      Game.play(currentPlayer.name, card)
      card.play()
    }
  }
}

object Village extends Action("Village", 3, "+1 Cards, +2 Action.") {
  override def play() {
    currentPlayer.drawToHand(1)
    updateAction(2)
  }
}

object Witch extends Action("Witch", 5, "+2 Cards. Each other player gains a Curse card.") with Attack {
  override def play() {
    currentPlayer.drawToHand(2)
  }
  def attack() {
    cyclePlayers(_ => {
      gain(Curse)
    })
  }
}

object Woodcutter extends Action("Woodcutter", 3, "+1 Buy, +$2.") {
  override def play() {
    updateBuy(1)
    updateTreasure(2)
  }
}

object Workshop extends Action("Workshop", 3, "Gain a card costing up to $4.") {
  override def play() {
    selectAndGainFromSupply(_.cost <= 4)
  }
}