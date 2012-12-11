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
    
    while (revealedCards.count { _.isInstanceOf[Treasure] } < cardsToTake) {
      val card = currentPlayer.takeFromDeck(1)(0)
      reveal(card)
    }
    currentPlayer.addToHand(revealedCards.filter { _.isInstanceOf[Treasure] })
    currentPlayer.addToDiscard(revealedCards.filter { !_.isInstanceOf[Treasure] })
    revealedCards = Nil
  }
}

object Bureaucrat extends Action("Bureaucrat", 4, "Gain a Silver card; put it on top of your deck. Each other player reveals a Victory card from his hand and puts it on his deck (or reveals a hand with no Victory cards).") with Attack {
  override def play() {
    gain(Silver, currentPlayer.deck)
  }
  def attack() {
    cyclePlayers(_ => {
      if (currentPlayer.hand.exists { _.isInstanceOf[Victory] }) {
        val card = select(currentPlayer.hand.filter { _.isInstanceOf[Victory] }, 1)(0)
        card :: currentPlayer.deck
      } else {
        reveal(currentPlayer.hand)
        revealedCards = Nil
      }
    })
  }
}

object Cellar extends Action("Cellar", 2, "+1 Action. Discard any number of cards. +1 Card per card discarded.") {
  override def play() {
    actionCount += 1
    val cards = select(currentPlayer.hand)
    currentPlayer.hand = currentPlayer.hand diff cards
    currentPlayer.addToDiscard(cards)
    currentPlayer.drawToHand(cards.size)
  }
}

object Chancellor extends Action("Chancellor", 3, "+$2. You may immediately put your deck into your discard pile.") {
  override def play() {
    treasureCount += 2
    if (ask("Put deck into discard pile?")) {
      currentPlayer.addToDiscard(currentPlayer.deck)
      currentPlayer.deck = Nil
    }
  }
}

object Chapel extends Action("Chapel", 2, "Trash up to 4 cards from your hand.") {
  override def play() {
    val cards = select(currentPlayer.hand, 4)
    currentPlayer.hand = currentPlayer.hand diff cards
    trash(cards)
  }
}

object CouncilRoom extends Action("Council Room", 5, "+4 Cards, +1 Buy. Each other player draws a card.") {
  override def play() {
    currentPlayer.drawToHand(4)
    buyCount += 1
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
    val cards = availableSupply.keys.toList.filter { _.cost <= 5 }
    val card = select(cards, 1)(0)
    gain(card)
  }
}

object Festival extends Action("Festival", 5, "+2 Actions, +1 Buy, +$2.") {
  override def play() {
    actionCount += 2
    buyCount += 1
    treasureCount += 2
  }
}

object Gardens extends Victory("Gardens", 4) {
  override val description = "Worth 1vp for every 10 cards in your deck (rounded down)."
  override def value = Game.currentPlayer.deck.size / 10
}

object Laboratory extends Action("Laboratory", 5, "+2 Cards, +1 Action.") {
  override def play() {
    currentPlayer.drawToHand(2)
    actionCount += 1
  }
}

object Library extends Action("Library", 5, "Draw until you have 7 cards in hand. You may set aside any Action cards drawn this way, as you draw them; discard the set aside cards after you finish drawing.") {
  override def play() {
    var setAside: List[Card] = Nil
    while (currentPlayer.hand.size < 7 && (currentPlayer.deck.size + currentPlayer.discard.size) > 0) {
      val card = currentPlayer.takeFromDeck(1)(0)
      val setAsideCard = card.isInstanceOf[Action] && ask("Set this aside?")
      if (setAsideCard) setAside ::= card
      else currentPlayer.addToHand(List(card))
    }
    currentPlayer.addToDiscard(setAside)
  }
}

object Market extends Action("Market", 5, "+1 Card, +1 Action, +1 Buy, +$1.") {
  override def play() {
    currentPlayer.drawToHand(1)
    actionCount += 1
    buyCount += 1
    treasureCount += 1
  }
}

object Militia extends Action("Militia", 4, "+$2. Each other player discards down to 3 cards in his hand.") with Attack {
  override def play() {
    treasureCount += 2
  }
  def attack() {
    cyclePlayers(_ => {
      if (currentPlayer.hand.size > 3) {
        val cards = select(currentPlayer.hand, 3)
        currentPlayer.hand = cards
      }
    })
  }
}

object Mine extends Action("Mine", 5, "Trash a Treasure card from your hand. Gain a Treasure card costing up to $3 more; put it into your hand.") {
  override def play() {
    if (currentPlayer.hand.exists { _.isInstanceOf[Treasure] }) {
      val card = select(currentPlayer.hand.filter { _.isInstanceOf[Treasure] }, 1)(0)
      currentPlayer.hand = currentPlayer.hand diff List(card)
      trash(card)
      val cardToGain = select(availableSupply.keys.toList.filter { c => c.isInstanceOf[Treasure] && c.cost <= card.cost + 3 }, 1)(0)
      gain(cardToGain)
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
      currentPlayer.hand diff List(Copper)
      trash(Copper)
      treasureCount += 3
    }
  }
}

object Remodel extends Action("Remodel", 4, "Trash a card from your hand. Gain a card costing up to $2 more than the trashed card.") {
  override def play() {
    if (currentPlayer.hand != Nil) {
      val card = select(currentPlayer.hand, 1)(0)
      currentPlayer.hand diff List(card)
      trash(card)
      val cardToGain = select(availableSupply.keys.toList.filter { _.cost <= card.cost + 2 }, 1)(0)
      gain(cardToGain)
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
    actionCount += 1
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
      revealedCards = Nil
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
      val count = revealedCards.count { _.isInstanceOf[Treasure] }
      if (count > 0) {
        val card = (
          if (count == 1) revealedCards.filter { _.isInstanceOf[Treasure] }
          else select(revealedCards, 1)
        )(0)
        trashedCards ::= card
        revealedCards diff List(card)
        trash(card)
      }
      currentPlayer.addToDiscard(revealedCards)
    })
    if (trashedCards != Nil) {
      val cardsToGain = select(trashedCards)
      gain(cardsToGain)
    }
  }
}

object ThroneRoom extends Action("Throne Room", 4, "Choose an Action card in your hand. Play it twice.") {
  override def play() {
    if (currentPlayer.hand.count { _.isInstanceOf[Action] } > 0) {
      val card = select(currentPlayer.hand.filter { _.isInstanceOf[Action] }, 1)(0)
      Game.play(currentPlayer, card)
      card.play()
    }
  }
}

object Village extends Action("Village", 3, "+1 Cards, +2 Action.") {
  override def play() {
    currentPlayer.drawToHand(1)
    actionCount += 2
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
    buyCount += 1
    treasureCount += 2
  }
}

object Workshop extends Action("Workshop", 3, "Gain a card costing up to $4.") {
  override def play() {
    val card = select(availableSupply.keys.toList.filter { _.cost <= 4 }, 1)(0)
    gain(card)
  }
}