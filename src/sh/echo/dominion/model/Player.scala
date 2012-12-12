package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.util.Random
import sh.echo.dominion.model.cards.special.Copper;
import sh.echo.dominion.model.cards.special.Estate;

class Player(val name: String) {
  import Game._
  
  var deck: List[Card] = Random.shuffle(List(Estate, Estate, Estate, Copper, Copper, Copper, Copper, Copper, Copper, Copper))
  var hand: List[Card] = takeFromDeck(5)
  var discard: List[Card] = Nil
  
  def drawToHand(count: Int) {
    addToHand(takeFromDeck(count))
  }
  
  def addToHand(cards: List[Card]) {
    hand :::= cards
    fireEvent(_.addedToHand(currentPlayer, cards.size))
  }
  
  def addToDiscard(cards: List[Card]) {
    discard :::= cards
    fireEvent(_.addedToDiscard(currentPlayer, cards))
  }
  
  def addToDeck(cards: List[Card]) {
    deck :::= cards
    fireEvent(_.addedToDeck(currentPlayer, cards.size))
  }
  
  def discardHand() {
    discard ++= hand
    hand = Nil
    fireEvent(_.discardedHand(currentPlayer, discard.size))
  }
  
  def shuffleDiscardIntoDeck() {
    deck ++= discard
    discard = Nil
    deck = Random.shuffle(deck)
    fireEvent(_.shuffledDiscardIntoDeck(currentPlayer, deck.size))
  }
  
  def takeFromDeck(count: Int): List[Card] = {
    if (count == 0) Nil
    else {
      if (deck == Nil) shuffleDiscardIntoDeck()
      if (deck == Nil) Nil
      val card = deck.head
      deck = deck.tail
      fireEvent(_.drewCardFromDeck(currentPlayer))
      card :: takeFromDeck(count - 1)
    }
  }
}
