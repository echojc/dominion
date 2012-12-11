package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.util.Random
import sh.echo.dominion.model.cards.special.Copper;
import sh.echo.dominion.model.cards.special.Estate;

class Player(val name: String) {
  var deck: List[Card] = Random.shuffle(List(Estate, Estate, Estate, Copper, Copper, Copper, Copper, Copper, Copper, Copper))
  var hand: List[Card] = takeFromDeck(5)
  var discard: List[Card] = Nil
  
  def drawToHand(count: Int) {
    addToHand(takeFromDeck(count))
  }
  
  def addToHand(cards: List[Card]) {
    hand :::= cards
  }
  
  def addToDiscard(cards: List[Card]) {
    discard :::= cards
  }
  
  def addToDeck(cards: List[Card]) {
    deck :::= cards
  }
  
  def discardHand() {
    discard ++= hand
    hand = Nil
  }
  
  def shuffleDiscardIntoDeck() {
    deck ++= discard
    discard = Nil
    deck = Random.shuffle(deck)
  }
  
  def takeFromDeck(count: Int): List[Card] = {
    if (count == 0) Nil
    else {
      if (deck == Nil) shuffleDiscardIntoDeck()
      val card = deck.head
      deck = deck.tail
      card :: takeFromDeck(count - 1)
    }
  }
}
