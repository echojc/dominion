package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.util.Random

class Player {
  var deck: List[Card] = Nil
  var hand: List[Card] = Nil
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
    if (deck == Nil) shuffleDiscardIntoDeck()
    val card = deck.head
    deck = deck.tail
    card :: takeFromDeck(count - 1)
  }
}
