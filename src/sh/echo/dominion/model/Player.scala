package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.util.Random
import sh.echo.dominion.model.cards.special.Copper;
import sh.echo.dominion.model.cards.special.Estate;
import scala.collection.JavaConversions._

class Player(val name: String) {
  import Game._
  
  var deck: List[Card] = Nil
  var hand: List[Card] = Nil
  var discard: List[Card] = Nil
  
  def init() {
    deck = Random.shuffle(List(Estate, Estate, Estate, Copper, Copper, Copper, Copper, Copper, Copper, Copper))
    drawToHand(5)
  }
  
  def drawToHand(count: Int) {
    addToHand(takeFromDeck(count))
  }
  
  def addToHand(cards: List[Card]) {
    hand :::= cards
    fireEventWithSpecial(this, _.addedToHand(name, cards), _.addedToHand(name, cards.map(_ => null)))
  }
  
  def addToDiscard(cards: List[Card]) {
    discard :::= cards
    fireEvent(_.addedToDiscard(name, cards))
  }
  
  def addToDeck(cards: List[Card]) {
    deck :::= cards
    fireEventWithSpecial(this, _.addedToDeck(name, cards), _.addedToDeck(name, cards.map(_ => null)))
  }
  
  def discardHand() {
    discard ++= hand
    fireEvent(_.discardedHand(name, hand))
    hand = Nil
  }
  
  def shuffleDiscardIntoDeck() {
    deck ++= discard
    discard = Nil
    deck = Random.shuffle(deck)
    fireEvent(_.shuffledDiscardIntoDeck(name, deck.size))
  }
  
  def takeFromDeck(count: Int): List[Card] = {
    if (count == 0) Nil
    else {
      if (deck == Nil) shuffleDiscardIntoDeck()
      if (deck == Nil) Nil
      val card = deck.head
      deck = deck.tail
      fireEventWithSpecial(this, _.tookCardFromDeck(name, card), _.tookCardFromDeck(name, null))
      card :: takeFromDeck(count - 1)
    }
  }
}

object Player {
  def get() = this
  
  val PILE_DECK = 0
  val PILE_HAND = 1
  val PILE_DISCARD = 2
}