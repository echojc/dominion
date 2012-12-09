package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.util.Random

class Player {
  var deck: List[Card] = Nil
  var hand: List[Card] = Nil
  var discard: List[Card] = Nil
  
  def drawToHand(count: Int) {
    if (count == 0) return
    if (deck == Nil) shuffleDiscardIntoDeck
    hand ::= deck.head
    deck = deck.tail
    drawToHand(count - 1)
  }
  
  def discardHand() {
    discard ++= hand
    hand = Nil
  }
  
  def shuffleDiscardIntoDeck {
    deck ++= discard
    discard = Nil
    deck = Random.shuffle(deck)
  }
}