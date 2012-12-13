package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.actors.Actor
import sh.echo.dominion.model.cards.Reaction

trait View {
  def gameStarted(p: String, players: java.util.List[String], supply: java.util.List[Card])
  def turnStarted(p: String)
  def turnEnded(p: String)
  def nextPlayer(p: String)
  def cardPlayed(p: String, c: Card)
  def cardGained(p: String, c: Card, to: Int)
  def cardBought(p: String, c: Card)
  def reactionUsed(p: String, c: Card)
  def cardTrashed(p: String, c: Card)
  def cardRevealed(p: String, c: Card)
  def revealCleared()
  def playedCardsCleared()
  def countsUpdated(action: Int, buy: Int, treasure: Int)
  def selectedFromHand(p: String, cards: java.util.List[Card])
  def attackingPlayer(cur: String, target: String)
  
  def addedToHand(p: String, cards: java.util.List[Card])
  def addedToDiscard(p: String, cards: java.util.List[Card])
  def addedToDeck(p: String, cards: java.util.List[Card])
  def discardedHand(p: String, hand: java.util.List[Card])
  def shuffledDiscardIntoDeck(p: String, deckCount: Int)
  def tookCardFromDeck(p: String)
  
  def waitingFor(p: String)
  def waitedFor(p: String)
  def ask(msg: String): Boolean
  def selectFromHand(cards: java.util.List[Card], max: Int, exact: Boolean): java.util.List[Card]
  def selectFromList(cards: java.util.List[Card], max: Int, exact: Boolean): java.util.List[Card]
  def selectFromSupplyForGain(cards: java.util.List[Card]): Card
}

trait Controller {
  def createGame()
  def addPlayer(name: String, v: View): String
  def startGame(p: String)
  def play(p: String, c: Card)
  def buy(p: String, c: Card)
  def endTurn(p: String)
}
