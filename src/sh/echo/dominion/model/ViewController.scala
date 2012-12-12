package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.actors.Actor

trait View {
  def gameStarted(p: Player, players: java.util.List[Player], supply: java.util.List[Card])
  def turnStarted(p: Player)
  def turnEnded(p: Player)
  def nextPlayer(p: Player)
  def cardPlayed(p: Player, c: Card)
  def cardGained(p: Player, c: Card)
  def cardGainedToDeck(p: Player, c: Card)
  def cardTrashed(c: Card)
  def cardRevealed(c: Card)
  def revealCleared()
  def playedCardsCleared()
  def countsUpdated(action: Int, buy: Int, treasure: Int)
  def selectedFromHand(p: Player, count: Int)
  
  def addedToHand(p: Player, cardCount: Int)
  def addedToDiscard(p: Player, cards: List[Card])
  def addedToDeck(p: Player, cardCount: Int)
  def discardedHand(p: Player, discardCount: Int)
  def shuffledDiscardIntoDeck(p: Player, deckCount: Int)
  def drewCardFromDeck(p: Player)
  
  def waitingFor(p: Player)
  def waitedFor(p: Player)
  def ask(msg: String): Boolean
  def selectFromHand(cards: List[Card], max: Int, exact: Boolean): List[Card]
  def selectFromList(cards: List[Card], max: Int, exact: Boolean): List[Card]
  def selectFromSupplyForGain(cards: List[Card]): Card
}

trait Controller {
  def createGame()
  def addPlayer(name: String, v: View): Player
  def startGame(p: Player)
  def startTurn(p: Player)
  def play(p: Player, c: Card)
  def endTurn(p: Player)
}
