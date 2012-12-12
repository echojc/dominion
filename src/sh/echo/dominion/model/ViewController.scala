package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import scala.actors.Actor

trait View {
  def gameStarted(p: String, players: java.util.List[String], supply: java.util.List[Card])
  def turnStarted(p: String)
  def turnEnded(p: String)
  def nextPlayer(p: String)
  def cardPlayed(p: String, c: Card)
  def cardGained(p: String, c: Card)
  def cardGainedToDeck(p: String, c: Card)
  def cardTrashed(c: Card)
  def cardRevealed(c: Card)
  def revealCleared()
  def playedCardsCleared()
  def countsUpdated(action: Int, buy: Int, treasure: Int)
  def selectedFromHand(p: String, count: Int)
  
  def addedToHand(p: String, cards: java.util.List[Card])
  def addedToDiscard(p: String, cards: java.util.List[Card])
  def addedToDeck(p: String, cards: java.util.List[Card])
  def discardedHand(p: String, discardCount: Int)
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
  def startTurn(p: String)
  def play(p: String, c: Card)
  def endTurn(p: String)
}

class ViewActor(val view: View) extends Actor {
  type Callback[T] = View => T
  def act() {
    loop {
      react {
        case event: Callback[Unit] => event(view)
        case event: Callback[_] => reply(event(view))
        case _ => {}
      }
    }
  }
}
