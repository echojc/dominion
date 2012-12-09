package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card
import sh.echo.dominion.model.cards.Special
import scala.util.Random
import sh.echo.dominion.model.cards.Base

object Game {

  var actionCount = 1
  var buyCount = 1
  var treasureCount = 0
  
  def currentPlayer: Player = players(currentPlayerIndex)
  var players: List[Player] = Nil
  var currentPlayerIndex = 0
  
  var playedCards: List[Card] = Nil
  var revealedCards: List[Card] = Nil
  var trash: List[Card] = Nil
  
  var supply: List[(Card, Int)] = Nil
  
  def startTurn {
    actionCount = 1
    buyCount = 1
    treasureCount = 0
  }
  
  def endTurn {
    if (revealedCards != Nil) throw new IllegalStateException("Revealed cards were left revealed.");
    
    currentPlayer.discardHand
    currentPlayer.discard ++= playedCards
    playedCards = Nil
    
    Game.nextPlayer
  }
  
  def playCard(card: Card) {
    if (!card.isPlayable) throw new IllegalArgumentException("Tried to play unplayable card.")
    else card.play
  }
  
  def nextPlayer {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
  }
  
  def prepareSupply {
    def addToSupply(card: Card) {
      supply ::= (card, card.count)
    }
    Special.all foreach addToSupply
    Random.shuffle(Base.all).take(10) foreach addToSupply
  }
  
  def createGame {
    players = Nil
    currentPlayerIndex = 0
  
    playedCards = Nil
    revealedCards = Nil
    trash = Nil
  }
}