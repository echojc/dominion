package sh.echo.dominion.model

import sh.echo.dominion.model.cards._
import scala.util.Random
import scala.collection.JavaConversions._
import scala.collection.mutable.Map

object Game {

  var actionCount = 1
  var buyCount = 1
  var treasureCount = 0
  
  def currentPlayer = players(currentPlayerIndex)
  var players: List[Player] = Nil
  var currentPlayerIndex = 0
  
  var playedCards: List[Card] = Nil
  var revealedCards: List[Card] = Nil
  var trash: List[Card] = Nil
  
  var supply: Map[Card, Int] = Map()
  def availableSupply = supply.filter { _._2 > 0 }
  
  def startTurn() {
    actionCount = 1
    buyCount = 1
    treasureCount = 0
  }
  
  def endTurn() {
    if (revealedCards != Nil) throw new IllegalStateException("Revealed cards were left revealed.");
    
    currentPlayer.discardHand
    currentPlayer.discard ++= playedCards
    playedCards = Nil
  }
  
  def play(card: Card) {
    if (!card.isPlayable) throw new IllegalArgumentException("Tried to play unplayable card.")
    if (!currentPlayer.hand.contains(card)) throw new IllegalArgumentException("Tried to play non-existent card.")
    if (card.isInstanceOf[Action]) {
      if (actionCount == 0) throw new IllegalArgumentException("No more actions!")
      actionCount -= 1
    }
    currentPlayer.hand = currentPlayer.hand diff List(card)
    playedCards ::= card
    card.play()
  }
  
  def nextPlayer() {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
  }
  
  def reveal(card: Card) {
    revealedCards ::= card
  }
  
  def reveal(cards: List[Card]) {
    cards.foreach(reveal)
  }
  
  def gain(cards: List[Card]) {
    cards.foreach(gain)
  }
  
  def gain(card: Card) {
    gain(card, Game.currentPlayer.discard)
  }
  
  def gain(card: Card, pile: List[Card]) {
    if (supply(card) > 0) {
      supply(card) -= 1
      card :: pile
    }
  }
  
  def select(from: List[Card], count: Int = -1, playerIndex: Int = currentPlayerIndex): List[Card] = {
    if (count == -1) from
    else from.take(count)
  }
  
  def ask(msg: String, playerIndex: Int = currentPlayerIndex) : Boolean = {
    false
  }
  
  def trash(card: Card) {
    card :: trash
  }
  
  def trash(cards: List[Card]) {
    cards.foreach(trash)
  }
  
  def cyclePlayers(actions: Int => Unit, includeSelf: Boolean = false) {
    val origPlayerIndex = currentPlayerIndex;
    if (!includeSelf) nextPlayer()
    do {
      actions(origPlayerIndex)
      nextPlayer()
    } while (currentPlayerIndex != origPlayerIndex)
  }
  
  def prepareSupply() {
    supply = Map()
    def addToSupply(card: Card) {
      supply(card) = card.count
    }
    CardLists.Special.foreach(addToSupply)
    Random.shuffle(CardLists.Base).take(10).foreach(addToSupply)
  }
  
  def createGame {
    players = Nil
    currentPlayerIndex = 0
  
    playedCards = Nil
    revealedCards = Nil
    trash = Nil
  }
  
  def addPlayer() {
    players ::= new Player()
  }
  
  def startGame {
    Random.shuffle(players)
    prepareSupply()
  }
  
  def supplyCount(card: Card): Int = {
    supply(card)
  }
  
  def sortedKingdomSupply(): java.util.List[Card] = {
    supply.filterNot(c => CardLists.Special.contains(c._1)).keys.toList.sortWith((a, b) => {
      if (a.cost != b.cost) a.cost < b.cost
      else a.name < b.name
    })
  }
}