package sh.echo.dominion.model

import sh.echo.dominion.model.cards._
import scala.util.Random
import scala.collection.JavaConversions._
import scala.collection.mutable.Map

object Game extends Controller {
  def get() = this
  
  def createGame() {
    players = Nil
    playedCards = Nil
    revealedCards = Nil
    trash = Nil
  }
  
  def addPlayer(name: String, v: View): String = {
    val player = new Player(name)
    players ::= player
    playerViews += player -> v
    name
  }
  
  def startGame(p: String) {
    Random.shuffle(players)
    
    supply = Map()
    (CardLists.Special ++ Random.shuffle(CardLists.Base).take(10)).foreach(c => supply(c) = c.count)
    fireEvent(_.gameStarted(p, players.map(_.name), supply.keys.toList))
    
    players.foreach(_.init())
    currentPlayerIndex = 0
    fireEvent(_.nextPlayer(currentPlayer.name))
    
    startTurn();
  }
  
  def play(p: String, card: Card) {
    if (p.toLowerCase() != currentPlayer.name.toLowerCase()) throw new IllegalArgumentException("Player is acting out of turn")
    if (!playerCanPlayAction) throw new IllegalArgumentException("Cannot play any action cards!")
    if (!card.isPlayable) throw new IllegalArgumentException("Tried to play unplayable card.")
    if (!currentPlayer.hand.contains(card)) throw new IllegalArgumentException("Tried to play non-existent card.")
    if (card.isInstanceOf[Action]) {
      if (actionCount == 0) throw new IllegalArgumentException("No more actions!")
      updateAction(-1)
    }
    
    currentPlayer.hand = currentPlayer.hand diff List(card)
    playedCards ::= card
    fireEvent(_.cardPlayed(currentPlayer.name, card))
    card.play()
  }
  
  def buy(p: String, card: Card) {
    if (p.toLowerCase() != currentPlayer.name.toLowerCase()) throw new IllegalArgumentException("Player is acting out of turn")
    if (!supply.contains(card)) throw new IllegalArgumentException("Card is not in supply.")
    if (supply(card) == 0) throw new IllegalArgumentException("No more copies of card to buy.")
    
    updateBuy(-1)
    updateTreasure(-card.cost)
    playerCanPlayAction = false
    gain(card)
  }
  
  def endTurn(p: String) {
    if (p.toLowerCase() != currentPlayer.name.toLowerCase()) throw new IllegalArgumentException("Player is acting out of turn")
    if (revealedCards != Nil) throw new IllegalStateException("Revealed cards were left revealed.")
    
    currentPlayer.discardHand()
    currentPlayer.addToDiscard(playedCards)
    playedCards = Nil
    fireEvent(_.playedCardsCleared())
    
    currentPlayer.drawToHand(5);
    fireEvent(_.turnEnded(currentPlayer.name))
    
    nextPlayer()
    startTurn()
  }

  var actionCount = 1
  var buyCount = 1
  var treasureCount = 0
  var playerCanPlayAction = true;
  
  def currentPlayer = players(currentPlayerIndex)
  var players: List[Player] = Nil
  var playerViews: Map[Player, View] = Map()
  var currentPlayerIndex = 0
  
  var playedCards: List[Card] = Nil
  var revealedCards: List[Card] = Nil
  var trash: List[Card] = Nil
  
  var supply: Map[Card, Int] = Map()
  def availableSupply = supply.filter { _._2 > 0 }
  
  def nextPlayer() {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
    fireEvent(_.nextPlayer(currentPlayer.name))
  }
  
  def startTurn() {
    actionCount = 1
    buyCount = 1
    treasureCount = 0
    playerCanPlayAction = true;
    fireEvent(_.countsUpdated(actionCount, buyCount, treasureCount))
    fireEvent(_.turnStarted(currentPlayer.name))
  }
  
  def reveal(card: Card) {
    revealedCards ::= card
    fireEvent(_.cardRevealed(card))
  }
  
  def reveal(cards: List[Card]) {
    cards.foreach(reveal)
  }
  
  def revealClear() {
    revealedCards = Nil
    fireEvent(_.revealCleared())
  }
  
  def gain(cards: List[Card]) {
    cards.foreach(c => gain(c))
  }
  
  def gain(card: Card, to: Int = Player.PILE_DISCARD) {
    if (supply(card) > 0) {
      supply(card) -= 1
      fireEvent(_.cardGained(currentPlayer.name, card, to))
      to match {
        case Player.PILE_DECK => currentPlayer.addToDeck(List(card))
        case Player.PILE_DISCARD => currentPlayer.addToDiscard(List(card))
        case Player.PILE_HAND => currentPlayer.addToHand(List(card))
      }
    }
  }
  
  def selectFromHand(filter: Card => Boolean = _ => true, max: Int = -1, exact: Boolean = false, auto: Boolean = false): List[Card] = {
    val cards = currentPlayer.hand.filter(filter)
    var selected: List[Card] = null
    if (auto) {
      selected = 
        if (max == -1) cards
        else cards.take(max)
    } else {
      fireEvent(_.waitingFor(currentPlayer.name))
      selected = fireEventForResult(currentPlayer, _.selectFromHand(cards, max, exact)).toList
      fireEvent(_.waitedFor(currentPlayer.name))
    }
    if ((exact && selected.size != max) || (!exact && max != -1 && selected.size > max)) 
      throw new IllegalStateException("Incorrect number of cards received.")
    if (!(selected diff cards).isEmpty)
      throw new IllegalStateException("Invalid cards received.")
    currentPlayer.hand = currentPlayer.hand diff selected
    fireEventWithSpecial(currentPlayer,
        _.selectedFromHand(currentPlayer.name, selected),
        _.selectedFromHand(currentPlayer.name, selected.map(_ => null)))
    selected
  }
  
  def selectFromList(cards: List[Card], max: Int = -1, exact: Boolean = false): List[Card] = {
    fireEvent(_.waitingFor(currentPlayer.name))
    val selected = fireEventForResult(currentPlayer, _.selectFromList(cards, max, exact)).toList
    fireEvent(_.waitedFor(currentPlayer.name))
    
    if ((exact && selected.size != max) || (!exact && max != -1 && selected.size > max)) 
      throw new IllegalStateException("Incorrect number of cards received.")
    if (!(selected diff cards).isEmpty)
      throw new IllegalStateException("Invalid cards received.")
    selected
  }
  
  def selectAndGainFromSupply(filter: Card => Boolean = _ => true, to: Int = Player.PILE_DISCARD) {
    fireEvent(_.waitingFor(currentPlayer.name))
    val selected = fireEventForResult(currentPlayer, _.selectFromSupplyForGain(supply.keys.filter(filter).toList))
    fireEvent(_.waitedFor(currentPlayer.name))
    
    if (!filter(selected) || supply(selected) == 0)
      throw new IllegalStateException("Invalid card selected from supply for gain.")
    gain(selected, to)
  }
  
  def ask(msg: String, playerIndex: Int = currentPlayerIndex) : Boolean = {
    val player = players(playerIndex)
    fireEvent(_.waitingFor(player.name))
    val reply = fireEventForResult(player, _.ask(msg))
    fireEvent(_.waitedFor(player.name))
    reply
  }
  
  def trash(card: Card) {
    card :: trash
    fireEvent(_.cardTrashed(card))
  }
  
  def trash(cards: List[Card]) {
    cards.foreach(trash)
  }
  
  def updateAction(value: Int) {
    actionCount += value
    fireEvent(_.countsUpdated(actionCount, buyCount, treasureCount))
  }
  
  def updateBuy(value: Int) {
    buyCount += value
    fireEvent(_.countsUpdated(actionCount, buyCount, treasureCount))
  }
  
  def updateTreasure(value: Int) {
    treasureCount += value
    fireEvent(_.countsUpdated(actionCount, buyCount, treasureCount))
  }
  
  def cyclePlayers(actions: Int => Unit, includeSelf: Boolean = false) {
    val origPlayerIndex = currentPlayerIndex;
    if (!includeSelf) nextPlayer()
    do {
      actions(origPlayerIndex)
      nextPlayer()
    } while (currentPlayerIndex != origPlayerIndex)
  }
  
  def fireEvent(event: View => Unit) {
    playerViews.values.foreach(event)
  }
  
  def fireEventWithSpecial(player: Player, eventForPlayer: View => Unit, eventForOthers: View => Unit) {
    eventForPlayer(playerViews(player))
    playerViews.filterNot(_._1 == player).values.foreach(eventForOthers)
  }
  
  def fireEventForResult[T](p: Player, event: View => T): T = {
    event(playerViews(p))
  }
}