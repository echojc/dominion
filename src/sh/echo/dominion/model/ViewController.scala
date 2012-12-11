package sh.echo.dominion.model

import sh.echo.dominion.model.cards.Card

trait View {
  def gameStarted(p: Player, players: java.util.List[Player], supply: java.util.List[Card])
  def turnStarted(p: Player)
  def turnEnded(p: Player)
  def nextPlayer(p: Player)
  def cardPlayed(p: Player, c: Card)
}

trait Controller {
  def createGame()
  def addPlayer(name: String, v: View): Player
  def startGame(p: Player)
  def startTurn(p: Player)
  def play(p: Player, c: Card)
  def endTurn(p: Player)
}
