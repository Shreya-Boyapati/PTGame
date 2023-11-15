import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import scala.collection.immutable
import Main._

final case class User(name: String, age: Int, countryOfResidence: String)
final case class Users(users: immutable.Seq[User])

object UserRegistry {
  sealed trait Command
  final case class GetUsers(replyTo: ActorRef[ActionPerformed]) extends Command
  final case class MakeMove(node: String, replyTo: ActorRef[ActionPerformed]) extends Command
  final case class ActionPerformed(description: String)

  private var turn = true // flag to check whether policeman or thief turn
  private var over = false // flag to check if game is over

  def apply(): Behavior[Command] = registry()

  /*
   * Enumerates information about the graphs and the state of the gameplay. Current positions of
   * policeman and thief are printed. Also printed are the nodes adjacent to both players as
   * determined by the perturbed graph and the list of nodes that have been modified between the
   * original graph and the perturbed graph as determined by the simrank algorithm. Checks if
   * either player is stuck. If a player is stuck, they automatically lose an the other wins.
   */
  def nodeInfo(): Unit = {
    logger.info(s"Policeman is at $pos0 and thief is at $pos1")

    logger.info("Node and edge info...")
    logger.info("Nodes adjacent to policeman are " + secondShard.adjacentNodes(pos0))
    logger.info("Nodes adjacent to thief are " + secondShard.adjacentNodes(pos1))
    logger.info(s"Nodes ${simrank.toArray.mkString("", ", ", "")} have been modified.")
    logger.info(s"Nodes ${valuable.mkString("", ", ", "")} contain valuable data.")

    if (firstShard.adjacentNodes(pos0).isEmpty) {
      logger.info("Policeman is stuck. Game over.")
      logger.info("Thief wins!")
    }

    if (firstShard.adjacentNodes(pos1).isEmpty) {
      logger.info("Thief is stuck. Game over.")
      logger.info("Policeman wins!")
    }
  }

  private def registry(): Behavior[Command] =
    Behaviors.receiveMessage {
      /*
       * Command to begin the game and print the initial state information.
       * The policeman will begin as the first player.
       */
      case GetUsers(replyTo) =>
        replyTo ! ActionPerformed("Game has begun!")
        logger.info(s"The game has begun! Policeman is at $pos0 and thief is at $pos1")
        logger.info("Policeman plays first.\n")
        nodeInfo()
        Behaviors.same
      /*
       * Command to make a move. The player whose turn it is will attempt to make a move to
       * the position given in the http command. If the node given is not an adjacent node
       * to the player's current node, the move is invalid and the player loses. Else the
       * player is moved to the new node. After each move, two checks are made. If the thief
       * is on a node with valuable data, the thief wins. Else if the policeman and the thief are
       * on the same node, the policeman wins. If there is no victor, play continues with the next
       * player's turn.
       */
      case MakeMove(node, replyTo) =>
        replyTo ! ActionPerformed(s"Move to node $node attempted!")
        logger.info(s"Move made to node $node")

        if (turn) {
          if (!firstShard.adjacentNodes(pos0).contains(node)) {
            logger.info("Invalid move. Game over.")
            logger.info("Thief wins!")
            over = true
            system.terminate()
          }
          else {
            pos0 = node
          }
        }
        else {
          if (!firstShard.adjacentNodes(pos1).contains(node)) {
            logger.info("Invalid move. Game over.")
            logger.info("Policeman wins!")
            over = true
            system.terminate()
          }
          else {
            pos1 = node
          }
        }

        if (valuable.contains(pos1)) {
          logger.info("Thief wins!")
          over = true
          system.terminate()
        }
        else if (pos0 == pos1) {
          logger.info("Policeman wins!")
          over = true
          system.terminate()
        }

        if (!over) {
          if (turn) {
            logger.info("Now it's thief's turn!")
          }
          else {
            logger.info("Now it's policeman's turn!")
          }

          nodeInfo()
        }

        turn = !turn
        Behaviors.same
    }
}
