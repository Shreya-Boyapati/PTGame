import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Success}
import java.io.{FileInputStream, ObjectInputStream}
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable

object Main {
  val random = new Random
  val logger = LoggerFactory.getLogger("main")
  val applicationConf: Config = ConfigFactory.load("application.conf")

  // global variables created to be shareable with registry
  var pos0 = "0"
  var pos1 = "1"
  var valuable = Array("")
  var simrank = ListBuffer[String]()
  var firstShard = createGraph(Array("0", "1"), Array("0:1"))
  var secondShard = createGraph(Array("0", "1"), Array("0:1"))

  /*
   * Reconstructs a graph of the type MutableValueGraph from the lists of nodes and edges provided.
   * This graph is used for simrank calculations and gameplay. Used in homework 2.
   */
  def createGraph(nodesList: Array[String], edgesList: Array[String]): MutableValueGraph[String, Int] = {
    val graph: MutableValueGraph[String, Int] = ValueGraphBuilder.directed.allowsSelfLoops(false).build

    nodesList.foreach(node => graph.addNode(node))
    edgesList.foreach(node => graph.putEdgeValue(node.split(":")(0), node.split(":")(1), 1))

    graph
  }

  /*
   * Compares original and perturbed graph to determine the delta between the two.
   * Inspired by simrank algorithm of homework 1/used in homework 2.
   * Algorithm traverses through the list of nodes of the original graph
   * and compares it to the nodes of the perturbed graph to determine which have been removed.
   * The same is done with the two flipped to determine which nodes have been added.
   * The nodes that have been modified are added to a list, which is returned.
   */
  def simRank(orig: MutableValueGraph[String, Int], perturbed: MutableValueGraph[String, Int]): ListBuffer[String] = {
    val thisNodes = orig.nodes().asScala.toList.par
    val thatNodes = perturbed.nodes().asScala.toList.par

    val lst = new ListBuffer[String]()

    thisNodes.foldLeft(0) {
      case (acc, node) =>
        if (thatNodes.exists(_ == node)) {
          acc
        }
        else {
          lst += node
          acc + 1
        }

    } + thatNodes.foldLeft(0) {
      case (acc, node) => if (thisNodes.exists(_ == node)) {
        acc
      }
      else {
        lst += node
        acc + 1
      }
    }

    lst
  }

  /*
   * Starts the http server on the port 8080. Checks for success or for error in connection.
   */
  private def startHttpServer(routes: Route)(implicit system: ActorSystem[_]): Unit = {
    import system.executionContext

    val futureBinding = Http().newServerAt("localhost", 8080).bind(routes)
    futureBinding.onComplete {
      case Success(binding) =>
        logger.info("Server online!")
      case Failure(ex) =>
        logger.error("Failed to bind HTTP endpoint, terminating system", ex)
        system.terminate()
    }
  }

  def main(args: Array[String]): Unit = {
    // get object g which will be used to reconstruct the graphs from NetGameSim
    val fileInputStream = new FileInputStream(applicationConf.getString("app.shardsFile"))
    val objectInputStream = new ObjectInputStream(fileInputStream)
    val g = objectInputStream.readObject()
    objectInputStream.close()

    // Recreate the graphs from the file contents.
    // Contents are formatted as four semi-colon separated lists.
    firstShard = createGraph(g.toString.split(";")(0).split(","), g.toString.split(";")(2).split(","))
    secondShard = createGraph(g.toString.split(";")(1).split(","), g.toString.split(";")(3).split(","))

    valuable = g.toString.split(";")(4).split(",") // get list of nodes with valuable data
    simrank = simRank(firstShard, secondShard) // get list of modified nodes

    // policeman and thief are both assigned to random starting nodes
    val listOfNodes = g.toString.split(";")(0).split(",")
    pos0 = listOfNodes(random.nextInt(listOfNodes.length)).split(" ").head
    pos1 = listOfNodes(random.nextInt(listOfNodes.length)).split(" ").head
  }
  // bootstrap the server
  val rootBehavior = Behaviors.setup[Nothing] { context =>
    val userActor = context.spawn(UserRegistry(), "UserActor")
    context.watch(userActor)

    val routes = new Routes(userActor)(context.system)
    startHttpServer(routes.userRoutes)(context.system)
    Behaviors.empty

  }
  logger.info("Welcome to the policeman and thief game!")

  val system = ActorSystem[Nothing](rootBehavior, "AkkaHttpServer")
} 