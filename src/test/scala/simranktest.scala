import com.google.common.graph.{MutableValueGraph, ValueGraphBuilder}
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable.ListBuffer

class simranktest extends AnyWordSpec {
  "simrank" should {
    "show delta between graphs" in {
      val firstShard: MutableValueGraph[String, Int] = ValueGraphBuilder.directed.allowsSelfLoops(false).build
      val secondShard: MutableValueGraph[String, Int] = ValueGraphBuilder.directed.allowsSelfLoops(false).build

      List("1", "2", "3", "4", "5", "6", "7").foreach(node => firstShard.addNode(node))
      List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11").foreach(node => secondShard.addNode(node))

      firstShard.putEdgeValue("1", "2", 1)
      firstShard.putEdgeValue("5", "6", 1)

      secondShard.putEdgeValue("1", "2", 1)
      secondShard.putEdgeValue("5", "6", 1)

      val result = Main.simRank(firstShard, secondShard)
      assert(result == ListBuffer("8", "9", "10", "11"))
      }
    }

}
