import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class routertest extends AnyWordSpec with Matchers with ScalaFutures with ScalatestRouteTest{
  lazy val testKit = ActorTestKit()
  implicit def typedSystem: ActorSystem[_] = testKit.system

  override def createActorSystem(): akka.actor.ActorSystem = testKit.system.classicSystem


  val registry = testKit.spawn(UserRegistry())
  lazy val routes = new Routes(registry).userRoutes

  "UserRoutes" should {
    "show that game has begun" in {
      val request = HttpRequest(uri = "/users")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)

        entityAs[String] should ===("""{"description":"Game has begun!"}""")
      }
    }
  }

  "UserRoutes" should {
    "show that move was attempted" in {
      val request = HttpRequest(uri = "/users/3")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)

        entityAs[String] should ===("""{"description":"Move to node 3 attempted!"}""")
      }
    }
  }

  "UserRoutes" should {
    "show that always invalid move was attempted" in {
      val request = HttpRequest(uri = "/users/33.3")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)

        entityAs[String] should ===("""{"description":"Move to node 33.3 attempted!"}""")
      }
    }
  }

  "UserRoutes" should {
    "show that nonsense move was made" in {
      val request = HttpRequest(uri = "/users/banana")

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)

        entityAs[String] should ===("""{"description":"Move to node banana attempted!"}""")
      }
    }
  }
}