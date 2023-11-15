import UserRegistry._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import scala.concurrent.Future
import JsonFormats._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

/*
 * Router to call appropriate command(s) from registry
 */
class Routes(userRegistry: ActorRef[UserRegistry.Command])(implicit val system: ActorSystem[_]) {
  private implicit val timeout: Timeout = Timeout.create(system.settings.config.getDuration("app.routes.ask-timeout"))

  def initialize(): Future[ActionPerformed] =
    userRegistry.ask(GetUsers.apply)
  def makeMove(node: String): Future[ActionPerformed] =
    userRegistry.ask(MakeMove(node, _))

  // checks if command is for initializing the game or for making a move
  val userRoutes: Route =
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            get {
              complete(initialize())
            })
        },
        path(Segment) { name =>
          concat(
            get {
              rejectEmptyResponse {
                onSuccess(makeMove(name)) { response =>
                  complete(response)
                }
              }
            }
          )
        }
      )
    }
}
