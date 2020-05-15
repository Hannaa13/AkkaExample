package strategy

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{Behavior, SupervisorStrategy}
import akka.actor.{Actor, OneForOneStrategy, Props}

object Supervision extends App {

  import akka.actor.ActorSystem

  val system = ActorSystem("system")
  val command = system.actorOf(Props[SupervisionActor], "command")

  command ! Props[ActorInc]

  case class Report(msg: String)

  class ActorInc extends Actor {

    var counter = 0

    override def receive: Receive = {
      case Report(worlds) => worlds match {

        case "" => throw new NullPointerException

        case words: String =>
          if (words.length > 20)
            throw new RuntimeException

          else if (!Character.isUpperCase(words(0)))
            throw new IllegalArgumentException

          else counter += words.split("").length
          println(counter)

        case _ => throw new Exception
      }
    }
  }

  class SupervisionActor extends Actor {
    override def receive: Receive = {
      case props: Props =>
        val childRef = context.actorOf(props)
        childRef ! Report("I love Akka (no)")
        childRef ! Report("Hate Akka Hate Akka Hate Akka Hate Akka Hate Akka Hate Akka")
        childRef ! Report("scala")
        childRef ! Report("")
    }

    override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy() {
      case _: Exception => Escalate
      case _: RuntimeException => Resume
      case _: IllegalArgumentException => Stop
      case _: NullPointerException => Restart

    }
  }

}



object SupervisionTyped extends App {

  import akka.actor.typed.ActorSystem

  val system = ActorSystem(Counter(), "system")

  system ! Counter.Message("")
  system ! Counter.Message("I love Scala, but hate Akka!")
  system ! Counter.Message("I hate Akka!")

  object Counter {

    case class Message(str: String)


    def apply(): Behavior[Message] = Behaviors.supervise[Message] {
      Behaviors.supervise[Message] {
        Behaviors.receive { (context, message) =>
          message match {
            case Message("") => context.log.warn("NULLPOINTER EX")
              throw new NullPointerException

            case Message(str) if str.length > 20 =>
              context.log.warn("ILLEGAL ARGUMENT")
              throw new IllegalArgumentException

            case Message(str) =>
              val length = str.split("").length

              context.log.info("length == {}", length)
              Behaviors.same

          }

        }
      }.onFailure[NullPointerException](SupervisorStrategy.restart)
    }.onFailure[IllegalArgumentException](SupervisorStrategy.stop)


  }

}