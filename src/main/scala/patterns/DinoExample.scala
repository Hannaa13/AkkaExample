package patterns

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object SimpleActor extends App {
  // это главный, туда передается фабрика (или метод. или переменная), в которой есть Behaviors.receive
  val system = ActorSystem(Dino.greet, "system1")

  // передача сообщения  Dino
  system ! HelloDino("John")
  system.terminate()

  case class HelloDino(name: String) // это осообщение

  // это объект-фабрика для актора  - тип - HelloDino
  object Dino {
    def greet = Behaviors.receive[HelloDino] {
      case (context, HelloDino(name)) =>
        context.log.info("Hello, my name is {}", name)
        Behaviors.same

    }
  }

}

object RequestResponse extends App {
  val system = ActorSystem(create2, "system1")

  system ! Start //передаем сообщение в create2
  system.terminate()


  trait Command

  case class DinoRequest(name: String, ref: ActorRef[DinoResponse]) extends Command

  case class DinoResponse(name: String) extends Command

  case object Start extends Command

  //1.1.
  def create: Behavior[DinoResponse] = Behaviors.setup { context =>
    context.log.info("new dinosaur!")
    val dino = context.spawn(Request(), "dino")
    val response = context.spawn(Response(), "response")
    dino ! DinoRequest("John", response)
    Behaviors.same
  }

  // Без setup и с одним context.spawn
  def create2: Behavior[Command] = Behaviors.receivePartial {
    // 1. получили Start - создали актор типа Request() и передали сообщение  DinoRequest с ссылкой на себя
    case (context, Start) =>
      context.log.info("new dinosaur!")
      val dino = context.spawn(Request(), "dino")
      dino ! DinoRequest("John", context.self)
      Behaviors.same
    //  3. получили обратно DinoResponse - напечатали сообщение из дугого аткора
    case (context, DinoResponse(msg)) =>
      context.log.info("Hi, {} from another dino 2 !", msg)
      Behaviors.same


  }

  //2.1.
  object Request {
    def apply() = Behaviors.receive[DinoRequest] { (context, message) =>
      //  2. приняли сообщение DinoRequest и отправили актору сообщение DinoResponse c именем, которое получили
      message match {
        case DinoRequest(name, ref) =>
          context.log.info("Hi, I am {}", name)
          ref ! DinoResponse(name)
          Behaviors.same
      }
    }
  }

  //3.1.
  object Response {
    def apply() = Behaviors.receive[DinoResponse] { (context, message) =>
      message match {
        case DinoResponse(name) =>
          context.log.info("Hi, {} from another dino 1 !", name)
          Behaviors.same

      }
    }


  }

}