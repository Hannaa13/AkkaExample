package example1

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object SimpleActor extends App {

  //val system = ActorSystem(Dino(), "system1")
  //system ! HelloDino("John")
  //system.terminate()

  case class HelloDino(name: String)

  object Dino {
    def apply(): Behavior[HelloDino] = Behaviors.receive { (context, message) =>
      context.log.info("Hello, my name is {}", message.name)
      Behaviors.same

    }
  }

}

object RequestResponse extends App {
  val system = ActorSystem(create, "system1")

//  system.terminate()

//ПОЧЕМУ НЕ ПЕЧАТАЕТСЯ ОТВЕТ!!!!!!!

  case class DinoRequest(name: String, ref: ActorRef[DinoResponse])

  case class DinoResponse(name: String)


    def create: Behavior[DinoResponse] = Behaviors.setup { context =>
      context.log.info("Yeah, new dinosaur!")
      val dino = context.spawn(Request(), "dino")
      dino ! DinoRequest("John", context.self)
      Behaviors.same
    }


  object Request {
    def apply() = Behaviors.receive[DinoRequest] { (context, message) =>
      message match {
        case DinoRequest(name, ref) =>
          context.log.info("Hi, I am {}", name)
          ref ! DinoResponse(name)
          Behaviors.same
      }
    }
  }

  object Response {
    def apply() = Behaviors.receive[DinoResponse] { (context, message) =>
      message match {
        case DinoResponse(name) =>
        // ВОТ ЭТОТ !!!?????
          context.log.info("Hi, {} from another dino!", name)
          Behaviors.same

      }
    }
  }

}


