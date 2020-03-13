
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop, Terminated}
import akka.actor.typed.scaladsl.Behaviors


object AkkaLife extends App {
  val system: ActorSystem[Command] = ActorSystem(ParentActor(), "parent")
  system !  Begin

  system.terminate()
}

sealed trait  Command
case object Begin extends Command
case class HelloParent(childRef: ActorRef[ParentBehaviors]) extends Command

sealed trait ParentBehaviors
case class StartChild(name: String, ref: ActorRef[Command]) extends ParentBehaviors
case class StopChild(name: String) extends ParentBehaviors

object ParentActor {

  def apply(): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case Begin =>
        val parent = context.spawn(StartStopChildActor(), "c1")
        parent ! StartChild("child1", context.self)
        Behaviors.same
      case HelloParent(childRef) => context.log.info("hello from  {}", childRef)
        Behaviors.stopped

    }

  }
}

object StartStopChildActor {

  def apply(): Behavior[ParentBehaviors] = withChildren(Map())

  def withChildren(children: Map[String, ActorRef[Child]]): Behavior[ParentBehaviors] = Behaviors.receive[ParentBehaviors] {
    (context, message) =>
      message match {
        case StartChild(name, ref) => context.log.info("Starting child {}", name)
          val child = context.spawn(ChildActor(), name)
          withChildren(children + (name -> child))
          child ! Child(s"Hi, I am a $name ")
          ref ! HelloParent(context.self)
          Behaviors.same

        case StopChild(name) => context.log.info("Stopping  child {}", name)
          val childOption = children.get(name)
          childOption.foreach(childRef => context.stop(childRef))
          Behaviors.stopped
      }
  }.receiveSignal {
    case (context, PostStop) =>
      context.log.info("stopped")
      Behaviors.same
  }
}


sealed trait ChildBehaviors

case class Child(name: String) extends ChildBehaviors

object ChildActor {
  def apply(): Behavior[ChildBehaviors] = Behaviors.receive {
    (context, message) =>
      context.log.info(message.toString)
      Behaviors.same
  }


}

