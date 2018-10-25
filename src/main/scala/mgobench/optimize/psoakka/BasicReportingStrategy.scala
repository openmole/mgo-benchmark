package mgobench.optimize.psoakka

import akka.actor.ActorRef
import com.hungrylearner.pso.swarm.{ParentReporter, PeriodicLocalReporting}

class BasicReportingStrategy[F,P]( override val parent: ActorRef)
  extends ParentReporter[F,P]
    with PeriodicLocalReporting[F,P]

