package mgobench.optimize.psoakka


/*
import akka.actor._
import akka.event.{Logging, LoggingAdapter}
import breeze.linalg.{DenseVector, sum}
import breeze.numerics.abs
import com.hungrylearner.pso.particle.EvaluatedPosition
import com.hungrylearner.pso.particle.breezedvd._
import com.hungrylearner.pso.swarm.CompletedType._
import com.hungrylearner.pso.swarm.Report.{Progress, ProgressReport}
import com.hungrylearner.pso.swarm.TerminateCriteriaStatus.TerminateCriteriaStatus
import com.hungrylearner.pso.swarm._
import mgobench.optimize.{GradientDescent, Optimization}
import mgobench.problem.Problem
import mgobench.result.Result

import scala.util.Random



/**
  * WARNING : this particle swarm does not work with CocoProblem (coco suite is not parallelized - locking fails)
  *
  * @param iterations
  * @param particles
  */
case class BasicPSOAkka(
                     iterations: Int,
                     particles: Int
                   ) extends Optimization {

  override def optimize(problem: Problem): Result = BasicPSOAkka.optimize(this,problem)

  override def name: String = "BasicPSOAkka-"+iterations+"-"+particles

}



object BasicPSOAkka {


  /**
    * Global shared problem
    * FIXME cannot work as BasicPSO object is created independantly of a Problem
    *  -> cannot ensure consistent concurrent access to cocoJNI here ?
    */
  var currentProblem = Problem.emptyProblem

  /**
    * does not solve the problem in argument but the shared global problem, so the lock with Coco_evaluate actually works
    * @param basicPSO
    * @param problem
    * @return
    */
  def optimize(basicPSO: BasicPSOAkka, problem: Problem): Result = {

    val system = ActorSystem("basic-pso")
    //private val Logger = Logging.getLogger(system, this)
    //Logger.debug( "Simulation begin")

    val runPso: ActorRef = system.actorOf( Props( classOf[BasicPSOSimulator], basicPSO.iterations,basicPSO.particles,(x:Vector[Double])=>problem.fitness(x)(0)),  "runPso")
    //system.whenTerminated

    Result.empty
  }


  case object Initialize

  def makeLocalSwarmConfig(iterations: Int,
                           particles: Int,
                           system: ActorSystem,
                           fitness: Vector[Double]=>Double
                          ): LocalSwarmConfig[Double,PositionVector] = {

    val positionDimension = 1     // The number of items in a position vector.
    val positionBounds = Array[(Double,Double)]( (0,100), (0,10))
    val particleSpaceContext = ParticleSpaceDVD.ParticleSpaceContext(
      initialPosition = (dim: Int, particleIndex: Int) => new ParticlePosition(
        DenseVector.tabulate[Double]( dim) (i => boundedRandom( positionBounds(i))), positionBounds,fitness),
      positionBounds,
      initialHistory = (dim: Int) => DenseVector.fill[Double]( dim) (0.0)
    )

    val velocityBounds = (-5.0, 5.0)
    val randomFunction = math.random _
    val kinematicContext = KinematicParticleSpaceDVD.KinematicContext(
      initialVelocity = (dim: Int, particleIndex: Int) => DenseVector.fill[Double]( dim) (boundedRandom( velocityBounds)),
      velocityBounds = velocityBounds,
      inertiaWeight = (i: Int) => {i.toDouble / iterations.toDouble * 10.0 + 0.25},
      phiP = 0.25,
      phiG = 0.25,
      random = randomFunction,
      system = system
    )

    val particleContext = ParticleDVD.ParticleContext( positionDimension, particleSpaceContext, kinematicContext)

    // Create an "actor-in-a-box"
    //val inbox = Inbox.create(system)

    val particleCount = particles
    val simulationContext = SimulationContext( iterations, system)
    def makeParticle( swarmIndex: Int, particleIndex: Int, particleCount: Int) = {
      val sleepTime = (new Random).nextInt(100)
      println(s"Particle $particleIndex sleeping $sleepTime")
      java.lang.Thread.sleep(sleepTime)
      new ParticleDVD(
        simulationContext, particleContext, particleIndex)
    }

    new LocalSwarmConfig[Double,PositionVector]( particleCount, makeParticle, simulationContext)
  }

  private def boundedRandom( bounds: (Double,Double)): Double = {
    val range = bounds._2 - bounds._1
    math.random * range + bounds._1
  }


  class BasicPSOSimulator(iterations: Int,particles: Int,fitness: Vector[Double]=>Double) extends Actor with ActorLogging {
    //( iterations: Int)

    //  private val Logger = Logging.getLogger(system, this)

    var swarm: ActorRef = _
    //val swarmAround = SwarmAround( iterations / 4)
    val swarmAround = SwarmAround( iterations)

    override def preStart(): Unit = {
      log.debug( "Simulation.preStart")
      self ! Initialize
    }

    def receive = {
      case Initialize => initializePso
      case ProgressReport( completedType: CompletedType,
      childIndex: Int,
      evaluatedPosition: EvaluatedPosition[_,_],
      iteration: Int,
      progress: Progress,
      terminateCriteriaStatus: TerminateCriteriaStatus ) =>  onReport( completedType, childIndex, evaluatedPosition, iteration, progress, terminateCriteriaStatus)
      case Terminated( child) =>
        log.info( s"LocalSwarmActor '${child.path.name}' Terminated ")
        context.system.terminate() //shutdown()

      case unknownMessage: AnyRef => log.error( "RunPso.receive: Unknown message {}", unknownMessage)
    }

    def initializePso = {
      log.debug( "BasicPSOSimulator.initializePso")

      if( swarm == null) {
        val swarmConfig = makeLocalSwarmConfig( iterations,particles, context.system,fitness)

        val localSwarmIntelligenceFactory = ( childIndex: Int, context: ActorContext) =>
          new BasicLocalSwarmIntelligence[Double,PositionVector]( swarmConfig, childIndex, context)

        val props = Props(classOf[LocalSwarmActor[Double,PositionVector]], localSwarmIntelligenceFactory, 0)
        swarm = context.actorOf(props,  "localSwarm1")
        context.watch( swarm) // watch for child Terminated
        swarm ! swarmAround
      } else {
        log.error( "Simulation.initializePso swarm already initialized!")
      }
    }

    def onReport( completedType: CompletedType,
                  childIndex: Int,
                  evaluatedPosition: EvaluatedPosition[_,_],
                  iteration: Int,
                  progress: Progress,
                  terminateCriteriaStatus: TerminateCriteriaStatus) = {

      if(iteration == iterations) {
        swarm ! PoisonPill
        self ! PoisonPill
      }


      evaluatedPosition match {
        case EvaluatedPosition( position: ParticlePosition, isBest: Boolean) =>
          log.info(  s"++++++++++++++++++++++++++++++++ ProgressReport ${iteration} ${position.value(0)}")
          if( completedType != SwarmingCompleted)
            swarm ! swarmAround
          else {
            // Stop the child. Since we're watching, We'll receive a Terminated when child is stopped.
            context.stop( swarm)
          }


        case _ =>
      }


    }
  }




}

*/
