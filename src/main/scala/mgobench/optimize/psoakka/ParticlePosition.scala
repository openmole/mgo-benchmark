package mgobench.optimize.psoakka

import breeze.linalg.sum
import breeze.numerics.abs
import com.hungrylearner.pso.particle.breezedvd.MutablePositionDVD


class ParticlePosition(
                        /**
                          * From where the particle starts
                          */
                        initialPosition: PositionVector,

                        /**
                          * Authorized space for the particle
                          */
                        bounds: Array[(Double,Double)],

                        /**
                          * Monoobjective fitness in position
                          */
                        fitness: Vector[Double]=>Double
                      ) extends MutablePositionDVD( initialPosition, bounds) {

  override def evaluateFitness( v: PositionVector, iteration: Int): Double = fitness(v.data.toVector)

  /**
    * EnforceConstraints is called by addVelocity() after adding the velocity and before calling evaluateFitness.
    * This is an opportunity to change the particles position if it's not within custom constraints. Constraints
    * can include particle boundaries or invalid combinations of values within the position vector.
    */
  override def enforceConstraints() = {
    // super.enforceConstraints()
    // TODO: enforce bounds here or in super!.
  }

  /**
    * The superclass's constructor copies the position before using it.
    * @return a deep copy.
    */
  override def copy: ParticlePosition = new ParticlePosition( value, bounds,fitness)
}