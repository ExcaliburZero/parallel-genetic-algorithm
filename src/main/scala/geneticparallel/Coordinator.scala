package geneticparallel

import java.util.concurrent.Exchanger
import javafx.concurrent.Task

import scalafx.application.Platform
import scalafx.scene.image.ImageView

/**
  * Coordinator is an object that contains constants and type definitions used
  * by the Coordinator class.
  */
object Coordinator {
  /**
    * A Sibling is an Exchanger for exchanging a set of chromosomes between two
    * sibling task threads.
    */
  type Sibling = Exchanger[List[Chromosome]]
  type Parent = Exchanger[Population]

  /**
    * The number of generations in between each sibling chromosome swap.
    */
  val SWAP_GEN_NUMBER: Int = 25

  /**
    * The number of generations that each task thread will run the genetic
    * algorithm for.
    */
  val N_GENERATIONS: Int = 500

  /**
    * The size of a chromosome square in the visualization.
    */
  val SCALING_FACTOR: Int = 15
}

/**
  * A Coordinator is an object that manages a set of task threads that run a
  * genetic algorithm and pass solutions between each other.
  *
  * Each task thread has a sibling that they swap some of their solutions with
  * every few generations.
  */
class Coordinator(imageViews: Array[ImageView]) {
  /**
    * Visualizes the given chromosome population in the given ImageView pane.
    *
    * @param imageView The ImageView pane to visualize the population in.
    * @param population The population of chromosomes to visualize.
    */
  private def visualizePopulation(imageView: ImageView, population: Population): Unit = {
    val image = population.getImage(Coordinator.SCALING_FACTOR)

    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        imageView.setImage(image)
      }
    })
  }

  /**
    * Runs a genetic algorithm for a given number of generations, swapping some
    * chromosomes with the current task thread's sibling every few generations.
    *
    * Also visualizes the population at the end of each generation in the given
    * ImageView pane.
    *
    * @param students The student seating preferences that define the problem.
    * @param sibling The exchanger for the sibling task thread.
    * @param parent The exchanger for the parent task thread.
    * @param imageView The ImageView pane to visualize the population in.
    */
  private def runGeneticAlgorithm(students: Students, sibling: Coordinator.Sibling, parent: Coordinator.Parent, imageView: ImageView): Unit = {
    var population = Population.createPopulation(students)

    for (i <- 0 until Coordinator.N_GENERATIONS) {
      val (mean, stddev) = population.getScoreStats()
      println(i + ": " + mean + "\tstd: " + stddev)
      visualizePopulation(imageView, population)

      population = population.generation()

      if (i % Coordinator.SWAP_GEN_NUMBER == 0) {
        val sample = population.getSample()
        val newSample = sibling.exchange(sample)

        println("Sent: " + new Population(students, sample).getScoreStats()._1)
        println("Recieved: " + new Population(students, newSample).getScoreStats()._1)

        population = population.receiveSample(newSample)
        println("SWAP")
      }
    }
  }

  /**
    * Starts the genetic algorithm task threads.
    */
  def start(): Unit = {
    val exchangers = for (_ <- 0 until this.imageViews.length / 2) yield new Coordinator.Sibling()
    val parentConnections = for (_ <- this.imageViews.indices) yield new Coordinator.Parent()

    val students = Students.createStudents()

    def create_thread(students: Students, sibling: Coordinator.Sibling, parent: Coordinator.Parent, iv: ImageView): Thread = {
      val task: Task[Unit] = new Task[Unit]() {
        override def call(): Unit = {
          runGeneticAlgorithm(students, sibling, parent, iv)
        }
      }
      val th = new Thread(task)
      th.setDaemon(true)

      th
    }

    val threads = (
      for (i <- this.imageViews.indices)
        yield create_thread(students, exchangers(i / 2), parentConnections(i), this.imageViews(i))
      ).toArray

    for (th <- threads) {
      th.start()
    }
  }
}
