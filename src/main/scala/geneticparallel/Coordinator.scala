package geneticparallel

import java.util.concurrent.{Exchanger, Semaphore, TimeUnit}
import javafx.concurrent.Task

import scala.concurrent.TimeoutException
import scalafx.application.Platform
import scalafx.scene.control.Label
import scalafx.scene.image.ImageView

/**
  * Coordinator is an object that contains constants and type definitions used
  * by the Coordinator class.
  */
object Coordinator {
  type Swapper = Exchanger[List[Chromosome]]
  type Parent = Exchanger[Chromosome]

  /**
    * The number of generations in between each chromosome swap.
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

  /**
    * The time in milliseconds to wait for a swap.
    */
  val SWAP_TIME_WAIT: Int = 750
}

/**
  * A Coordinator is an object that manages a set of task threads that run a
  * genetic algorithm and pass solutions between each other.
  *
  * Each task thread swaps some of their solutions with another thread every
  * few generations.
  */
class Coordinator(imageViews: Array[ImageView], chromosomeView: ImageView, resultsLabel: Label) {
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
    * Displays the given top chromosome score in the GUI.
    *
    * @param bestScore The score to display.
    */
  private def displayTopScore(students: Students, bestChromosome: Chromosome, bestScore: Double): Unit = {
    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        resultsLabel.setText("Best: %.3f".format(bestScore))
        chromosomeView.setImage(bestChromosome.getImage(students, 16))
      }
    })
  }

  /**
    * Runs a genetic algorithm for a given number of generations, swapping some
    * chromosomes with another task thread every few generations.
    *
    * Also visualizes the population at the end of each generation in the given
    * ImageView pane.
    *
    * @param students The student seating preferences that define the problem.
    * @param swapper The exchanger for swaping chromosomes.
    * @param parent The exchanger for the parent task thread.
    * @param imageView The ImageView pane to visualize the population in.
    */
  private def runGeneticAlgorithm(students: Students, swapper: Coordinator.Swapper, parent: Coordinator.Parent, imageView: ImageView): Unit = {
    var population = Population.createPopulation(students)

    for (i <- 0 until Coordinator.N_GENERATIONS) {
      val (mean, stddev) = population.getScoreStats()
      println(i + ":\t%.3f\t\tstd: %.3f".format(mean, stddev))
      visualizePopulation(imageView, population)

      population = population.generation()

      if (i % Coordinator.SWAP_GEN_NUMBER == 0) {
        val sample = population.getSample()

        val newSample = try {
          val n = swapper.exchange(sample, Coordinator.SWAP_TIME_WAIT, TimeUnit.MILLISECONDS)
          println("SWAP")
          n
        } catch {
          case _: TimeoutException =>
            println("SKIPPED")
            sample
        }

        population = population.receiveSample(newSample)
      }
    }

    val chromosomeScores = for (c <- population.chromosomes) yield students.getScore(c)
    val bestChromosome = population.chromosomes(
      chromosomeScores.indices.maxBy(chromosomeScores)
    )

    parent.exchange(bestChromosome)
  }

  /**
    * Creates a thread to monitor the child threads and display the score of
    * the best chromosome after all the child threads have finished running.
    *
    * @param students The student preferences that define the problem.
    * @param children The exchangers for each of the child threads' best
    * chromosomes.
    * @return The parent, results monitoring thread.
    */
  private def monitorResults(students: Students, children: List[Coordinator.Parent]): Thread = {
    val task: Task[Unit] = new Task[Unit]() {
      override def call(): Unit = {
        val bestChromosomes = for (child <- children) yield child.exchange(null)

        val chromosomeScores = for (c <- bestChromosomes) yield students.getScore(c)

        val best = bestChromosomes(chromosomeScores.indices.maxBy(chromosomeScores))

        val bestScore = chromosomeScores.max

        displayTopScore(students, best, bestScore)
      }
    }
    val th = new Thread(task)
    th.setDaemon(true)

    th
  }

  /**
    * Creates the child threads to run the genetic algorithm instances.
    *
    * @param students The student preferences that define the problem.
    * @param parentConnections The exchangers for child threads and the parent
    * thread.
    * @return The child threads.
    */
  private def createChildThreads(students: Students, parentConnections: List[Coordinator.Parent]): IndexedSeq[Thread] = {
    val exchanger = new Coordinator.Swapper

    def createThread(students: Students, swapper: Coordinator.Swapper, parent: Coordinator.Parent, iv: ImageView): Thread = {
      val task: Task[Unit] = new Task[Unit]() {
        override def call(): Unit = {
          runGeneticAlgorithm(students, swapper, parent, iv)
        }
      }
      val th = new Thread(task)
      th.setDaemon(true)

      th
    }

    for (i <- this.imageViews.indices)
        yield createThread(students, exchanger, parentConnections(i), this.imageViews(i))
  }

  /**
    * Starts the genetic algorithm task threads.
    */
  def start(): Unit = {
    val parentConnections = (for (_ <- this.imageViews.indices) yield new Coordinator.Parent()).toList

    val students = Students.createStudents()

    val childThreads = createChildThreads(students, parentConnections)
    for (th <- childThreads) yield th.start()

    val parent = monitorResults(students, parentConnections)
    parent.start()
  }
}
