package geneticparallel

import java.util.concurrent.Exchanger
import javafx.concurrent.Task

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
  val N_GENERATIONS: Int = 100

  /**
    * The size of a chromosome square in the visualization.
    */
  val SCALING_FACTOR: Int = 15
}

/**
  * A Coordinator is an object that manages a set of task threads that run a
  * genetic algorithm and pass solutions between each other.
  *
  * Each task thread swaps some of their solutions with another thread every
  * few generations.
  */
class Coordinator(imageViews: Array[ImageView], resultsLabel: Label) {
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
  private def displayTopScore(bestScore: Double): Unit = {
    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        resultsLabel.setText("Best: %.3f".format(bestScore))
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
      println(i + ": %.3f\tstd: %.3f".format(mean, stddev))
      visualizePopulation(imageView, population)

      population = population.generation()

      if (i % Coordinator.SWAP_GEN_NUMBER == 0) {
        val sample = population.getSample()
        val newSample = swapper.exchange(sample)

        println("Sent: " + new Population(students, sample).getScoreStats()._1)
        println("Recieved: " + new Population(students, newSample).getScoreStats()._1)

        population = population.receiveSample(newSample)
        println("SWAP")
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
    * @return The parent, results monioring thread.
    */
  private def monitorResults(students: Students, children: List[Coordinator.Parent]): Thread = {
    val task: Task[Unit] = new Task[Unit]() {
      override def call(): Unit = {
        val bestChromosomes = for (child <- children) yield child.exchange(null)

        val chromosomeScores = for (c <- bestChromosomes) yield students.getScore(c)

        val bestScore = chromosomeScores.max

        displayTopScore(bestScore)
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

    def create_thread(students: Students, swapper: Coordinator.Swapper, parent: Coordinator.Parent, iv: ImageView): Thread = {
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
        yield create_thread(students, exchanger, parentConnections(i), this.imageViews(i))
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
