package geneticparallel

import java.util.concurrent.Exchanger
import javafx.concurrent.Task

import scalafx.application.Platform
import scalafx.scene.image.ImageView

object Coordinator {
  type Sister = Exchanger[List[Chromosome]]
  type Parent = Exchanger[Population]

  val SWAP_GEN_NUMBER: Int = 25
  val N_GENERATIONS: Int = 500

  val SCALING_FACTOR: Int = 30 / 2
}

class Coordinator(imageViews: Array[ImageView]) {
  def visualizePopulation(imageView: ImageView, population: Population): Unit = {
    val image = population.getImage(Coordinator.SCALING_FACTOR)

    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        imageView.setImage(image)
      }
    })
  }

  def runGeneticAlgorithm(students: Students, sister: Coordinator.Sister, parent: Coordinator.Parent, imageView: ImageView): Unit = {
    var population = Population.createPopulation(students)

    for (i <- 50 until Coordinator.N_GENERATIONS) {
      val (mean, stddev) = population.getScoreStats()
      println(i + ": " + mean + "\tstd: " + stddev)
      visualizePopulation(imageView, population)

      population = population.generation()

      if (i % Coordinator.SWAP_GEN_NUMBER == 0) {
        val sample = population.getSample()
        val newSample = sister.exchange(sample)

        println("Sent: " + new Population(students, sample).getScoreStats()._1)
        println("Recieved: " + new Population(students, newSample).getScoreStats()._1)

        population = population.receiveSample(newSample)
        println("SWAP")
      }
    }
  }

  def start(): Unit = {
    val exchangers = for (_ <- 0 until this.imageViews.length / 2) yield new Coordinator.Sister()
    val parentConnections = for (_ <- this.imageViews.indices) yield new Coordinator.Parent()

    val students = Students.createStudents()

    def create_thread(students: Students, sister: Coordinator.Sister, parent: Coordinator.Parent, iv: ImageView): Thread = {
      val task: Task[Unit] = new Task[Unit]() {
        override def call(): Unit = {
          runGeneticAlgorithm(students, sister, parent, iv)
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
