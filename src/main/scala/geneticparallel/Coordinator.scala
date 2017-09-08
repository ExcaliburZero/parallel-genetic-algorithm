package geneticparallel

import java.util.concurrent.Exchanger
import javafx.concurrent.Task

import scalafx.application.Platform
import scalafx.scene.image.ImageView

object Coordinator {
  type Sister = Exchanger[Array[Chromosome]]
  type Parent = Exchanger[Population]

  val SWAP_GEN_NUMBER = 25
}

class Coordinator(imageViews: Array[ImageView]) {
  def visualizePopulation(imageView: ImageView, population: Population): Unit = {
    val scalingFactor = 100
    val image = population.getImage(scalingFactor)

    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        imageView.setImage(image)
      }
    })
  }

  def runGeneticAlgorithm(sister: Coordinator.Sister, parent: Coordinator.Parent, imageView: ImageView): Unit = {
    val students = Students.createStudents()
    var population = Population.createPopulation(students)

    val n_generations = 500

    for (i <- 50 until n_generations) {
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
    val exchangers = for (_ <- 0 until this.imageViews.length / 2) yield new Exchanger[Array[Chromosome]]()
    val parentConnections = for (_ <- this.imageViews.indices) yield new Exchanger[Population]()

    def create_thread(sister: Coordinator.Sister, parent: Coordinator.Parent, iv: ImageView): Thread = {
      val task: Task[Unit] = new Task[Unit]() {
        override def call(): Unit = {
          runGeneticAlgorithm(sister, parent, iv)
        }
      }
      val th = new Thread(task)
      th.setDaemon(true)

      th
    }

    val threads = (
      for (i <- this.imageViews.indices)
        yield create_thread(exchangers(i / 2), parentConnections(i), this.imageViews(i))
      ).toArray

    for (th <- threads) {
      th.start()
    }
  }
}
