package geneticparallel

import scalafx.application.{JFXApp, Platform}
import javafx.concurrent.Task
import scalafx.scene.Scene
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.paint.Color._

object Main extends JFXApp {
  var imageView: ImageView = _
  var mainScene: Scene = _

  def createStage(): JFXApp.PrimaryStage = {
    this.imageView = new ImageView() {
      x = 25
      y = 40
      smooth = false
    }

    this.mainScene = new Scene {
      fill = LightGreen
      content = imageView
    }

    new JFXApp.PrimaryStage {
      title.value = "Parallel Genetic Algorithm"
      width = 600
      height = 450
      scene = mainScene
    }
  }

  def visualizePopulation(population: Population): Unit = {
    val scalingFactor = 100
    val image = population.getImage(scalingFactor)

    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        imageView.setImage(image)
      }
    })
  }

  def runGeneticAlgorithm(imageView: ImageView): Unit = {
    val students = Students.createStudents()
    var population = Population.createPopulation(students)

    val n_generations = 500

    for (i <- 50 until n_generations) {
      val (mean, stddev) = population.getScoreStats()
      println(i + ": " + mean + "\tstd: " + stddev)
      visualizePopulation(population)

      population = population.generation()
    }
  }

  stage = createStage()

  val task: Task[Unit] = new Task[Unit]() {
    override def call(): Unit = {
      runGeneticAlgorithm(imageView)
    }
  }
  val th: Thread = new Thread(task)
  th.setDaemon(true)
  th.start()
}
