package geneticparallel

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.application.JFXApp

object Main extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Parallel Genetic Algorithm"
    width = 600
    height = 450
    scene = new Scene {
      fill = LightGreen
      content = new Rectangle {
        x = 25
        y = 40
        width = 100
        height = 100
        fill <== when(hover) choose Green otherwise Red
      }
    }
  }

  val students = Students.createStudents()
  var population = Population.createPopulation(students)

  val n_generations = 500

  println("Add mutation!")

  for (i <- 0 to n_generations) {
    println(i + ": " + population.getAverageScore())
    population = population.generation()
  }
}
