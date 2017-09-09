package geneticparallel

import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.image.ImageView
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._

object Main extends JFXApp {
  val MAX_ROW_CELLS: Int = 8

  val N_THREADS: Int = 32
  val N_H_SETS: Int = Math.ceil(N_THREADS / MAX_ROW_CELLS.toDouble).toInt
  val N_V_SETS: Int = if (N_THREADS < MAX_ROW_CELLS) N_THREADS else MAX_ROW_CELLS

  val H_GAP: Int = 10
  val V_GAP: Int = 10

  val WIDTH: Int = 1550 - H_GAP * N_H_SETS
  val HEIGHT: Int = 1450 - V_GAP * N_V_SETS

  var imageViews: Array[ImageView] = _
  var mainScene: Scene = _

  def createStage(): JFXApp.PrimaryStage = {
    this.imageViews = (for (_ <- 0 until N_THREADS) yield new ImageView() {smooth = false}).toArray

    val imageGrid = new GridPane {
      hgap = H_GAP
      vgap = V_GAP
      padding = Insets(18)
    }

    for (i <- imageViews.indices) yield imageGrid.add(this.imageViews(i), i / MAX_ROW_CELLS, i % MAX_ROW_CELLS)

    this.mainScene = new Scene {
      fill = LightGreen
      content = imageGrid
    }

    new JFXApp.PrimaryStage {
      title.value = "Parallel Genetic Algorithm"
      width = 600
      height = 450
      scene = mainScene
    }
  }

  stage = createStage()

  val coordinator = new Coordinator(this.imageViews)
  coordinator.start()
}
