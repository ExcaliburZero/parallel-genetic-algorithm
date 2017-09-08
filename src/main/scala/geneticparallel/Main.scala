package geneticparallel

import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.image.ImageView
import scalafx.scene.layout.GridPane
import scalafx.scene.paint.Color._

object Main extends JFXApp {
  val N_THREADS: Int = 2

  var imageViews: Array[ImageView] = _
  var mainScene: Scene = _

  def createStage(): JFXApp.PrimaryStage = {
    this.imageViews = (for (_ <- 0 until N_THREADS) yield new ImageView() {smooth = false}).toArray

    val imageGrid = new GridPane {
      hgap = 40
      vgap = 20
      padding = Insets(18)
    }

    for (i <- imageViews.indices) yield imageGrid.add(this.imageViews(i), 0, i)

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
