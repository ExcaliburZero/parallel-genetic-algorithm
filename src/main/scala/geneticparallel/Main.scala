package geneticparallel

import scalafx.application.JFXApp
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Label
import scalafx.scene.image.ImageView
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Font
import scalafx.scene.paint.Color._

/**
  * The Main object contains the GUI for the program.
  */
object Main extends JFXApp {
  /**
    * The number of threads, and thus cells, to use.
    */
  val N_THREADS: Int = 64

  /**
    * The max number rows of cells. After this number is exceeded, cells are
    * wrapped into a new column.
    */
  val MAX_ROW_CELLS: Int = 8

  /**
    * The number of rows of cells.
    */
  val N_H_SETS: Int = Math.ceil(N_THREADS / MAX_ROW_CELLS.toDouble).toInt

  /**
    * The number of columns of cells.
    */
  val N_V_SETS: Int = if (N_THREADS < MAX_ROW_CELLS) N_THREADS else MAX_ROW_CELLS

  /**
    * The gap between cell columns.
    */
  val H_GAP: Int = 10

  /**
    * The gap between cell rows.
    */
  val V_GAP: Int = 10

  /**
    * The approximate width of the cell grid.
    */
  val WIDTH: Int = 1550 - H_GAP * N_H_SETS

  /**
    * The approximate height of the cell grid.
    */
  val HEIGHT: Int = 1450 - V_GAP * N_V_SETS

  /**
    * The font size to use for the results display box.
    */
  val FONT_SIZE: Int = 36

  var imageViews: Array[ImageView] = _
  var chromosomeView: ImageView = _
  var resultsLabel: Label = _
  var mainScene: Scene = _

  /**
    * Creates the PrimaryStage of the GUI.
    *
    * @return The created primary stage.
    */
  def createStage(): JFXApp.PrimaryStage = {
    this.imageViews = (for (_ <- 0 until N_THREADS) yield new ImageView() {smooth = false}).toArray

    val mainGrid = new GridPane()

    val imageGrid = new GridPane {
      hgap = H_GAP
      vgap = V_GAP
      padding = Insets(18)
    }

    val resultsGrid = new GridPane {
      hgap = H_GAP
      vgap = V_GAP
      padding = Insets(18)
    }

    mainGrid.add(imageGrid, 0, 0)
    mainGrid.add(resultsGrid, 0, 1)

    for (i <- imageViews.indices) yield imageGrid.add(this.imageViews(i), i / MAX_ROW_CELLS, i % MAX_ROW_CELLS)

    resultsLabel = new Label("Calculating ...")
    resultsLabel.setFont(new Font(FONT_SIZE))
    resultsGrid.add(resultsLabel, 0, 0)

    chromosomeView = new ImageView() {smooth = false}
    resultsGrid.add(chromosomeView, 1, 0)

    this.mainScene = new Scene {
      fill = LightGreen
      content = mainGrid
    }

    new JFXApp.PrimaryStage {
      title.value = "Parallel Genetic Algorithm"
      width = 600
      height = 450
      scene = mainScene
    }
  }

  stage = createStage()

  val coordinator = new Coordinator(this.imageViews, this.chromosomeView, this.resultsLabel)
  coordinator.start()
}
