package geneticparallel

import scala.collection.mutable.ArrayBuffer
import scalafx.scene.image.{Image, WritableImage}
import scalafx.scene.paint.Color

/**
  * Chromosome is an object that contains constants and type definitions used
  * by the Chromosome class.
  */
object Chromosome {
  /**
    * The number of chromosomes to exchange between simulation threads at each
    * swap.
    */
  val NUM_SWAP: Int = Students.N_STUDENTS / 3

  /**
    * The 1/n chance that a given chromosome will be mutated.
    */
  val MUTATION_CHANCE: Int = 10

  /**
    * The average number of mutations that a mutated chromosome will undergo.
    */
  val AVG_NUM_MUTATIONS: Double = Students.N_STUDENTS / 10.0

  /**
    * Creates a randomized chromosome.
    *
    * This is used when creating the initial chromosomes for a simulation.
    *
    * @return A randomly generated chromosome.
    */
  def createRandom(): Chromosome = {
    val indexes = (0 until Students.N_STUDENTS).toList
    val list = scala.util.Random.shuffle(indexes)

    new Chromosome(list)
  }
}

/**
  * A Chromosome is a representation of a possible solution to the problem.
  *
  * For this problem, each chromosome is a list of the students that are in
  * each seat in the classroom.
  */
case class Chromosome(private val seating: List[Int]) {
  /**
    * Returns the id of the student that is located in the given seat.
    *
    * @param x The column of the seat.
    * @param y The row of the seat.
    * @return The id of the student in the seat.
    */
  def getStudentIndex(x: Int, y: Int): Int = {
    val index = y * Students.N_COLUMNS + x
    seating(index)
  }

  /**
    * Creates a new child chromosome based on this chromosome and the given
    * chromosome.
    *
    * Parts of the child chromosome also have a chance of being mutated.
    *
    * @param other The other parent chromosome.
    * @return The newly born child chromosome.
    */
  def mate(other: Chromosome): Chromosome = {
    val changeIndices = scala.util.Random.shuffle((0 until Students.N_STUDENTS).toList)
      .take(Chromosome.NUM_SWAP)

    val newArray = this.seating.toArray
    val swapped = ArrayBuffer[Int]()
    for (i <- changeIndices) {
      if (!swapped.contains(i)) {
        val newSeat = other.seating(i)
        val prevSeat = newArray(i)

        val swapIndex = newArray.indexOf(newSeat)

        newArray.update(i, newSeat)
        newArray.update(swapIndex, prevSeat)

        swapped.append(i)
        swapped.append(swapIndex)
      }
    }

    if (scala.util.Random.nextInt(100) < Chromosome.MUTATION_CHANCE) {
      mutate(newArray)
    }

    new Chromosome(newArray.toList)
  }

  /**
    * Mutates the given chromosome by swapping a random number of the students'
    * seating positions.
    *
    * Note that this method works by actually mutating the contents of the
    * passed in array.
    *
    * @param newArray The array representing the chromosome.
    */
  private def mutate(newArray: Array[Int]): Unit = {
    val num_mutations = (scala.util.Random.nextGaussian() * Chromosome.AVG_NUM_MUTATIONS).toInt

    val mutateIndices = scala.util.Random.shuffle((0 until Students.N_STUDENTS).toList)
      .take(num_mutations)

    for (i <- mutateIndices) {
      val other_i = scala.util.Random.nextInt(Students.N_STUDENTS)

      val prev_v = newArray(i)
      val new_v = newArray(other_i)

      newArray.update(i, new_v)
      newArray.update(other_i, prev_v)
    }
  }
}

/**
  * Population is an object that contains constants and type definitions used
  * by the Population class.
  */
object Population {
  /**
    * The number of chromosomes per row.
    */
  val CHROM_ROWS: Int = Main.HEIGHT / Main.N_V_SETS / Coordinator.SCALING_FACTOR

  /**
    * The number of chromosomes per column.
    */
  val CHROM_COLUMNS: Int = Main.WIDTH / Main.N_H_SETS / Coordinator.SCALING_FACTOR

  /**
    * The number of chromosomes in a population.
    */
  val NUM_CHROMOSOMES: Int = CHROM_ROWS * CHROM_COLUMNS

  /**
    * The number of chromosomes to remove each generation.
    */
  val NUM_REMOVE: Int = NUM_CHROMOSOMES / 2

  /**
    * The number of chromosomes to keep each generation.
    */
  val NUM_KEEP: Int = NUM_CHROMOSOMES - NUM_REMOVE

  /**
    * The number of chromosomes to pass in a swap.
    */
  val SAMPLE_SIZE: Int = (NUM_CHROMOSOMES / 10.0).toInt

  /**
    * Creates a randomized population of chromosomes.
    *
    * This is used when creating the initial populaion for a simulation.
    *
    * @return A randomly generated population.
    */
  def createPopulation(students: Students): Population = {
    val chromosomes = (for (_ <- 0 until NUM_CHROMOSOMES) yield Chromosome.createRandom()).toList

    new Population(students, chromosomes)
  }
}

case class Population(students: Students, chromosomes: List[Chromosome]) {
  def generation(): Population = {
    val scores = for (c <- this.chromosomes) yield students.getScore(c)
    val keepIndices = chromosomes.indices.sortBy(scores).reverse.take(Population.NUM_KEEP)

    val newChromosomes = Array.fill[Chromosome](Population.NUM_CHROMOSOMES)(null)

    for (i <- keepIndices.indices) {
      val newI = keepIndices(i)

      newChromosomes.update(i, this.chromosomes(newI))
    }

    for (i <- Population.NUM_KEEP until Population.NUM_CHROMOSOMES) {
      val parents = scala.util.Random.shuffle((0 until Population.NUM_KEEP).toList)
        .take(2).toArray
      val c = chromosomes(parents(0)).mate(chromosomes(parents(1)))

      newChromosomes.update(i, c)
    }

    new Population(students, newChromosomes.toList)
  }

  def getSample(): List[Chromosome] = {
    this.chromosomes.take(Population.SAMPLE_SIZE)
  }

  def receiveSample(sample: List[Chromosome]): Population = {
    val newChromosomes = this.chromosomes.toArray

    for (i <- sample.indices) {
      newChromosomes.update(i, sample(i))
    }

    new Population(students, newChromosomes.toList)
  }

  def getScoreStats(): (Double, Double) = {
    val scores = for (c <- chromosomes) yield students.getScore(c)
    val count = scores.length

    val mean = scores.sum / scores.length.toDouble
    val devs = scores.map(score => (score - mean) * (score - mean))
    val stddev = Math.sqrt(devs.sum / (count - 1))

    (mean, stddev)
  }

  def getImage(scalingFactor: Int): Image = {
    val height = Population.CHROM_ROWS * scalingFactor
    val width = Population.CHROM_COLUMNS * scalingFactor

    val image = new WritableImage(width, height)
    val pw = image.getPixelWriter

    val shuffledChromosomes = scala.util.Random.shuffle(chromosomes.toIterable).toArray

    for (x <- 0 until Population.CHROM_COLUMNS; y <- 0 until Population.CHROM_ROWS) {
      val index = y * Population.CHROM_COLUMNS + x

      val c = shuffledChromosomes(index)
      val score = students.getScore(c)

      val color = pickColor(score)

      for (x2 <- 0 until scalingFactor; y2 <- 0 until scalingFactor) {
        val i1 = x * scalingFactor + x2
        val i2 = y * scalingFactor + y2
        pw.setColor(i1, i2, color)
      }
    }

    image
  }

  private def pickColor(value: Double): Color = {
    val shift = 0.5
    val scalingFactor = 6.0 * 8
    val scaledValue = (value - shift) * scalingFactor

    val sigmoidValue = 1 / (1 + Math.exp(-1 * scaledValue))

    val hue = 145.44
    val brightness = 0.8
    val saturation = sigmoidValue

    Color.hsb(hue, saturation, brightness)
  }
}

object Students {
  val N_ROWS = 10
  val N_COLUMNS = 10

  val N_STUDENTS: Int = N_ROWS * N_COLUMNS

  val MAX_LIKE: Int = 10
  val MAX_SCORE: Double = MAX_LIKE * (
    (4 * 2) +
      (((N_COLUMNS + N_ROWS) * 2 - 4) * 3) +
      (N_COLUMNS - 2) * (N_ROWS - 2) * 4
    )

  def createStudents(): Students = {
    val preferences = for (s1 <- 0 until N_STUDENTS) yield
      (for (s2 <- 0 until N_STUDENTS) yield scala.util.Random.nextDouble() * MAX_LIKE).toArray

    new Students(preferences.toArray)
  }
}

case class Students(preferences: Array[Array[Double]]) {
  def getScore(chromosome: Chromosome): Double = {
    def getStudentScore(x: Int, y: Int): Double = {
      val student = chromosome.getStudentIndex(x, y)

      val neighborIndexes = getNeighborsIndexes(x, y)
      val neighborPreferences = for ((x, y) <- neighborIndexes) yield getPreference(x, y, student)

      neighborPreferences.sum
    }

    val scores = for (x <- 0 until Students.N_COLUMNS; y <- 0 until Students.N_ROWS) yield getStudentScore(x, y)

    scores.sum / Students.MAX_SCORE
  }

  private def getPreference(x: Int, y: Int, i: Int): Double = {
    val index = y * Students.N_COLUMNS + x
    preferences(index)(i)
  }

  private def getNeighborsIndexes(x: Int, y: Int): Array[(Int, Int)] = {
    val possibleNeighbors = List[(Int, Int)](
      (x + 1, y),
      (x - 1, y),
      (x, y + 1),
      (x, y - 1)
    )

    def isValidNeighbor(ab: (Int, Int)): Boolean = {
      ab match {
        case (a, b) => a >= 0 && b >= 0 && a < Students.N_COLUMNS && b < Students.N_ROWS
      }
    }

    val neighbors = possibleNeighbors.filter(isValidNeighbor)

    neighbors.toArray
  }
}
