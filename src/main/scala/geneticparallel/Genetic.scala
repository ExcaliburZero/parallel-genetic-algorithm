package geneticparallel

import java.util
import java.util.concurrent.ThreadLocalRandom

import scala.collection.JavaConversions._
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
  val MUTATION_CHANCE: Int = 2

  /**
    * The average number of mutations that a mutated chromosome will undergo.
    */
  val AVG_NUM_MUTATIONS: Double = Students.N_STUDENTS / 4.0

  /**
    * Creates a randomized chromosome.
    *
    * This is used when creating the initial chromosomes for a simulation.
    *
    * @return A randomly generated chromosome.
    */
  def createRandom(): Chromosome = {
    val indexes = (0 until Students.N_STUDENTS).toList
    val list: util.List[Int] = new util.ArrayList[Int](indexes)
    util.Collections.shuffle(list, ThreadLocalRandom.current())

    new Chromosome(list.toList)
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
    val list: util.List[Int] = new util.ArrayList[Int](0 until Students.N_STUDENTS)
    util.Collections.shuffle(list, ThreadLocalRandom.current())

    val changeIndices = list.take(Chromosome.NUM_SWAP)

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

    val random = ThreadLocalRandom.current()
    if (random.nextInt(100) < Chromosome.MUTATION_CHANCE) {
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
    val random = ThreadLocalRandom.current()
    val numMutations = (random.nextGaussian() * Chromosome.AVG_NUM_MUTATIONS).toInt

    val list = new util.ArrayList[Int](0 until Students.N_STUDENTS)
    util.Collections.shuffle(list, random)
    val mutateIndices = list.take(numMutations)

    for (i <- mutateIndices) {
      val other_i = random.nextInt(Students.N_STUDENTS)

      val prev_v = newArray(i)
      val new_v = newArray(other_i)

      newArray.update(i, new_v)
      newArray.update(other_i, prev_v)
    }
  }

  def getImage(students: Students, scalingFactor: Int): Image = {
    val height = Students.N_ROWS * scalingFactor
    val width = Students.N_COLUMNS * scalingFactor

    val image = new WritableImage(width, height)
    val pw = image.getPixelWriter

    for (x <- 0 until Students.N_COLUMNS; y <- 0 until Students.N_ROWS) {
      val index = getStudentIndex(x, y)

      val color = students.colors(index)

      for (x2 <- 0 until scalingFactor; y2 <- 0 until scalingFactor) {
        val i1 = x * scalingFactor + x2
        val i2 = y * scalingFactor + y2
        pw.setColor(i1, i2, color)
      }
    }

    image
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

  val APROX_MAX_SCORE: Double = 0.19
  val COLOR_CONST: Double = -math.log(1.0 / 9.0) / APROX_MAX_SCORE

  /**
    * Creates a randomized population of chromosomes.
    *
    * This is used when creating the initial population for a simulation.
    *
    * @return A randomly generated population.
    */
  def createPopulation(students: Students): Population = {
    val chromosomes = (for (_ <- 0 until NUM_CHROMOSOMES) yield Chromosome.createRandom()).toArray

    new Population(students, chromosomes.toList)
  }
}

/**
  * A Population is a set of solutions and the definition of a student seating
  * problem.
  */
case class Population(students: Students, chromosomes: List[Chromosome]) {
  /**
    * Runs a generation over the population, removing low scoring chromosomes,
    * and mating the remaining chromosomes to refill the population.
    *
    * @return The Population representing the next generation.
    */
  def generation(): Population = {
    val scores = for (c <- this.chromosomes) yield students.getScore(c)
    val keepIndices = chromosomes.indices.sortBy(scores).reverse.take(Population.NUM_KEEP)

    val newChromosomes = Array.fill[Chromosome](Population.NUM_CHROMOSOMES)(null)

    for (i <- keepIndices.indices) {
      val newI = keepIndices(i)

      newChromosomes.update(i, this.chromosomes(newI))
    }

    for (i <- Population.NUM_KEEP until Population.NUM_CHROMOSOMES) {

      val list: util.List[Int] = new util.ArrayList[Int](0 until Population.NUM_KEEP)
      util.Collections.shuffle(list, ThreadLocalRandom.current())
      val parents = list.take(2).toArray
      val c = chromosomes(parents(0)).mate(chromosomes(parents(1)))

      newChromosomes.update(i, c)
    }

    new Population(students, newChromosomes.toList)
  }

  /**
    * Returns a sample of the best chromosomes to send in a chromosome swap.
    *
    * @return The sample of chromosomes.
    */
  def getSample(): List[Chromosome] = {
    this.chromosomes.take(Population.SAMPLE_SIZE)
  }

  /**
    * Creates a new population with the given set of swapped chromosomes
    * transplanted into the current population.
    *
    * @param sample The sample of chromosomes to transplant.
    * @return The population with the new sample.
    */
  def receiveSample(sample: List[Chromosome]): Population = {
    val newChromosomes = this.chromosomes.toArray

    for (i <- sample.indices) {
      newChromosomes.update(i, sample(i))
    }

    new Population(students, newChromosomes.toList)
  }

  /**
    * Returns the mean and standard deviation of the chromosome scores.
    *
    * @return The mean and standard deviation of the chromosome scores.
    */
  def getScoreStats(): (Double, Double) = {
    val scores = for (c <- chromosomes) yield students.getScore(c)
    val count = scores.length

    val mean = scores.sum / scores.length.toDouble
    val devs = scores.map(score => (score - mean) * (score - mean))
    val stddev = Math.sqrt(devs.sum / (count - 1))

    (mean, stddev)
  }

  /**
    * Returns an image representing the Population's chromosomes' scores,
    * scaled using the given scaling factor.
    *
    * @param scalingFactor The pixel width and height for each chromosome
    * display.
    * @return The generated image.
    */
  def getImage(scalingFactor: Int): Image = {
    val height = Population.CHROM_ROWS * scalingFactor
    val width = Population.CHROM_COLUMNS * scalingFactor

    val image = new WritableImage(width, height)
    val pw = image.getPixelWriter

    val list: util.List[Chromosome] = new util.ArrayList[Chromosome](chromosomes)
    util.Collections.shuffle(list, ThreadLocalRandom.current())
    val shuffledChromosomes: Array[Chromosome] = list.toList.toArray

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

  /**
    * Returns a color to represent the given value.
    *
    * The given value should be between 0.0 and 1.0.
    *
    * @param value The value to get the color for.
    * @return The color to represent the given value.
    */
  private def pickColor(value: Double): Color = {
    val sigmoidValue = 1.0 / (1.0 + Math.exp(-1.0 * value * Population.COLOR_CONST))

    val hue = 145.44
    val brightness = 0.8
    val saturation = sigmoidValue

    Color.hsb(hue, saturation, brightness)
  }
}

/**
  * Students is an object that contains constants and type definitions used
  * by the Students class.
  */
object Students {
  /**
    * The number of rows of students.
    */
  val N_ROWS = 10

  /**
    * The number of columns of students.
    */
  val N_COLUMNS = 10

  /**
    * The total number of students in a classroom.
    */
  val N_STUDENTS: Int = N_ROWS * N_COLUMNS

  /**
    * The highest like score that a student can have towards another student.
    */
  val MAX_LIKE: Int = 10

  /**
    * The maximum raw score that a chromosome can have.
    */
  val MAX_SCORE: Double = MAX_LIKE * (
    (4 * 2) +
      (((N_COLUMNS + N_ROWS) * 2 - 4) * 3) +
      (N_COLUMNS - 2) * (N_ROWS - 2) * 4
    )

  /**
    * Creates a randomized set of student preferences.
    *
    * This is used when creating the student preferences for a simulation.
    *
    * @return A randomly generated set of student preferences.
    */
  def createStudents(): Students = {
    val random = ThreadLocalRandom.current()
    val colors = for (s1 <- 0 until N_STUDENTS) yield Color.rgb(
      random.nextInt(256),
      0,
      0
    )

    new Students(colors.toArray)
  }
}

/**
  * A Students object represents the definition of a student seating problem.
  *
  * It defines the preferences of all of the students toward each other.
  *
  * The preferences are defined by each student having an RGB color, such that
  * the more simmilar the color of two students, the higher preference they
  * have towards one another.
  */
case class Students(colors: Array[Color]) {
  /**
    * Calculates a normalized [0.0, 1.0] score for the given Chromosome
    * solution to the seating problem.
    *
    * @param chromosome The chromosome to score.
    * @return The normalized score of the chromosome.
    */
  def getScore(chromosome: Chromosome): Double = {
    def getStudentScore(x: Int, y: Int): Double = {
      val student = chromosome.getStudentIndex(x, y)

      val neighborIndexes = getNeighborsIndexes(x, y)
      val neighborPreferences = for ((a, b) <- neighborIndexes) yield getPreference(chromosome.getStudentIndex(a, b), student)

      neighborPreferences.sum
    }

    val scores = for (x <- 0 until Students.N_COLUMNS; y <- 0 until Students.N_ROWS) yield getStudentScore(x, y)

    if (scores.sum > 0) 1.0 / scores.sum else 1.0
  }

  /**
    * Returns the preference of the given student to the student at the given
    * seating position.
    *
    * @param otherIndex The index of the other student to get the preference of.
    * @param studentIndex The index of the student to get the preference of.
    * @return The preference value.
    */
  private def getPreference(otherIndex: Int, studentIndex: Int): Double = {
    val otherColor = colors(otherIndex)
    val thisColor = colors(studentIndex)

    Math.pow(thisColor.red - otherColor.red, 2) + Math.pow(thisColor.green - otherColor.green, 2) +
      Math.pow(thisColor.blue - otherColor.blue, 2)
  }

  /**
    * Returns the positions of all of the possible seats neighboring the given
    * seating position.
    *
    * @param x The column of the seat.
    * @param y The row of the seat.
    * @return The columns and rows of the possible neighboring seats.
    */
  private def getNeighborsIndexes(x: Int, y: Int): Array[(Int, Int)] = {
    val possibleNeighbors = for (
      xd <- List(-1, 0, 1);
      yd <- List(-1, 0, 1)
    ) yield (x + xd, y + yd)

    def isValidNeighbor(ab: (Int, Int)): Boolean = {
      ab match {
        case (a, b) => a >= 0 && b >= 0 && a < Students.N_COLUMNS && b < Students.N_ROWS
      }
    }

    val neighbors = possibleNeighbors.filter(isValidNeighbor)

    neighbors.toArray
  }
}
