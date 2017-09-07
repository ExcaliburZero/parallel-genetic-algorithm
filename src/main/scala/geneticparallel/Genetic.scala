package geneticparallel

import scala.collection.mutable.ArrayBuffer

object Chromosome {
  val NUM_SWAP: Int = Students.N_STUDENTS / 3

  val MUTATION_CHANCE: Int = 10
  val AVG_NUM_MUTATIONS: Double = Students.N_STUDENTS / 10.0

  def createRandom(): Chromosome = {
    val indexes = (0 until Students.N_STUDENTS).toList
    val array = scala.util.Random.shuffle(indexes).toArray

    new Chromosome(array)
  }
}

case class Chromosome(seating: Array[Int]) {
  def getStudentIndex(x: Int, y: Int): Int = {
    val index = y * Students.N_COLUMNS + x
    seating(index)
  }

  def mate(other: Chromosome): Chromosome = {
    val changeIndices = scala.util.Random.shuffle((0 until Students.N_STUDENTS).toList).toIterable
      .take(Chromosome.NUM_SWAP)

    val newArray = this.seating.clone()
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

    new Chromosome(newArray)
  }

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

  override def clone(): AnyRef = {
    new Chromosome(seating.clone())
  }
}

object Population {
  val NUM_CHROMOSOMES: Int = 50
  val NUM_REMOVE: Int = NUM_CHROMOSOMES / 2
  val NUM_KEEP: Int = NUM_CHROMOSOMES - NUM_REMOVE

  def createPopulation(students: Students): Population = {
    val chromosomes = (for (_ <- 0 until NUM_CHROMOSOMES) yield Chromosome.createRandom()).toArray

    new Population(students, chromosomes)
  }
}

case class Population(students: Students, chromosomes: Array[Chromosome]) {
  def generation(): Population = {
    val scores = for (c <- this.chromosomes) yield students.getScore(c)
    val keepIndices = chromosomes.indices.sortBy(scores).reverse.take(Population.NUM_KEEP)

    val copyChromosomes = for (c <- this.chromosomes) yield c.clone().asInstanceOf[Chromosome]

    val newChromosomes = Array.fill[Chromosome](Population.NUM_CHROMOSOMES)(null)

    for (i <- keepIndices.indices) {
      val newI = keepIndices(i)

      newChromosomes.update(i, copyChromosomes(newI))
    }

    for (i <- Population.NUM_KEEP until Population.NUM_CHROMOSOMES) {
      val parents = scala.util.Random.shuffle((0 until Population.NUM_KEEP).toList).toIterable
        .take(2).toArray
      val c = chromosomes(parents(0)).mate(chromosomes(parents(1)))

      newChromosomes.update(i, c)
    }

    new Population(students, newChromosomes)
  }

  def getAverageScore(): Double = {
    val scores = for (c <- chromosomes) yield students.getScore(c)
    scores.sum / scores.length.toDouble
  }
}

object Students {
  val N_ROWS = 25
  val N_COLUMNS = 25

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
