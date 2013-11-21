package simulations

import math.random

class EpidemySimulator extends Simulator {
  def randomBelow(i: Int) = (random * i).toInt

  /**
    * Returns a boolean with x probability, where x is between 1 and
    * 100.
    */
  def withProb(x: Int): Boolean = randomBelow(100) < x

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val sicknessProbability: Int = 1
    val infectedProbability: Int = 40
    val deathProbability: Int = 25
  }

  /**
    * Returns the number of days that a character should wait before
    * moving.
    */
  def generateDelay = randomBelow(6) + 1

  /**
    * schedules an action with some delay function. This can be a
    * constant, of course.
    */
  def loop(delay: => Int)(f: Action) {
    afterDelay(delay) { f(); loop(delay)(f) }
  }

  import SimConfig._

  val persons: List[Person] = (0 until 300).map { i =>
    val p = new Person(i)
    loop(1)(() => p.lifecycle)
    loop(generateDelay)(() => p.act)
    p.infected = withProb(sicknessProbability) // Initialize with sickness.
    p
  }.toList

  class Person(val id: Int) {
    type Coord = (Int, Int) // row, col
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    private var sickDays = 0

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isPosition(coord: Coord): Boolean =
      coord == (row, col)

    def mod(i: Int, modulus: Int): Int =
      if (i < 0)
        mod(i + modulus, modulus)
      else
        i % modulus

    def above: Coord = (mod(row + 1, roomRows), col)
    def below: Coord = (mod(row - 1, roomRows), col)
    def left: Coord = (row, mod(col - 1, roomColumns))
    def right: Coord = (row, mod(col + 1, roomColumns))
    def move(newPos: Coord) {
      row = newPos._1
      col = newPos._2
    }

    def surroundings = Set(above, below, left, right)

    def infectious = infected || sick || dead
    def looksHealthy = !(sick || dead)
    def withSick = persons.exists(p => p.isPosition(row -> col) && p.infected)

    /**
      * Returns a pair of safe room choices, each paired with a
      * boolean about whether a sick person's in there or not.
      */
    def choices: Set[Coord] =
      Set(above, below, left, right).filter { loc =>
        val colocated = persons.filter(_.isPosition(loc))
        colocated.filterNot(_.looksHealthy).isEmpty
      }


    def randomElement[T](items: List[T]): Option[T] = {
      items match {
        case Nil => None
        case xs => Some(xs(randomBelow(xs.length)))
      }
    }

    def nextRoom: Option[Coord] =
      randomElement(choices.toList)

    /**
      * Teleports the user to a different square on the board. Used
      * for air travel implementation.
      */
    def teleport {
      move(randomBelow(roomRows), randomBelow(roomColumns))
    }

    /**
      * Performs the actual moving lifecycle.
      */
    def act {
      if (!dead) {
        nextRoom.foreach { newPos =>
          move(newPos)
          if (!infected && withSick)
            infected = withProb(infectedProbability)
        }
      }
    }

    /**
      * Gets called once every day. this needs to start before the
      * moving loop so we count properly. Encodes the rules
      */
    def lifecycle {
      def shouldDie = withProb(deathProbability)
      if (infected && !dead) sickDays += 1
      assert(sickDays <= 18, "days shouldn't ever get above 18.")
      if (sickDays == 18) {
        sickDays = 0
        infected = false
        sick = false
        immune = false
      } else if (sickDays == 16) {
        immune = true
        sick = false
      } else if (sickDays == 15 || sickDays == 14) {
        if (shouldDie) dead = true
      } else if (sickDays == 6) sick = true
    }
  }
}
