package org.geoscript.geocss.logic

import org.specs._

class SCPSpec extends Specification {
"Exercise constraint satisfier" in {
  "Example 1: Australia map coloring" in {
    object Coloring extends ProblemDomain {
      type Variable = Symbol
      type Value = Symbol
    }

    import Coloring._

    val variables = Set('WA, 'NT, 'Q, 'NSW, 'V, 'SA, 'T)
    val domains =
      (variables zip Stream.continually(Set('red, 'green, 'blue))).toMap

    val edges = Seq(
      'SA -> 'WA,
      'SA -> 'NT,
      'SA -> 'Q,
      'SA -> 'NSW,
      'SA -> 'V,
      'WA -> 'NT,
      'NT -> 'Q,
      'Q -> 'NSW,
      'NSW -> 'V
    )

    val distinct = (x: Value, y: Value) => x != y
    val consistent =
      Constraints((edges zip (Stream continually distinct)).toMap)
    val problem = Problem(variables, domains, consistent)
    val answers = 
      solve(selectUnassignedVariable, orderDomainValues, inference)(problem)

    answers.toSet must_== Set(
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'red, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'blue),
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'red, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'green),
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'green, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'blue),
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'green, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'green),
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'blue, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'blue),
      Map('NSW -> 'red, 'NT -> 'red, 'T -> 'blue, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'green),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'red, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'blue),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'red, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'red),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'green, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'blue),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'green, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'red),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'blue, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'blue),
      Map('NSW -> 'green, 'NT -> 'green, 'T -> 'blue, 'Q -> 'blue, 'WA -> 'blue, 'V -> 'blue, 'SA -> 'red),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'red, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'green),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'red, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'red),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'green, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'green),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'green, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'red),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'blue, 'Q -> 'red, 'WA -> 'red, 'V -> 'red, 'SA -> 'green),
      Map('NSW -> 'blue, 'NT -> 'blue, 'T -> 'blue, 'Q -> 'green, 'WA -> 'green, 'V -> 'green, 'SA -> 'red)
    )
  }

  "Example 2: (easy) Sudoku puzzle" in { 
    object Sudoku extends ProblemDomain {
      type Variable = Symbol
      type Value = Int
      
      val variables =
        ( for {
            r <- 'A' to 'I'
            c <- '1' to '9'
          } yield Symbol(r.toString + c)
        ).toSet

      val values = (1 to 9).toSet
      
      val domains =
        (variables zip Stream.continually(values)).toMap

      val rows =
        for (r <- 'A' to 'I') yield
          for (c <- '1' to '9') yield Symbol(r.toString + c)

      val cols =
        for (c <- '1' to '9') yield
          for (r <- 'A' to 'I') yield Symbol(r.toString + c)

      val boxes =
        for {
          R <- 'A' to 'I' grouped(3)
          C <- '1' to '9' grouped(3)
        } yield {
          for (r <- R; c <- C) yield Symbol(r.toString + c)
        }

      val distinct = (a: Value, b: Value) => a != b

      val constraints =
        for { 
          group <- rows ++ cols ++ boxes
          x <- group
          y <- group
          if x != y
        } yield ((x, y), distinct)

      val consistent = Constraints(constraints.toMap)
    }
    import Sudoku._
    val givens = Map(
      'A3 -> 3, 'A5 -> 2, 'A7 -> 6, 'B1 -> 9, 'B4 -> 3, 'B6 -> 5, 'B9 -> 1, 'C3 -> 1, 'C4 -> 8, 'C6 -> 6, 'C7 -> 4, 'D3 -> 8, 'D4 -> 1, 'D6 -> 2, 'D7 -> 9, 'E1 -> 7, 'E9 -> 8, 'F3 -> 6, 'F4 -> 7, 'F6 -> 8, 'F7 -> 2, 'G3 -> 2, 'G4 -> 6, 'G6 -> 9, 'G7 -> 5, 'H1 -> 8, 'H4 -> 2, 'H6 -> 3, 'H9 -> 9, 'I3 -> 5, 'I5 -> 1, 'I7 -> 3
    ).mapValues(Set(_))

    val puzzle = Problem(variables, domains ++ givens, consistent)
    val reduced = reduce(puzzle)
    reduced must beSome
    val p = reduced.get
    val answer = 
      solve(selectUnassignedVariable, orderDomainValues, inference)(p)

    answer must haveSize(1)
    val grid = answer.head
    val answerAsGrid = rows.map(r => r map grid)

    // leaving out the type parameters below makes compilation way slow...
    val expected = 
      Seq(
        Seq(4, 8, 3, 9, 2, 1, 6, 5, 7),
        Seq(9, 6, 7, 3, 4, 5, 8, 2, 1),
        Seq(2, 5, 1, 8, 7, 6, 4, 9, 3),
        Seq(5, 4, 8, 1, 3, 2, 9, 7, 6),
        Seq(7, 2, 9, 5, 6, 4, 1, 3, 8),
        Seq(1, 3, 6, 7, 9, 8, 2, 4, 5),
        Seq(3, 7, 2, 6, 8, 9, 5, 1, 4),
        Seq(8, 1, 4, 2, 5, 3, 7, 6, 9),
        Seq(6, 9, 5, 4, 1, 7, 3, 8, 2)
      )

    answerAsGrid must_== expected
  }
}
}
