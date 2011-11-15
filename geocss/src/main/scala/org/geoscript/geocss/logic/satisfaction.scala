package org.geoscript.geocss.logic

trait ProblemDomain {
  type Variable
  type Value
  type Arc = (Variable, Variable)
  type Assignment = Map[Variable, Value]
  type Domain = Set[Value]

  case class Constraints(
    constraints: Map[Arc, (Value, Value) => Boolean]
  ) extends (((Variable, Value), (Variable, Value)) => Boolean) {
    def apply(a: (Variable, Value), b: (Variable, Value)) = (
      constraints.get(a._1 -> b._1).map(f => f(a._2, b._2)) 
      orElse
      constraints.get(b._1 -> a._1).map(f => f(b._2, a._2))
      getOrElse
      true
    )

    def pairwise(x: Variable, y: Variable): Option[(Value, Value) => Boolean] =
    (
      constraints.get(x -> y)
      orElse
      constraints.get(y -> x).map(f => (a: Value, b: Value) => f(b, a))
    )

    def neighbors(x: Variable): Set[Variable] =
      constraints.keySet.collect {
        case (`x`, y) => y
        case (y, `x`) => y
      }
  }

  case class Problem(
    variables: Set[Variable],
    domains: Map[Variable, Domain],
    constraints: Constraints
  ) {
    def complete(assignment: Assignment) = assignment.keySet == variables
    def consistent(assignment: Assignment) = {
      val pairs =
        for (x <- assignment.view; y <- assignment.view) yield (x, y)
      pairs.forall(constraints.tupled)
    }
    def arcs = for (x <- variables; y <- variables; if x != y) yield (x, y)
    def neighbors(x: Variable): Set[Variable] = constraints.neighbors(x)
  }

  val selectUnassignedVariable =
    (p: Problem, a: Assignment) => (p.variables -- a.keySet).head

  val orderDomainValues =
    (p: Problem, a: Assignment, v: Variable) => (p domains v).toSeq

  val inference =
    (p: Problem, v: Variable, x: Value) => Some(Map.empty: Assignment)

  def solve(
    selectUnassignedVariable: (Problem, Assignment) => Variable,
    orderDomainValues: (Problem, Assignment, Variable) => Seq[Value],
    inference: (Problem, Variable, Value) => Option[Assignment]
  ) (problem: Problem): Seq[Assignment] = {
    def backtrack(assignment: Map[Variable, Value]): Seq[Assignment] =
      if (problem.complete(assignment))
        Seq(assignment)
      else {
        val variable = selectUnassignedVariable(problem, assignment)
        for {
          value <- orderDomainValues(problem, assignment, variable).view
          assignment_ = assignment + (variable -> value)
          if problem.consistent(assignment_)
          inferences <- inference(problem, variable, value).toSeq
          assignment__ = assignment_ ++ inferences
          result <- backtrack(assignment__)
        } yield result
      }

    backtrack(Map.empty)
  }

  def reduce(problem: Problem): Option[Problem] = {
    def revise(problem: Problem, xi: Variable, xj: Variable)
    : Option[Problem] = {
      val di = problem.domains(xi)
      val dj = problem.domains(xj)
      val constraints = problem.constraints.pairwise(xi, xj)
      constraints.flatMap { pred =>
        val di_ = di.filter { x => dj.exists { y => pred(x, y) } }
        if (di != di_)
          Some(problem.copy(domains = problem.domains + (xi -> di_)))
        else
          None
      }
    }

    import collection.immutable.Queue
    def recurse(queue: Queue[Arc], problem: Problem): Option[Problem] = {
      if (queue isEmpty)
        Some(problem)
      else {
        val ((xi, xj), queue_) = queue.dequeue
        revise(problem, xi, xj) match {
          case Some(p) if p.domains(xi) isEmpty =>
            None
          case Some(p) =>
            val additional = for(xk <- p.neighbors(xi) - xj) yield (xk, xi)
            recurse(queue_ ++ additional, p)
          case None =>
            recurse(queue_, problem)
        }
      }
    }

    recurse(Queue.empty ++ problem.arcs, problem)
  }
}
