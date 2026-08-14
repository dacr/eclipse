package fr.janalyse.sotohp.media.imaging

/** Small dense linear algebra helpers, enough for the fits done around here */
object LinearAlgebra {

  /** Solves `matrix . solution = vector` by gaussian elimination with partial pivoting */
  def solve(matrix: Array[Array[Double]], vector: Array[Double]): Option[Array[Double]] = {
    val size = vector.length
    if (matrix.length != size || matrix.exists(_.length != size)) None
    else {
      val work   = matrix.map(_.clone())
      val result = vector.clone()
      var column = 0
      var failed = false
      while (column < size && !failed) {
        var pivotRow = column
        var row      = column + 1
        while (row < size) {
          if (math.abs(work(row)(column)) > math.abs(work(pivotRow)(column))) pivotRow = row
          row += 1
        }
        if (math.abs(work(pivotRow)(column)) < 1e-12) failed = true
        else {
          val swappedRow = work(column); work(column) = work(pivotRow); work(pivotRow) = swappedRow
          val swappedRes = result(column); result(column) = result(pivotRow); result(pivotRow) = swappedRes
          var target     = column + 1
          while (target < size) {
            val factor = work(target)(column) / work(column)(column)
            var index  = column
            while (index < size) { work(target)(index) -= factor * work(column)(index); index += 1 }
            result(target) -= factor * result(column)
            target += 1
          }
          column += 1
        }
      }
      if (failed) None
      else {
        val solution = Array.ofDim[Double](size)
        var row      = size - 1
        while (row >= 0) {
          var accumulator = result(row)
          var index       = row + 1
          while (index < size) { accumulator -= work(row)(index) * solution(index); index += 1 }
          solution(row) = accumulator / work(row)(row)
          row -= 1
        }
        Some(solution)
      }
    }
  }

  /** Least squares polynomial fit, returns the coefficients from the constant term upwards */
  def polynomialFit(points: Seq[(Double, Double)], degree: Int): Option[Array[Double]] = {
    val size = degree + 1
    if (points.sizeIs < size) None
    else {
      val matrix = Array.fill(size, size)(0d)
      val vector = Array.fill(size)(0d)
      points.foreach { case (x, y) =>
        var row = 0
        while (row < size) {
          var column = 0
          while (column < size) {
            matrix(row)(column) += math.pow(x, (row + column).toDouble)
            column += 1
          }
          vector(row) += y * math.pow(x, row.toDouble)
          row += 1
        }
      }
      solve(matrix, vector)
    }
  }

  /** Evaluates a polynomial given its coefficients, constant term first */
  def evaluatePolynomial(coefficients: Array[Double], x: Double): Double = {
    var result = 0d
    var index  = coefficients.length - 1
    while (index >= 0) {
      result = result * x + coefficients(index)
      index -= 1
    }
    result
  }
}
