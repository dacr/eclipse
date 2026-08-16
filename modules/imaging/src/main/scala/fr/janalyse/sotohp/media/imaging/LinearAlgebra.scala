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

  /** Product of two 3x3 row major matrices, `left` applied after `right` */
  def multiply3x3(left: Array[Double], right: Array[Double]): Array[Double] = {
    val result = Array.ofDim[Double](9)
    var row    = 0
    while (row < 3) {
      var column = 0
      while (column < 3) {
        var accumulator = 0d
        var index       = 0
        while (index < 3) { accumulator += left(row * 3 + index) * right(index * 3 + column); index += 1 }
        result(row * 3 + column) = accumulator
        column += 1
      }
      row += 1
    }
    result
  }

  /** Affine transform written as a 3x3 matrix : scale then translation, per axis */
  def affine3x3(scaleX: Double, scaleY: Double, offsetX: Double, offsetY: Double): Array[Double] =
    Array(scaleX, 0d, offsetX, 0d, scaleY, offsetY, 0d, 0d, 1d)

  /** Applies a 3x3 projective map to a point.
    *
    * `None` when the third coordinate is not positive : the point is then at infinity, or behind the
    * plane the map projects onto - which is the very same thing as a camera not seeing what is
    * behind it.
    */
  def applyProjective(matrix: Array[Double], x: Double, y: Double): Option[(Double, Double)] = {
    val depth = matrix(6) * x + matrix(7) * y + matrix(8)
    if (depth <= 1e-12d) None
    else Some(((matrix(0) * x + matrix(1) * y + matrix(2)) / depth, (matrix(3) * x + matrix(4) * y + matrix(5)) / depth))
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
