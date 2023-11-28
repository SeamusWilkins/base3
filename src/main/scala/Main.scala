@main def main: Unit = {
  var cella = Cell(4, 10)
  var cellb = Cell(10, 10)
  var bsalpha = BaseStation("Alpha", cella)
  var bsbeta = BaseStation("Beta", cellb)

  var mygrid = Grid(21)
  mygrid.add(bsalpha)
  mygrid.add(bsbeta)

  println("Before:\n" + mygrid.toString())

  mygrid.improveGrid

  println("\nAfter:\n" + mygrid.toString())

}


class Cell(val x: Int, val y: Int) {override def toString: String = s"($x, $y)"}

class BaseStation(val name: String, val cell: Cell) {override def toString: String = s"$name$cell"}

class Grid(dimension: Int){

  if(dimension<1) throw new IllegalArgumentException("Dimension must be at least 1")
  private val map = scala.collection.mutable.Map[Cell, BaseStation]()
  def distance(x1: Int, y1: Int, x2: Int, y2: Int): Double = Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2))
  def insideGrid(cell: Cell): Boolean = cell.x>=0 && cell.x<dimension && cell.y>=0 && cell.y<dimension
  override def toString: String = map.values.map(_.toString).mkString(", ")

  def add(bStation: BaseStation): Unit = {
    val cell = bStation.cell
    if(!map.contains(cell) && insideGrid(cell)) map(cell) = bStation
    else println("Invalid cell")
  }

  def coverage(cell: Cell): Double = {
    if(map.isEmpty) return 0
    var lowestDistance = dimension.toDouble
    for(bs <- map.keys) lowestDistance = Math.min(distance(cell.x, cell.y, bs.x, bs.y), lowestDistance)
    1/(1+lowestDistance)
  }

  def allCoverage: List[Double] = (
    for( j <- 0 until dimension; i <- 0 until dimension )
      yield coverage(Cell(i, j))
  ).toList

  def neighbours(cell: Cell): List[Cell] = (
    for (j <- cell.y-1 to cell.y+1; i <- cell.x-1 to cell.x+1 if(j != cell.y || i != cell.x))
      yield  Cell(i, j)
  ).toList

  def gridQuality: Double = {val coverages = allCoverage; coverages.sum / coverages.length}

  def bestNeighbour(bs: BaseStation): (Cell, Double) = {
    var maxQuality = gridQuality; var bestCell = bs.cell
    for(cell <- neighbours(bs.cell) if(!map.contains(cell) && insideGrid(cell))){
      map.remove(bs.cell)
      map(cell)=BaseStation("test", cell);

      val newQuality = gridQuality
      if(newQuality>maxQuality){ maxQuality=newQuality; bestCell = cell }

      map(bs.cell)=bs
      map.remove(cell)
    }
    (bestCell, maxQuality)
  }

  def subImproveGrid(bs: BaseStation): Unit = {
    var best = bestNeighbour(bs)
    if (best._2>gridQuality) {
      map.remove(bs.cell)
      map(best._1) = BaseStation(bs.name, best._1)
      println("Moved " + bs + " to " + map(best._1))
      subImproveGrid(map(best._1))
    }
  }

  def improveGrid: Unit = for(bs <- map.values) subImproveGrid(bs)


}