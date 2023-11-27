@main def main: Unit = {
  var cella = Cell(1, 2)
  var cellb = Cell(10, 20)
  var bsalpha = BaseStation("Alpha", cella)
  var bsbeta = BaseStation("Beta", cellb)

  println(cella.toString())
  println(bsalpha.toString())

  var mygrid = Grid(2)
  mygrid.add(bsalpha)
  mygrid.add(bsbeta)

  println(mygrid.toString())

}


class Cell(x: Int, y: Int) {
  override def toString: String = {
    s"($x, $y)"
  }
}

class BaseStation(name: String, cell: Cell) {
  override def toString: String = {
    s"$name$cell"
  }
}

class Grid(dimension: Int){
  private var myString =""
  def add(bStation: BaseStation): Unit = {
    myString += bStation.toString + ", "
  }

  override def toString: String = myString

}