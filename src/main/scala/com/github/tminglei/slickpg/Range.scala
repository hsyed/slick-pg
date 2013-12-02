package com.github.tminglei.slickpg

case class Range[T](start: T, end: T, edge: EdgeType = `[_,_)`) {

  def as[A](implicit convert: Converter[T, A]): Range[A] = {
    new Range[A](convert(start), convert(end), edge)
  }

  override def toString = edge match {
    case `[_,_)` => s"[$start,$end)"
    case `(_,_]` => s"($start,$end]"
    case `(_,_)` => s"($start,$end)"
    case `[_,_]` => s"[$start,$end]"
  }
}

// edge type definitions
sealed trait EdgeType
  case object `[_,_)` extends EdgeType
  case object `(_,_]` extends EdgeType
  case object `(_,_)` extends EdgeType
  case object `[_,_]` extends EdgeType

///
object Range {

  // regular expr matchers to range string
  val `[_,_)Range`  = """\["?([^,]*)"?,[ ]*"?([^,]*)"?\)""".r   // matches: [_,_)
  val `(_,_]Range`  = """\("?([^,]*)"?,[ ]*"?([^,]*)"?\]""".r   // matches: (_,_]
  val `(_,_)Range`  = """\("?([^,]*)"?,[ ]*"?([^,]*)"?\)""".r   // matches: (_,_)
  val `[_,_]Range`  = """\["?([^,]*)"?,[ ]*"?([^,]*)"?\]""".r   // matches: [_,_]

  def mkFromString[T](implicit convert: Converter[String, T]): Converter[String, Range[T]] =
    Converter((str: String) => str match {
      case `[_,_)Range`(start, end) => Range(convert(start), convert(end), `[_,_)`)
      case `(_,_]Range`(start, end) => Range(convert(start), convert(end), `(_,_]`)
      case `(_,_)Range`(start, end) => Range(convert(start), convert(end), `(_,_)`)
      case `[_,_]Range`(start, end) => Range(convert(start), convert(end), `[_,_]`)
    })

  ///
  def mkWithLength[T](start: T, length: Double, edge: EdgeType = `[_,_)`) = {
    val upper = (start.asInstanceOf[Double] + length).asInstanceOf[T]
    new Range[T](start, upper, edge)
  }

  def mkWithInterval[T <: java.util.Date](start: T, interval: Interval, edge: EdgeType = `[_,_)`) = {
    val end = (start +: interval).asInstanceOf[T]
    new Range[T](start, end, edge)
  }
}
