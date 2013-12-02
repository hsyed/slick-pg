package com.github.tminglei.slickpg
package struct

import scala.reflect.ClassTag
import scala.slick.jdbc.{PositionedResult, PositionedParameters, JdbcType}
import scala.slick.ast.{ScalaBaseType, ScalaType, BaseTypedType}
import org.postgresql.util.PGobject

class StructListJdbcType[T <: Struct](pgStructType: String)(implicit tag: ClassTag[T],
                            convFromString: Converter[String,List[T]], convToString: Converter[List[T],String])
                            extends JdbcType[List[T]] with BaseTypedType[List[T]] {

  def scalaType: ScalaType[List[T]] = ScalaBaseType[List[T]]

  def zero: List[T] = Nil

  def sqlType: Int = java.sql.Types.ARRAY

  def sqlTypeName: String = s"$pgStructType ARRAY"

  def setValue(v: List[T], p: PositionedParameters) = p.setObject(mkPgObject(v), sqlType)

  def setOption(v: Option[List[T]], p: PositionedParameters) = p.setObjectOption(v.map(mkPgObject), sqlType)

  def nextValue(r: PositionedResult): List[T] = r.nextStringOption().map(convFromString).getOrElse(zero)

  def updateValue(v: List[T], r: PositionedResult) = r.updateObject(mkPgObject(v))

  def hasLiteralForm: Boolean = false

  override def valueToSQLLiteral(v: List[T]) = convToString(v)

  ///
  private def mkPgObject(v: List[T]) = {
    val obj = new PGobject
    obj.setType(sqlTypeName)
    obj.setValue(valueToSQLLiteral(v))
    obj
  }
}
