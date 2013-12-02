package com.github.tminglei.slickpg
package struct

import scala.reflect.ClassTag
import scala.slick.jdbc.{PositionedResult, PositionedParameters, JdbcType}
import scala.slick.ast.{ScalaBaseType, ScalaType, BaseTypedType}
import org.postgresql.util.PGobject

class StructJdbcType[T <: Struct](pgStructType: String)(implicit tag: ClassTag[T],
                          convFromString: Converter[String,T], convToString: Converter[T,String])
                          extends JdbcType[T] with BaseTypedType[T] {

  def scalaType: ScalaType[T] = ScalaBaseType[T]

  def zero: T = null.asInstanceOf[T]

  def sqlType: Int = java.sql.Types.OTHER

  def sqlTypeName: String = pgStructType

  def setValue(v: T, p: PositionedParameters) = p.setObject(mkPgObject(v), sqlType)

  def setOption(v: Option[T], p: PositionedParameters) = p.setObjectOption(v.map(mkPgObject), sqlType)

  def nextValue(r: PositionedResult): T = r.nextStringOption().map(convFromString).getOrElse(zero)

  def updateValue(v: T, r: PositionedResult) = r.updateObject(mkPgObject(v))

  def hasLiteralForm: Boolean = false

  override def valueToSQLLiteral(v: T) = convToString(v)

  ///
  private def mkPgObject(v: T) = {
    val obj = new PGobject
    obj.setType(sqlTypeName)
    obj.setValue(valueToSQLLiteral(v))
    obj
  }
}
