package com.github.tminglei.slickpg

import scala.slick.driver.PostgresDriver
import scala.slick.lifted.Column
import java.sql.{Date, Timestamp}
import scala.slick.jdbc.JdbcType

trait PgRangeSupport extends range.PgRangeExtensions { driver: PostgresDriver =>

  type RANGEType[T] = Range[T]

  private val tsFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  private val dateFormatter = new java.text.SimpleDateFormat("yyyy-MM-dd")
  private def toTimestamp(str: String) = new Timestamp(tsFormatter.parse(str).getTime)
  private def toSQLDate(str: String) = new Date(dateFormatter.parse(str).getTime)

  trait RangeImplicits extends ImplicitConverters {
    implicit val intRangeTypeMapper = new GenericJdbcType[Range[Int]]("int4range", _fromString = Range.mkFromString[Int])
    implicit val longRangeTypeMapper = new GenericJdbcType[Range[Long]]("int8range", _fromString = Range.mkFromString[Long])
    implicit val floatRangeTypeMapper = new GenericJdbcType[Range[Float]]("numrange", _fromString = Range.mkFromString[Float])
    implicit val timestampRangeTypeMapper = new GenericJdbcType[Range[Timestamp]]("tsrange", _fromString = Range.mkFromString(Converter(toTimestamp)))
    implicit val dateRangeTypeMapper = new GenericJdbcType[Range[Date]]("daterange", _fromString = Range.mkFromString(Converter(toSQLDate)))

    implicit def rangeColumnExtensionMethods[B0, Range[B0]](c: Column[Range[B0]])(
      implicit tm: JdbcType[B0], tm1: JdbcType[RANGEType[B0]]) = {
        new RangeColumnExtensionMethods[B0, Range[B0]](c)
      }
    implicit def rangeOptionColumnExtensionMethods[B0, Range[B0]](c: Column[Option[Range[B0]]])(
      implicit tm: JdbcType[B0], tm1: JdbcType[RANGEType[B0]]) = {
        new RangeColumnExtensionMethods[B0, Option[Range[B0]]](c)
      }
  }
}
