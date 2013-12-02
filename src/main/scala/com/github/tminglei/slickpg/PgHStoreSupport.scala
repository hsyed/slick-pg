package com.github.tminglei.slickpg

import scala.slick.driver.PostgresDriver
import scala.slick.lifted.Column
import scala.slick.jdbc.JdbcType
import org.postgresql.util.HStoreConverter
import scala.collection.convert.{WrapAsScala, WrapAsJava}

trait PgHStoreSupport extends hstore.PgHStoreExtensions { driver: PostgresDriver =>

  trait HStoreImplicits {
    implicit val hstoreTypeMapper =
      new GenericJdbcType[Map[String, String]](
        "hstore",
        _fromString = ((s: String) => WrapAsScala.mapAsScalaMap(HStoreConverter.fromString(s).asInstanceOf[java.util.Map[String,String]]).toMap),
        _toString = ((v: Map[String, String]) => HStoreConverter.toString(WrapAsJava.mapAsJavaMap(v))),
        zero = Map.empty[String, String]
      )

    implicit def hstoreColumnExtensionMethods(c: Column[Map[String, String]])(
      implicit tm: JdbcType[Map[String, String]], tm1: JdbcType[List[String]]) = {
        new HStoreColumnExtensionMethods[Map[String, String]](c)
      }
    implicit def hstoreOptionColumnExtensionMethods(c: Column[Option[Map[String,String]]])(
      implicit tm: JdbcType[Map[String, String]], tm1: JdbcType[List[String]]) = {
        new HStoreColumnExtensionMethods[Option[Map[String, String]]](c)
      }
  }
}
