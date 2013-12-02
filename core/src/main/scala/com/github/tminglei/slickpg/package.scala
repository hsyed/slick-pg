package com.github.tminglei

import scala.reflect.runtime.universe.TypeTag

package object slickpg {
  type Converter[FROM, TO] = utils.Converter[FROM, TO]
  type GenericJdbcType[T] = utils.GenericJdbcType[T]

  val JdbcTypeHelper = utils.JdbcTypeHelper
  val Converter = utils.Converter

  trait ImplicitConverters {
    implicit def converter[FROM, TO](implicit ev1: TypeTag[FROM], ev2: TypeTag[TO]) = JdbcTypeHelper.converter[FROM, TO]
  }

  {
    // register (String => xxxx) converters
    JdbcTypeHelper.register((v: String) => v.toInt)
    JdbcTypeHelper.register((v: String) => v.toLong)
    JdbcTypeHelper.register((v: String) => v.toShort)
    JdbcTypeHelper.register((v: String) => v.toFloat)
    JdbcTypeHelper.register((v: String) => v.toDouble)
    JdbcTypeHelper.register((v: String) => v.toBoolean)
    JdbcTypeHelper.register((v: String) => v.toByte)

    // register date/time converters
//    JdbcTypeHelper.register(Converter())
  }
}
