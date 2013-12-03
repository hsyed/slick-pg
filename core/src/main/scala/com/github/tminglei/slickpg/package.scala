package com.github.tminglei

import scala.reflect.runtime.universe.TypeTag

package object slickpg {
  type Struct = struct.Struct
  type Converter[FROM, TO] = utils.Converters.Converter[FROM, TO]
  type GenericJdbcType[T] = utils.GenericJdbcType[T]

  val ConverterUtil = utils.ConverterUtil
  val Converters = utils.Converters
  val Converter = utils.Converters.Converter

  trait ImplicitConverters {
    implicit def converter[FROM, TO](implicit ev1: TypeTag[FROM], ev2: TypeTag[TO]) = Converters.converter[FROM, TO]
  }

  {
    // register (String => xxxx) converters
    Converters.register((v: String) => v.toInt)
    Converters.register((v: String) => v.toLong)
    Converters.register((v: String) => v.toShort)
    Converters.register((v: String) => v.toFloat)
    Converters.register((v: String) => v.toDouble)
    Converters.register((v: String) => v.toBoolean)
    Converters.register((v: String) => v.toByte)

    // register date/time converters
//    JdbcTypeHelper.register(Converter())
  }
}
