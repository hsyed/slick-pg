package com.github.tminglei.slickpg
package utils

import scala.reflect.runtime.{universe => ru, currentMirror => runtimeMirror}
import scala.reflect.ClassTag

@scala.annotation.implicitNotFound(msg = "No converter available for ${FROM} to ${TO}")
trait Converter[FROM, TO] extends (FROM => TO)

object Converter {
  def apply[FROM, TO](convert: (FROM => TO)) =
    new Converter[FROM, TO] {
      def apply(v: FROM) = convert(v)
    }
}

object JdbcTypeHelper {

  private[this] case class CacheKey(val from: ru.Type, val to: ru.Type) {
    override def equals(o: Any) = {
      if (o.isInstanceOf[CacheKey]) {
        val that = o.asInstanceOf[CacheKey]
        from =:= that.from && to =:= that.to
      } else false
    }
  }
  
  private var converterMap = Map[CacheKey, Converter[_, _]]()

  def register[FROM,TO](convert: (FROM => TO))(implicit from: ru.TypeTag[FROM], to: ru.TypeTag[TO]) = {
    println(s"register conveter for ${from.tpe} => ${to.tpe}")
    converterMap += (CacheKey(from.tpe, to.tpe) -> Converter(convert))
  }

  def converter[FROM,TO](implicit from: ru.TypeTag[FROM], to: ru.TypeTag[TO]): Converter[FROM,TO] = {
    val cacheKey = CacheKey(from.tpe, to.tpe)
    converterMap.get(cacheKey).map(_.asInstanceOf[Converter[FROM,TO]])
      .getOrElse({
        if (to.tpe <:< from.tpe) {
          converterMap += (cacheKey -> Converter((v: FROM) => v.asInstanceOf[TO]))
          converterMap(cacheKey).asInstanceOf[Converter[FROM,TO]]
        } else throw new IllegalArgumentException(s"Converter NOT FOUND for ${from.tpe} to ${to.tpe}")
      })
  }

  ///
  def createConvFromStringList[T](implicit ev: ru.TypeTag[T], ev1: ru.TypeTag[String]): Converter[List[String], T] = {

    new Converter[List[String], T] {
      val thisType = ru.typeOf[T]
      val strType  = ru.typeOf[String]
      
      val constructor = thisType.declaration(ru.nme.CONSTRUCTOR).asMethod
      val converters = constructor.paramss.head.map(_.typeSignature)
        .map(at => converterMap(CacheKey(strType, at)).asInstanceOf[Converter[String,_]])
        
      //--
      def apply(strList: List[String]): T = {
        if (strList.length != converters.length) throw new IllegalArgumentException("")

        val classMirror = runtimeMirror.reflectClass(thisType.typeSymbol.asClass)
        val ctorMirror = classMirror.reflectConstructor(constructor)
        
        val args = strList.zip(converters).map {
          case (str, conv) => conv(str)
        }
        ctorMirror.apply(args: _*).asInstanceOf[T]
      }
    }
  }
  
  def createConvToValueList[T](implicit ev: ru.TypeTag[T], ev1: ClassTag[T]): Converter[T, List[Any]] = {
    
    new Converter[T, List[Any]] {
      val thisType = ru.typeOf[T]
      
      val constructor = thisType.declaration(ru.nme.CONSTRUCTOR).asMethod
      val ctorFields  = constructor.paramss.head.map(_.name).map(thisType.declaration(_).asTerm)
      
      //--
      def apply(v: T): List[Any] = {
        val instanceMirror = runtimeMirror.reflect(v)
        ctorFields.map(instanceMirror.reflectField(_).get)
      }
    }
  }
  
  ///
  case class T(id: Long, name: String, desc: String)
  
  def main(args: Array[String]) {
    JdbcTypeHelper.register((v: String) => v.toInt)
    JdbcTypeHelper.register((v: String) => v.toLong)
    JdbcTypeHelper.register((v: String) => v)
    
    val conv = createConvFromStringList[T]    
    println(conv(List("111", "test", "test desc")))
    
    val conv1 = createConvToValueList[T]
    println(conv1(T(112, "test", "test 2")))
    
    println("ok")
  }
}
