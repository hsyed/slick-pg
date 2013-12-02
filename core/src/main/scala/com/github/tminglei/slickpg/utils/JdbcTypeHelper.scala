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

  private def internalGet(from: ru.Type, to: ru.Type) = {
    val cacheKey = CacheKey(from, to)
    converterMap.get(cacheKey).orElse({
      if (to <:< from) {
        converterMap += (cacheKey -> Converter((v: Any) => v))
        converterMap.get(cacheKey)
      } else None
    })
  }

  def register[FROM,TO](convert: (FROM => TO))(implicit from: ru.TypeTag[FROM], to: ru.TypeTag[TO]) = {
    println(s"register converter for ${from.tpe} => ${to.tpe}")
    converterMap += (CacheKey(from.tpe, to.tpe) -> Converter(convert))
  }

  def converter[FROM,TO](implicit from: ru.TypeTag[FROM], to: ru.TypeTag[TO]): Converter[FROM,TO] = {
    internalGet(from.tpe, to.tpe).map(_.asInstanceOf[Converter[FROM,TO]])
      .getOrElse(throw new IllegalArgumentException(s"Converter NOT FOUND for ${from.tpe} => ${to.tpe}"))
  }

  ///
  def mkStructConvFromStringList[T <: Struct](implicit ev: ru.TypeTag[T]): Converter[List[Any], T] = {

    new Converter[List[Any], T] {
      val thisType = ru.typeOf[T]
      val strType  = ru.typeOf[String]
      val structType = ru.typeOf[Struct]
      val seqType = ru.typeOf[Seq[_]]
      val listType = ru.typeOf[List[_]]
      
      val constructor = thisType.declaration(ru.nme.CONSTRUCTOR).asMethod
      val converters = constructor.paramss.head.map(_.typeSignature).map({
        case tpe if tpe <:< structType => internalGet(listType, tpe).get.asInstanceOf[Converter[Any,_]]
        case tpe if tpe <:< seqType    => internalGet(listType, tpe).get.asInstanceOf[Converter[Any,_]]
        case tpe  =>  internalGet(strType, tpe).get.asInstanceOf[Converter[Any,_]]
      })
        
      //--
      def apply(strList: List[Any]): T = {
        if (strList.length != converters.length) throw new IllegalArgumentException("value list length not matched!!!")

        val classMirror = runtimeMirror.reflectClass(thisType.typeSymbol.asClass)
        val ctorMirror = classMirror.reflectConstructor(constructor)
        
        val args = strList.zip(converters).map {
          case (str, conv) => conv(str)
        }
        ctorMirror.apply(args: _*).asInstanceOf[T]
      }
    }
  }
  
  def mkStructConvToValueList[T](implicit ev: ru.TypeTag[T], ev1: ClassTag[T]): Converter[T, List[Any]] = {
    
    new Converter[T, List[Any]] {
      val thisType = ru.typeOf[T]
      
      val constructor = thisType.declaration(ru.nme.CONSTRUCTOR).asMethod
      val ctorFields  = constructor.paramss.head.map(_.name).map(thisType.declaration(_).asTerm)
      
      //--
      def apply(v: T): List[Any] = {
        val instanceMirror = runtimeMirror.reflect(v)
        ctorFields.map( field => {
          val fv = instanceMirror.reflectField(field).get
          val rv = if (fv.isInstanceOf[Option[_]]) fv.asInstanceOf[Option[_]].getOrElse(null) else fv
//          rv match {
//            case lv: Seq[_] => val conv = internalGet
//          }
          rv
        })
      }
    }
  }
  
  ///
  case class T(id: Long, name: String, desc: String) extends Struct
  case class T1(id: Long, child: T) extends Struct
  
  def main(args: Array[String]) {
    JdbcTypeHelper.register((v: String) => v.toInt)
    JdbcTypeHelper.register((v: String) => v.toLong)
//    JdbcTypeHelper.register((v: String) => v)
    
    val conv = mkStructConvFromStringList[T]
    println(conv(List("111", "test", "test desc")))
    
    val conv1 = mkStructConvToValueList[T]
    println(conv1(T(112, "test", "test 2")))
    
    println("ok")

    JdbcTypeHelper.register(conv)
    val convt1 = mkStructConvFromStringList[T1]
    println(convt1(List("115", List("111", "test", "test dd"))))

    val convt11 = mkStructConvToValueList[T1]
    println(convt11(T1(116, T(111, "test", "test 3"))))
  }
}
