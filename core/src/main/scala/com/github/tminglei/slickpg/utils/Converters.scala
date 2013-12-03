package com.github.tminglei.slickpg
package utils

import scala.reflect.runtime.{universe => ru, currentMirror => runtimeMirror}
import scala.reflect.ClassTag

object Converters {
  @scala.annotation.implicitNotFound(msg = "No converter available for ${FROM} to ${TO}")
  trait Converter[FROM, TO] extends (FROM => TO)

  object Converter {
    def apply[FROM, TO](convert: (FROM => TO)) =
      new Converter[FROM, TO] {
        def apply(v: FROM) = convert(v)
      }
  }

  ///
  private case class CacheKey(val from: ru.Type, val to: ru.Type) {
    override def equals(o: Any) = {
      if (o.isInstanceOf[CacheKey]) {
        val that = o.asInstanceOf[CacheKey]
        from =:= that.from && to =:= that.to
      } else false
    }
  }

  private var converterMap = Map[CacheKey, Converter[_, _]]()

  private[utils] def internalGet(from: ru.Type, to: ru.Type) = {
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
}

object ConverterUtil {

  //TODO analog @hsyed's implementation, should be deleted later
  sealed trait Element
  case class ValueE(value: String) extends Element
  case class ArrayE(elements: List[Element]) extends Element
  case class CompositeE(members: List[Element]) extends Element
  //

  ///
  def mkConvFromElement[T <: AnyRef](implicit ev: ru.TypeTag[T]): Converter[Element, T] = {

    new Converter[Element, T] {
      private val thisType = ru.typeOf[T]
      private val optType = ru.typeOf[Option[_]]

      private val constructor = thisType.declaration(ru.nme.CONSTRUCTOR).asMethod
      private val ctorArgInfo = constructor.paramss.head.map(_.typeSignature).map(tpe =>
        if (tpe.typeConstructor =:= optType.typeConstructor) {
          val realType = tpe.asInstanceOf[ru.TypeRef].args(0)
          (realType, true)
        } else (tpe, false)
      )

      ///
      def apply(elem: Element): T = {
        val classMirror = runtimeMirror.reflectClass(thisType.typeSymbol.asClass)
        val ctorMirror = classMirror.reflectConstructor(constructor)

        val args = elem.asInstanceOf[CompositeE].members.zip(ctorArgInfo).map {
          case (e, (tpe, isOption)) => {
            val tv = convert(e, tpe)
            if (isOption) Option(tv) else tv
          }
        }
        ctorMirror.apply(args: _*).asInstanceOf[T]
      }
    }
  }
  
  def mkArrayConvFromElement[T](implicit ev: ru.TypeTag[T]): Converter[Element, List[T]] = {
    
    new Converter[Element, List[T]] {
      private val thisType = ru.typeOf[T]
      private val optType = ru.typeOf[Option[_]]

      private val (realType, isOption) =
        if (thisType.typeConstructor =:= optType.typeConstructor) {
          val realType = thisType.asInstanceOf[ru.TypeRef].args(0)
          (realType, true)
        } else (thisType, false)

      def apply(elem: Element): List[T] = {
        elem.asInstanceOf[ArrayE].elements.map { e =>
          val tv = convert(e, realType)
          if (isOption) Option(tv) else tv
        } map (_.asInstanceOf[T])
      }
    }
  }

  //
  private val strType  = ru.typeOf[String]
  private val elemType = ru.typeOf[Element]

  private def convert(e: Element, toType: ru.Type): Any = e match {
    case ValueE(v)  => Converters.internalGet(strType, toType).get.asInstanceOf[Converter[String,_]](v)
    case _: CompositeE  => Converters.internalGet(elemType, toType).get.asInstanceOf[Converter[Element,_]](e)
    case ArrayE(el) => {
      val eType = toType.asInstanceOf[ru.TypeRef].args(0)
      el.map(e => convert(e, eType))
    }
  }

  ////
  def mkConvToValueTree[T](implicit ev: ru.TypeTag[T], ev1: ClassTag[T]): Converter[T, List[Any]] = {
    
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
  case class T(id: Long, name: String, desc: String, opt: Option[String] = None)
  case class T1(id: Long, t: T, childIds: List[Long])
  
  def main(args: Array[String]) {
    Converters.register((v: String) => v.toInt)
    Converters.register((v: String) => v.toLong)
//    Converters.register((v: String) => v)
    
    val conv = mkConvFromElement[T]
    println(conv(CompositeE(List(ValueE("111"), ValueE("test"), ValueE("test desc"), ValueE(null)))))
    
    val conv1 = mkConvToValueTree[T]
    println(conv1(T(112, "test", "test 2")))
    
    println("ok")

    Converters.register(conv)
    val convt1 = mkConvFromElement[T1]
    println(convt1(CompositeE(List(ValueE("115"), CompositeE(List(ValueE("111"), ValueE("test"), ValueE("test dd"), ValueE("hi"))), ArrayE(List(ValueE("157")))))))

    val convt11 = mkConvToValueTree[T1]
    println(convt11(T1(116, T(111, "test", "test 3"), List(123,135))))

    println("ok")

    Converters.register(convt1)
    val convat = mkArrayConvFromElement[T1]
    println(convat(ArrayE(List(CompositeE(List(ValueE("115"), CompositeE(List(ValueE("111"), ValueE("test"), ValueE("test dd"), ValueE("hi"))), ArrayE(List(ValueE("157")))))))))
  }
}
