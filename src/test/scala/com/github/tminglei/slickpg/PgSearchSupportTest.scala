package com.github.tminglei.slickpg

import org.junit._
import org.junit.Assert._

class PgSearchSupportTest {
  import MyPostgresDriver.simple._

  val db = Database.forURL(url = "jdbc:postgresql://localhost/test?user=test", driver = "org.postgresql.Driver")

  case class TestBean(id: Long, text: String, comment: String)

  object TestTable extends Table[TestBean](Some("test"), "testTable") {
    def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
    def text = column[String]("text")
    def comment = column[String]("comment")

    def * = id ~ text ~ comment <> (TestBean, TestBean unapply _)
  }

  //-----------------------------------------------------------------------------

  val testRec1 = TestBean(33L, "fat cat ate rat", "")
  val testRec2 = TestBean(37L, "cat", "fat")

  @Test
  def testSearchFunctions(): Unit = {
    db withSession { implicit session: Session =>
      TestTable.insert(testRec1)
      TestTable.insert(testRec2)

      val q1 = TestTable.where(r => tsVector(r.text) @@ tsQuery("cat & rat".bind)).sortBy(_.id).map(t => t)
      println(s"'@@' sql = ${q1.selectStatement}")
      assertEquals(List(testRec1), q1.list())

      val q2 = TestTable.where(r => (tsVector(r.text) @+ tsVector(r.comment)) @@ tsQuery("cat & fat".bind)).sortBy(_.id).map(t => t)
      println(s"'@+' sql = ${q2.selectStatement}")
      assertEquals(List(testRec1, testRec2), q2.list())

      val q3 = TestTable.where(r => tsVector(r.text) @@ (tsQuery("cat".bind) @& tsQuery("rat".bind))).sortBy(_.id).map(t => t)
      println(s"'@&' sql = ${q3.selectStatement}")
      assertEquals(List(testRec1), q3.list())

      val q4 = TestTable.where(r => tsVector(r.text) @@ (tsQuery("cat".bind) @| tsQuery("rat".bind))).sortBy(_.id).map(t => t)
      println(s"'@|' sql = ${q4.selectStatement}")
      assertEquals(List(testRec1, testRec2), q4.list())

      val q5 = TestTable.where(r => tsVector(r.text) @@ (tsQuery("cat".bind) @& tsQuery("rat".bind).!!)).sortBy(_.id).map(t => t)
      println(s"'!!' sql = ${q5.selectStatement}")
      assertEquals(List(testRec2), q5.list())

      val q6 = TestTable.where(r => tsPlainQuery(r.text) @> tsQuery(r.comment)).sortBy(_.id).map(t => t)
      println(s"'@>' sql = ${q6.selectStatement}")
      assertEquals(List(testRec1), q6.list())
    }
  }

  @Test
  def testOtherFunctions(): Unit = {
    db withSession { implicit session: Session =>
      TestTable.insert(TestBean(11L, "Neutrinos in the Sun", ""))
      TestTable.insert(TestBean(12L, "The Sudbury Neutrino Detector", ""))
      TestTable.insert(TestBean(13L, "A MACHO View of Galactic Dark Matter", ""))
      TestTable.insert(TestBean(14L, "Hot Gas and Dark Matter", ""))
      TestTable.insert(TestBean(15L, "The Virgo Cluster: Hot Plasma and Dark Matter", ""))
      TestTable.insert(TestBean(16L, "Rafting for Solar Neutrinos", ""))
      TestTable.insert(TestBean(17L, "NGC 4650A: Strange Galaxy and Dark Matter", ""))
      TestTable.insert(TestBean(18L, "Hot Gas and Dark Matter", ""))
      TestTable.insert(TestBean(19L, "Ice Fishing for Cosmic Neutrinos", ""))
      TestTable.insert(TestBean(20L, "Weak Lensing Distorts the Universe", ""))

      val query = tsQuery("neutrino|(dark & matter)".bind)

      val q1 = TestTable.where(r => tsVector(r.text) @@ query).map(r => (r.id, r.text, tsRank(tsVector(r.text), query))).sortBy(_._3)
      println(s"\n'ts_rank' sql = ${q1.selectStatement}")
      q1.list().map(r => println(s"${r._1}  |  ${r._2} | ${r._3}"))

      val q2 = TestTable.where(r => tsVector(r.text) @@ query).map(r => (r.id, r.text, tsRankCD(tsVector(r.text), query))).sortBy(_._3)
      println(s"\n'ts_rank_cd' sql = ${q2.selectStatement}")
      q2.list().map(r => println(s"${r._1}  |  ${r._2} | ${r._3}"))

      val q3 = TestTable.where(r => tsVector(r.text) @@ query).map(r => (r.id, r.text, tsHeadline(r.text, query)))
      println(s"\n'ts_headline' sql = ${q3.selectStatement}")
      q3.list().map(r => println(s"${r._1}  |  ${r._2} | ${r._3}"))
    }
  }

  //--------------------------------------------------------------------------------

  @Before
  def createTables(): Unit = {
    db withSession { implicit session: Session =>
      TestTable.ddl create
    }
  }

  @After
  def dropTables(): Unit = {
    db withSession { implicit session: Session =>
      TestTable.ddl drop
    }
  }
}
