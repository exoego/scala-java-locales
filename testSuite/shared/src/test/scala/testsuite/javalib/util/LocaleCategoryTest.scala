package testsuite.javalib.util

import utest._
import java.util.Locale

import testsuite.utils.LocaleTestSetup
import testsuite.utils.AssertThrows.expectThrows

object LocaleCategoryTest extends TestSuite with LocaleTestSetup {
  import Locale.Category

  val tests = Tests {
    'test_getOrdinal - {
      assertEquals(0, Category.DISPLAY.ordinal)
      assertEquals(1, Category.FORMAT.ordinal)
    }

    'test_getValues - {
      assertEquals(2, Category.values().length)
      assertEquals(Category.DISPLAY, Category.values()(0))
      assertEquals(Category.FORMAT, Category.values()(1))
    }

    'test_valueOf - {
      assertEquals(Category.DISPLAY, Category.valueOf("DISPLAY"))
      assertEquals(Category.FORMAT, Category.valueOf("FORMAT"))

      expectThrows(classOf[IllegalArgumentException], Category.valueOf(""))
      expectThrows(classOf[NullPointerException], Category.valueOf(null))
    }
  }
}
