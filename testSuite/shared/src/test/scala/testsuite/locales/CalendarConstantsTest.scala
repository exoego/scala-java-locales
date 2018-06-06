package testsuite.locales

import locales.CalendarConstants
import utest._
import testsuite.utils.LocaleTestSetup

object CalendarConstantsTest extends TestSuite with LocaleTestSetup {
  val tests = Tests {
    'test_day_fields - {
      assertEquals(1, CalendarConstants.SUNDAY)
      assertEquals(2, CalendarConstants.MONDAY)
      assertEquals(3, CalendarConstants.TUESDAY)
      assertEquals(4, CalendarConstants.WEDNESDAY)
      assertEquals(5, CalendarConstants.THURSDAY)
      assertEquals(6, CalendarConstants.FRIDAY)
      assertEquals(7, CalendarConstants.SATURDAY)
    }
  }
}
