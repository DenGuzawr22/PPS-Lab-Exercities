package u06lab.code

import org.junit.Test
import org.junit.Assert.*
import u06lab.code.ConnectThree.Player.*
import u06lab.code.ConnectThree.*

class TestConnectThree:

  @Test def testFind(): Unit =
    printBoards(placeAnyDisk(List(), X))
    // .... .... .... ....
    // .... .... .... ....
    // .... .... .... ....
    // ...X ..X. .X.. X...
    assertEquals(Some(X), find(List(Disk(0, 0, X)), 0, 0))
    assertEquals(Some(O), find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))
    assertEquals(None, find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test def testFirstAvailableRaw(): Unit =
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    // .... .... .... ....
    // .... .... .... ....
    // ...X .... .... ....
    // ...O ..XO .X.O X..O
    assertEquals(Some(0), firstAvailableRow(List(), 0))
    assertEquals(Some(1), firstAvailableRow(List(Disk(0, 0, X)), 0))
    assertEquals(Some(2), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0))
    assertEquals(Some(3), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0))
    assertEquals(None, firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0))

  @Test def checkPrints(): Unit =
    computeAnyGame(O, 6).foreach { g =>
      printBoards(g)
      println()
    }
