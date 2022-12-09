package daysix

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DaySixSpec extends AnyFreeSpec {

  "start-of-packet marker" - {
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb -> 7" in {
      DaySix.doTheThing(0, Nil)(4)("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 7
    }
    "bvwbjplbgvbhsrlpgdmjqwftvncz -> 5 " in {
      DaySix.doTheThing(0, Nil)(4)("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 5
    }
    "nppdvjthqldpwncqszvftbrmjlhg -> 6 " in {
      DaySix.doTheThing(0, Nil)(4)("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 6
    }
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg -> 10 " in {
      DaySix.doTheThing(0, Nil)(4)("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 10
    }
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw -> 11 " in {
      DaySix.doTheThing(0, Nil)(4)("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 11
    }
  }

  "start-of-message marker" - {
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb -> 19" in {
      DaySix.doTheThing(0, Nil)(14)("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 19
    }
    "bvwbjplbgvbhsrlpgdmjqwftvncz -> 23" in {
      DaySix.doTheThing(0, Nil)(14)("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 23
    }
    "nppdvjthqldpwncqszvftbrmjlhg -> 23" in {
      DaySix.doTheThing(0, Nil)(14)("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 23
    }
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg -> 29" in {
      DaySix.doTheThing(0, Nil)(14)("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 29
    }
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw -> 26" in {
      DaySix.doTheThing(0, Nil)(14)("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 26
    }
  }
}
