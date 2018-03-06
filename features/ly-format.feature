Feature: ly format
  As a user of ksquant2
  I want to be able to output scores in ly format
  So that I can process them with LilyPond

  Scenario: lily
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
      :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple`
    Then the stdout should contain "r4 <c'>4 <c'>4 r4"
    Then the exit status should be 0

  Scenario: lily 2
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 2.5 3))) :time-signatures (4 4) :metronomes (4 60)
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple`
    Then the stdout should contain "r4 <c'>4 <c'>8 <c'>8 r4"
    Then the exit status should be 0

  Scenario: lily 3
    Given a file named "foo.simple" with:
      """
      (:simple (((0 4))) :time-signatures (4 4) :metronomes (4 60)
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple`
    Then the stdout should contain "<c'>4~ <c'>4~ <c'>4~ <c'>4"
    Then the exit status should be 0

  Scenario Outline: one duration 1//4
    Given a file named "foo.simple" with:
      """
      (:simple (((0 <in>))) :time-signatures (1 4) :metronomes (4 60)
       :max-div 32 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple`
    Then the stdout should contain "<out>"
    Then the exit status should be 0

    Examples:
      | in   | out     |
      | 1    | <c'>4   |
      | 1/2  | <c'>8   |
      | 1/4  | <c'>16  |
      | 1/8  | <c'>32  |
      | 1/16 | <c'>64  |
      | 1/32 | <c'>128 |

  Scenario Outline: one duration 1//1
    Given a file named "foo.simple" with:
      """
      (:simple (((0 <in>))) :time-signatures (1 1) :metronomes (4 60)
       :max-div 4 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple`
    Then the stdout should contain "<out>"
    Then the exit status should be 0

    Examples:
      | in | out   |
      |  4 | <c'>1 |
      |  2 | <c'>2 |
