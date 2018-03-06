Feature: errors

  As a user of ksquant2
  I want to be alerted of exceptional situations

  Scenario: no simple
    Given a file named "foo.simple" with:
      """
      (:time-signatures (4 4) :metronomes (4 60)
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1

  Scenario: no time-signatures
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :metronomes (4 60)
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "Could not find :time-signatures"

  Scenario Outline: invalid time-signatures
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :metronomes (4 60) :time-signatures <ts>
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "<message>"

    Examples:
    | ts | message                                 |
    | () | ensureListOfLists: empty list           |
    | 3  | ensureListOfLists: not a list: readLisp |

  Scenario: no metronomes
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4)
       :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "Could not find :metronomes"

  Scenario: no max-div
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
       :forbidden-divs nil)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "Could not find :max-div"

  Scenario: no forbidden-divs
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
       :max-div 8)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "Could not find :forbidden-divs"

  Scenario Outline: invalid forbidden-divs
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :metronomes (4 60) :time-signatures (4 4)
       :max-div 8 :forbidden-divs <fbdivs>)
      """
    When I run `ksquant2 foo.simple`
    Then the exit status should be 1
    And the stderr should contain "<message>"

    Examples:
    | fbdivs  | message                        |
    | 3       | ensureListOfIntegers: readLisp |
    | ("foo") | ensureInt: readLisp            |
