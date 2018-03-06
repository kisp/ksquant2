Feature: cli conventions

  As a user of ksquant2
  I want to apply common cli conventions

  Scenario: passing files by name
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
      :max-div 8 :forbidden-divs nil)
      """
    When I run `ksquant2 -w ly foo.simple out.ly`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      r4 <c'>4 <c'>4 r4
      """

  Scenario: passing files by pipes
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
      :max-div 8 :forbidden-divs nil)
      """
    When I run `bash -c 'ksquant2 -w ly <foo.simple >out.ly'`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      r4 <c'>4 <c'>4 r4
      """

  Scenario: using - for stdin
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
      :max-div 8 :forbidden-divs nil)
      """
    When I run `bash -c '<foo.simple ksquant2 -w ly - out.ly'`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      r4 <c'>4 <c'>4 r4
      """

  Scenario: piping into output file
    Given a file named "foo.simple" with:
      """
      (:simple (((1 2 3))) :time-signatures (4 4) :metronomes (4 60)
      :max-div 8 :forbidden-divs nil)
      """
    When I run `bash -c 'ksquant2 -w ly foo.simple >out.ly'`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      r4 <c'>4 <c'>4 r4
      """
