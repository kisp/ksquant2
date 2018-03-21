Feature: durs output format

  As a user of ksquant2
  I want to be able to output scores as simple durations
  So that the output is easier to read than enp format

  Scenario: 4 quarters
    Given a file named "foo.durs" with:
      """
      (1 1 1 1)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1 1 1 1)"
    Then the exit status should be 0

  Scenario: 8 eighths notes
    Given a file named "foo.durs" with:
      """
      (0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2)"
    Then the exit status should be 0

  Scenario: 16 sixteenth notes
    Given a file named "foo.durs" with:
      """
      (0.25 0.25 0.25 0.25
       0.25 0.25 0.25 0.25
       0.25 0.25 0.25 0.25
       0.25 0.25 0.25 0.25)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4)"
    Then the exit status should be 0

  Scenario: 3 quarters and a rest
    Given a file named "foo.durs" with:
      """
      (1 1 1 -1)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1 1 1 -1)"
    Then the exit status should be 0

  Scenario: a tie
    Given a file named "foo.durs" with:
      """
      (5)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1 :TIE 1 :TIE 1 :TIE 1 :TIE 1 -1 -1 -1)"
    Then the exit status should be 0

  Scenario: 2 parts
    Given a file named "foo.durs" with:
      """
      (1 1 1 1)
      (1 1 -1 -1)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain "(1 1 1 1)"
    And the stdout should contain "(1 1 -1 -1)"
    And the exit status should be 0

  Scenario: output as individual forms
    Given a file named "foo.durs" with:
      """
      (1 1 1 1)
      (1 1 -1 -1)
      """
    When I run `ksquant2 -r durs -w durs foo.durs`
    Then the stdout should contain:
    """
    (1 1 1 1)
    (1 1 -1 -1)
    """
    And the exit status should be 0
