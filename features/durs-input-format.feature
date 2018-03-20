Feature: durs input format

  As a user of ksquant2
  I want to be able to input scores as simple durations
  So that the input is easier to prepare than with the simple format input

  Scenario: positive dur is a chord
    Given a file named "foo.durs" with:
      """
      (0.5 0.5)
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the stdout should contain "ime 4/4 <c'>8 <c'>8 r4 r4 r4"
    Then the exit status should be 0

  Scenario: negative dur is a rest
    Given a file named "foo.durs" with:
      """
      (0.5 -0.5)
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the stdout should contain "ime 4/4 <c'>8 r8 r4 r4 r4"
    Then the exit status should be 0

  Scenario: can specify notes for dur
    Given a file named "foo.durs" with:
      """
      (0.5 (0.5 :notes (62)))
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the exit status should be 0
    And the stdout should contain "ime 4/4 <c'>8 <d'>8 r4 r4 r4"

  Scenario: can specify 2 notes for dur
    Given a file named "foo.durs" with:
      """
      (0.5 (0.5 :notes (60 62)))
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the exit status should be 0
    And the stdout should contain "ime 4/4 <c'>8 <c' d'>8 r4 r4 r4"

  Scenario: can add a tie between chords
    Given a file named "foo.durs" with:
      """
      ((0.5 :notes (60 62)) :tie (0.5 :notes (60 62)))
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the exit status should be 0
    And the stdout should contain "ime 4/4 <c' d'>4 r4 r4 r4"

  Scenario: can add a tie between 3 chords
    Given a file named "foo.durs" with:
      """
      ((0.5 :notes (60 62)) :tie (0.25 :notes (60 62)) :tie (0.25 :notes (60 62)))
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the exit status should be 0
    And the stdout should contain "ime 4/4 <c' d'>4 r4 r4 r4"

  Scenario: cannot add a tie between 2 different chords
    Given a file named "foo.durs" with:
      """
      ((0.5 :notes (61)) :tie (0.5 :notes (60 62)))
      """
    When I run `ksquant2 -r durs -w ly foo.durs`
    Then the exit status should be 1
