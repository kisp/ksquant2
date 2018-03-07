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
