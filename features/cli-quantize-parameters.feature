Feature: cli quantize parameters

  As a user of ksquant2
  I want to specify quantization parameters via the command line

  Scenario: specifying max-div
    Given a file named "foo.simple" with:
      """
      (1/3 1/3 1/3 -1 -1)
      """
    When I run `ksquant2 -r durs -w ly --max-div 2 foo.simple out.ly`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      <c'>8 <c'>8 r4 r4 r4
      """

  Scenario: specifying forbidden-divs
    Given a file named "foo.simple" with:
      """
      (1/3 1/3 1/3 -1 -1)
      """
    When I run `ksquant2 -r durs -w ly --max-div 4 --forbidden-divs '(3 4)' foo.simple out.ly`
    Then the exit status should be 0
    And the file "out.ly" should contain:
      """
      <c'>8 <c'>8 r4 r4 r4
      """
