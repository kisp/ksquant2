Feature: informational

  As a user of ksquant2
  I want to be able to query useful information about the quantizer

  Scenario: print information
    When I run `ksquant2 --version`
    Then the exit status should be 0
    And the output should contain "KSQuant2"
    And the output should contain "0.2.1"

  Scenario: xxx
    When I run `ksquant2 -x`
    Then the exit status should be 1
    When I run `ksquant2 -h`
    Then the exit status should be 0
    When I run `ksquant2 -v -h`
    Then the exit status should be 0
