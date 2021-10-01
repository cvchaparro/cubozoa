Feature: alarm-controller set-alarm-time
  The alarm time can be set as desired by the user.

  Scenario: Set the alarm time for a time in the future
    Given the alarm is not set
    And the time-zone is cst
    When the user sets the alarm-time to <time>
    Then the hours field of the alarm-time will be <hours>
    And the minutes field of the alarm-time will be <minutes>
    And the seconds field of the alarm-time will be <seconds>
    And the time zone field of the alarm-time will be cst
    And will publish alarm-time

    Examples:
      |     time | hours | minutes | seconds |
      |      7am |     7 |       0 |       0 |
      |     7 am |     7 |       0 |       0 |
      |     7:00 |     7 |       0 |       0 |
      |     0715 |     7 |      15 |       0 |
      |    12:00 |    12 |       0 |       0 |
      |     1200 |    12 |       0 |       0 |
      |     1300 |    13 |       0 |       0 |
      |      1pm |    13 |       0 |       0 |
      |     1 pm |    13 |       0 |       0 |
      | 14:15:16 |    14 |      15 |      16 |
      |  2:15:16 |    14 |      15 |      16 |
