REPORT yr_snakesladders.

CLASS lcl_snakes_ladders DEFINITION.

  PUBLIC SECTION.
    METHODS play
      IMPORTING
        die1 TYPE i
        die2 TYPE i.

    METHODS get_current_square
      RETURNING
        VALUE(r_result) TYPE i.

  PRIVATE SECTION.
    DATA current_square TYPE i.

ENDCLASS.

CLASS lcl_snakes_ladders IMPLEMENTATION.

  METHOD play.
    current_square = current_square + die1 + die2.
  ENDMETHOD.

  METHOD get_current_square.
    r_result = current_square.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_snakes_ladders DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA lo_snakes_ladders TYPE REF TO lcl_snakes_ladders.
    METHODS:
      setup,
      acceptance_test FOR TESTING,
      play_1_move FOR TESTING.
ENDCLASS.


CLASS ltcl_snakes_ladders IMPLEMENTATION.

  METHOD acceptance_test.

    lo_snakes_ladders->play( die1 = 1 die2 = 4 ).
    lo_snakes_ladders->play( die1 = 3 die2 = 6 ).
    lo_snakes_ladders->play( die1 = 4 die2 = 2 ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_snakes_ladders->get_current_square(  )
        exp                  = 20

    ).

  ENDMETHOD.

  METHOD play_1_move.
    lo_snakes_ladders->play( die1 = 1 die2 = 4 ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_snakes_ladders->get_current_square(  )
        exp                  = 5

    ).
  ENDMETHOD.

  METHOD setup.
    lo_snakes_ladders = NEW #(  ).
  ENDMETHOD.

ENDCLASS.