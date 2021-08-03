REPORT yr_snakesladders.

CLASS lcl_dice DEFINITION.

  PUBLIC SECTION.
    METHODS :
      roll_dice RETURNING VALUE(r_result) TYPE i.

ENDCLASS.

CLASS lcl_dice IMPLEMENTATION.

  METHOD roll_dice.
    r_result = cl_abap_random_int=>create( min = 1 max = 6 )->get_next(  ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_snakes_ladders DEFINITION.

  PUBLIC SECTION.
    METHODS play IMPORTING die1 TYPE i
                           die2 TYPE i.

    METHODS get_current_square RETURNING VALUE(r_result) TYPE i.

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

CLASS ltcl_dice DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      roll_dice FOR TESTING.
ENDCLASS.


CLASS ltcl_dice IMPLEMENTATION.

  METHOD roll_dice.
    cl_abap_unit_assert=>assert_number_between(
      EXPORTING
        lower            = 1
        upper            = 6
        number           = NEW lcl_dice(  )->roll_dice(  )
    ).

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
      play_one_move FOR TESTING.
ENDCLASS.


CLASS ltcl_snakes_ladders IMPLEMENTATION.

  METHOD setup.
    lo_snakes_ladders = NEW #(  ).
  ENDMETHOD.

  METHOD acceptance_test.

    DATA lv_sum_die TYPE i.
    DATA(lo_dice1) = NEW lcl_dice(  ).
    DATA(lo_dice2) = NEW lcl_dice(  ).

    DO 7 TIMES.
      DATA(lv_die1) =  lo_dice1->roll_dice(  ).
      DATA(lv_die2) =  lo_dice2->roll_dice(  ).
      lo_snakes_ladders->play( die1 = lv_die1 die2 = lv_die2 ).
      lv_sum_die = lv_sum_die + lv_die1 + lv_die2.
    ENDDO.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_snakes_ladders->get_current_square(  )
        exp                  = lv_sum_die

    ).

  ENDMETHOD.

  METHOD play_one_move.

    DATA lv_sum_die TYPE i.
    DATA(lo_dice1) = NEW lcl_dice(  ).
    DATA(lo_dice2) = NEW lcl_dice(  ).

    DATA(lv_die1) =  lo_dice1->roll_dice(  ).
    DATA(lv_die2) =  lo_dice2->roll_dice(  ).
    lo_snakes_ladders->play( die1 = lv_die1 die2 = lv_die2 ).
    lv_sum_die = lv_die1 + lv_die2.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_snakes_ladders->get_current_square(  )
        exp                  = lv_sum_die

    ).
  ENDMETHOD.

ENDCLASS.