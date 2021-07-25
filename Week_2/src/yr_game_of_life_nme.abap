REPORT yr_game_of_life_nme.

CLASS lcl_state DEFINITION.
  PUBLIC SECTION.
    CONSTANTS :
      none  TYPE i VALUE '0',
      alive TYPE i VALUE '1',
      died  TYPE i VALUE '2'.

    METHODS :
      constructor IMPORTING i_state TYPE i,
      is_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_alive RETURNING VALUE(r_bool) TYPE abap_bool,
      is_died RETURNING VALUE(r_bool) TYPE abap_bool.

  PRIVATE SECTION.
    DATA state TYPE i.
ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD constructor.
    state = i_state.
  ENDMETHOD.

  METHOD is_none.
    IF state = lcl_state=>none.
      r_bool = abap_true.
    ELSEIF state <> lcl_state=>none.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_alive.
    IF state = lcl_state=>alive.
      r_bool = abap_true.
    ELSEIF state <> lcl_state=>alive.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_died.
    IF state = lcl_state=>died.
      r_bool = abap_true.
    ELSEIF state <> lcl_state=>died.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cell_generator DEFINITION.
  PUBLIC SECTION.
    METHODS :
      generate_state_cell RETURNING VALUE(r_state) TYPE REF TO lcl_state.
ENDCLASS.

CLASS lcl_cell_generator IMPLEMENTATION.
  METHOD generate_state_cell.
    r_state = NEW #( cl_abap_random_int=>create( min = 0 max = 2 )->get_next(  )  ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cell DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor,
      is_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_alive RETURNING VALUE(r_bool) TYPE abap_bool,
      is_died RETURNING VALUE(r_bool) TYPE abap_bool.

  PRIVATE SECTION.
    DATA state TYPE REF TO lcl_state.

ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.

  METHOD constructor.
    state = NEW lcl_cell_generator(  )->generate_state_cell(  ).
  ENDMETHOD.

  METHOD is_none.
    r_bool = state->is_none(  ).
  ENDMETHOD.

  METHOD is_alive.
    r_bool = state->is_alive(  ).
  ENDMETHOD.

  METHOD is_died.
    r_bool = state->is_died(  ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_state DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      is_none_true FOR TESTING,
      is_none_false FOR TESTING,
      is_alive_true FOR TESTING,
      is_alive_false FOR TESTING,
      is_died_true FOR TESTING,
      is_died_false FOR TESTING.
ENDCLASS.

CLASS ltcl_state IMPLEMENTATION.

  METHOD is_none_true.
    cl_abap_unit_assert=>assert_true(
      act  = NEW lcl_state( 0 )->is_none(  ) ).
  ENDMETHOD.

  METHOD is_none_false.
    cl_abap_unit_assert=>assert_false(
      act  = NEW lcl_state( 1 )->is_none(  ) ).
  ENDMETHOD.

  METHOD is_alive_true.
    cl_abap_unit_assert=>assert_true(
      act  = NEW lcl_state( 1 )->is_alive(  ) ).
  ENDMETHOD.

  METHOD is_alive_false.
    cl_abap_unit_assert=>assert_false(
      act  = NEW lcl_state( 0 )->is_alive(  ) ).
  ENDMETHOD.

  METHOD is_died_false.
    cl_abap_unit_assert=>assert_false(
      act  = NEW lcl_state( 0 )->is_died(  ) ).
  ENDMETHOD.

  METHOD is_died_true.
    cl_abap_unit_assert=>assert_true(
      act  = NEW lcl_state( 2 )->is_died(  ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cell_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      generate_state_cell FOR TESTING.
ENDCLASS.


CLASS ltcl_cell_generator IMPLEMENTATION.

  METHOD generate_state_cell.

    TYPES tyt_state TYPE STANDARD TABLE OF REF TO lcl_state WITH NON-UNIQUE KEY table_line.
    DATA(dref) = NEW tyt_state( ( NEW lcl_state( 0 ) ) ( NEW lcl_state( 1 ) ) ( NEW lcl_state( 2 ) ) ).
    ASSIGN dref->* TO FIELD-SYMBOL(<itab>).

    cl_abap_unit_assert=>assert_table_contains(
      line  = NEW lcl_cell_generator(  )->generate_state_cell(  )
      table = <itab> ).

  ENDMETHOD.

ENDCLASS.