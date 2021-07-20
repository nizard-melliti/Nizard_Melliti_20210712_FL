REPORT yr_game_of_life_nme.

CLASS lcl_random_generator DEFINITION.
  PUBLIC SECTION.
    METHODS :
      generate_random_cell_state RETURNING VALUE(r_result) TYPE i.
ENDCLASS.

CLASS lcl_random_generator IMPLEMENTATION.

  METHOD generate_random_cell_state.
    r_result = cl_abap_random_int=>create( min = 0 max = 2 )->get_next(  ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_state DEFINITION.
  PUBLIC SECTION.
    CONSTANTS :
      none  TYPE i VALUE 0,
      died  TYPE i VALUE 1,
      alive TYPE i VALUE 2.
    METHODS :
      constructor IMPORTING i_state TYPE i,
      change_state IMPORTING i_state TYPE i,
      is_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_died RETURNING VALUE(r_bool) TYPE abap_bool,
      is_alive RETURNING VALUE(r_bool) TYPE abap_bool.
  PRIVATE SECTION.
    DATA state TYPE i.
ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD constructor.
    state = i_state.
  ENDMETHOD.

  METHOD change_state.
    state = i_state.
  ENDMETHOD.

  METHOD is_alive.
    IF state = alive.
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_died.
    IF state = died.
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_none.
    IF state = none.
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_cell DEFINITION.
  PUBLIC SECTION.
    METHODS :
      constructor,
      change_cell_state IMPORTING i_state TYPE i,
      is_cell_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_cell_died RETURNING VALUE(r_bool) TYPE abap_bool,
      is_cell_alive RETURNING VALUE(r_bool) TYPE abap_bool.
  PRIVATE SECTION.
    DATA state TYPE REF TO lcl_state.
ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.

  METHOD constructor.
    state = NEW #( NEW lcl_random_generator(  )->generate_random_cell_state( ) ).
  ENDMETHOD.

  METHOD change_cell_state.
    state->change_state( i_state ).
  ENDMETHOD.

  METHOD is_cell_alive.
    r_bool = state->is_alive(  ).
  ENDMETHOD.

  METHOD is_cell_died.
    r_bool = state->is_died(  ).
  ENDMETHOD.

  METHOD is_cell_none.
    r_bool = state->is_none(  ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_random_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      generate_random_cell_state FOR TESTING.
ENDCLASS.


CLASS ltcl_random_generator IMPLEMENTATION.

  METHOD generate_random_cell_state.
    cl_abap_unit_assert=>assert_number_between(
      lower  = 0
      upper  = 2
      number = NEW lcl_random_generator(  )->generate_random_cell_state( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_state DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA lo_state TYPE REF TO lcl_state.
    METHODS:
      setup,
      change_state_to_alive FOR TESTING,
      change_state_to_died FOR TESTING.
ENDCLASS.

CLASS ltcl_state IMPLEMENTATION.

  METHOD setup.
    lo_state = NEW lcl_state( lcl_state=>none ).
  ENDMETHOD.

  METHOD change_state_to_alive.
    lo_state->change_state( lcl_state=>alive ).
    cl_abap_unit_assert=>assert_true(
      act  = lo_state->is_alive(  ) ).
  ENDMETHOD.

  METHOD change_state_to_died.
    lo_state->change_state( lcl_state=>died ).
    cl_abap_unit_assert=>assert_true(
      act  = lo_state->is_died(  ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_cell DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA : lo_cell TYPE REF TO lcl_cell.
    METHODS:
      setup,
      change_cell_state_to_alive FOR TESTING,
      change_cell_state_to_died FOR TESTING,
      change_cell_state_to_none FOR TESTING.
ENDCLASS.


CLASS ltcl_cell IMPLEMENTATION.

  METHOD setup.
    lo_cell = NEW #(  ).
  ENDMETHOD.

  METHOD change_cell_state_to_alive.
    lo_cell->change_cell_state( lcl_state=>alive ).
    cl_abap_unit_assert=>assert_true(
      act  = lo_cell->is_cell_alive(  ) ).
  ENDMETHOD.

  METHOD change_cell_state_to_died.
    lo_cell->change_cell_state( lcl_state=>died ).
    cl_abap_unit_assert=>assert_true(
      act  = lo_cell->is_cell_died(  ) ).
  ENDMETHOD.

  METHOD change_cell_state_to_none.
    lo_cell->change_cell_state( lcl_state=>none ).
    cl_abap_unit_assert=>assert_true(
      act  = lo_cell->is_cell_none(  ) ).
  ENDMETHOD.

ENDCLASS.