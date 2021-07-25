REPORT yr_game_of_life_nme.

CLASS lcl_state DEFINITION.

  PUBLIC SECTION.
    CONSTANTS :
      none  TYPE i VALUE '0',
      alive TYPE i VALUE '1',
      died  TYPE i VALUE '2'.

    METHODS :
      constructor IMPORTING i_state TYPE i,
      is_alive RETURNING VALUE(r_bool) TYPE abap_bool,
      is_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_died RETURNING VALUE(r_bool) TYPE abap_bool.

  PRIVATE SECTION.
    DATA state TYPE i.
ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD constructor.
    state = i_state.
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

  METHOD is_none.
    IF state = lcl_state=>none.
      r_bool = abap_true.
    ELSEIF state <> lcl_state=>none.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_random_cell_generator DEFINITION.
  PUBLIC SECTION.
    METHODS :
      generate_state_cell RETURNING VALUE(r_state) TYPE REF TO lcl_state.
ENDCLASS.

CLASS lcl_random_cell_generator IMPLEMENTATION.
  METHOD generate_state_cell.
    r_state = NEW #( cl_abap_random_int=>create( min = 0 max = 2 )->get_next(  )  ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cell DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING i_state TYPE REF TO lcl_state,
      is_cell_alive RETURNING VALUE(r_bool) TYPE abap_bool,
      is_cell_none RETURNING VALUE(r_bool) TYPE abap_bool,
      is_cell_died RETURNING VALUE(r_bool) TYPE abap_bool.

  PRIVATE SECTION.
    DATA state TYPE REF TO lcl_state.

ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.

  METHOD constructor.
    state = NEW lcl_random_cell_generator(  )->generate_state_cell(  ).
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