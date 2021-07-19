REPORT yr_game_of_life_nme.

CLASS lcl_random_generator DEFINITION.
  PUBLIC SECTION.
    METHODS :
      random_generator_cell_state RETURNING VALUE(r_result) TYPE i.
ENDCLASS.

CLASS lcl_random_generator IMPLEMENTATION.
  METHOD random_generator_cell_state.
    r_result = cl_abap_random_int=>create( min = 0 max = 2 )->get_next(  ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_state DEFINITION.
  PUBLIC SECTION.
    CONSTANTS :
      c_none  TYPE i VALUE '0',
      c_died  TYPE i VALUE '1',
      c_alive TYPE i VALUE '2'.
    METHODS :
      constructor IMPORTING i_state TYPE i,
      change_state IMPORTING i_state TYPE i.
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
ENDCLASS.

CLASS lcl_cell DEFINITION.
  PUBLIC SECTION.
    METHODS :
      constructor,
      change_cell_state IMPORTING i_state TYPE i.
  PRIVATE SECTION.
    DATA go_state TYPE REF TO lcl_state.
ENDCLASS.

CLASS lcl_cell IMPLEMENTATION.
  METHOD constructor.
    go_state = NEW lcl_state( NEW lcl_random_generator(  )->random_generator_cell_state( ) ).
  ENDMETHOD.
  METHOD change_cell_state.
    go_state->change_state( i_state ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_game_board DEFINITION.
  PUBLIC SECTION.
    METHODS :
      constructor.
  PRIVATE SECTION.
    TYPES : BEGIN OF ty_cells,
              cell1 TYPE REF TO lcl_cell,
              cell2 TYPE REF TO lcl_cell,
              cell3 TYPE REF TO lcl_cell,
            END OF ty_cells,
            tty_cells TYPE TABLE OF ty_cells.
    DATA : t_cells TYPE tty_cells.
ENDCLASS.

CLASS lcl_game_board IMPLEMENTATION.
  METHOD constructor.
    DATA ls_cells TYPE ty_cells.
    DO 3 TIMES.
      ls_cells-cell1 = NEW #(  ).
      ls_cells-cell2 = NEW #(  ).
      ls_cells-cell3 = NEW #(  ).
      APPEND ls_cells TO t_cells.
      FREE : ls_cells-cell1, ls_cells-cell2, ls_cells-cell3.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_game_of_life DEFINITION.
  PUBLIC SECTION.
  PRIVATE SECTION.
    DATA game_board TYPE REF TO lcl_game_board.
ENDCLASS.

CLASS lcl_game_of_life IMPLEMENTATION.

ENDCLASS.