REPORT yr_snakesladders.

CLASS lcl_dice DEFINITION.

  PUBLIC SECTION.
    METHODS :
      roll_the_dice RETURNING VALUE(r_result) TYPE i.

ENDCLASS.

CLASS lcl_dice IMPLEMENTATION.

  METHOD roll_the_dice.
    r_result = cl_abap_random_int=>create( min = 1 max = 6 )->get_next(  ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_square DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING i_id TYPE i,
      get_id RETURNING VALUE(r_id) TYPE i,
      set_piece,
      remove_piece,
      is_piece_present RETURNING VALUE(r_bool) TYPE abap_bool.
  PRIVATE SECTION.
    DATA : id    TYPE i,
           piece TYPE abap_bool.

ENDCLASS.

CLASS lcl_square IMPLEMENTATION.

  METHOD constructor.
    id = i_id.
    piece = abap_false.
  ENDMETHOD.

  METHOD get_id.
    r_id = id.
  ENDMETHOD.

  METHOD is_piece_present.
    r_bool = piece.
  ENDMETHOD.

  METHOD remove_piece.
    piece = abap_false.
  ENDMETHOD.

  METHOD set_piece.
    piece = abap_true.
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
        number           = NEW lcl_dice(  )->roll_the_dice(  )
    ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_square DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA lo_square TYPE REF TO lcl_square.
    METHODS:
      setup,
      get_id FOR TESTING,
      set_piece FOR TESTING,
      remove_piece FOR TESTING.
ENDCLASS.


CLASS ltcl_square IMPLEMENTATION.

  METHOD get_id.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_square->get_id(  )
        exp                  = 15
    ).
  ENDMETHOD.

  METHOD remove_piece.
    lo_square->remove_piece(  ).
    cl_abap_unit_assert=>assert_false(
      EXPORTING
        act              = lo_square->is_piece_present(  )
    ).
  ENDMETHOD.

  METHOD set_piece.
    lo_square->set_piece(  ).
    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act              = lo_square->is_piece_present(  )
    ).
  ENDMETHOD.

  METHOD setup.
    lo_square = NEW lcl_square( 15 ).
  ENDMETHOD.

ENDCLASS.