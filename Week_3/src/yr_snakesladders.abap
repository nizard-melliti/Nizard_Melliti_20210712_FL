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

CLASS lcl_square DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING i_id TYPE i,
      set_piece,
      remove_piece,
      is_piece_present RETURNING VALUE(r_bool) TYPE abap_bool,
      get_id RETURNING VALUE(r_id) TYPE i.
  PRIVATE SECTION.
    DATA : piece TYPE abap_bool,
           id    TYPE i.
ENDCLASS.

CLASS lcl_square IMPLEMENTATION.

  METHOD constructor.
    piece = abap_false.
    id = i_id.
  ENDMETHOD.

  METHOD get_id.
    r_id = id.
  ENDMETHOD.

  METHOD set_piece.
    piece = abap_true.
  ENDMETHOD.

  METHOD remove_piece.
    piece = abap_false.
  ENDMETHOD.

  METHOD is_piece_present.
    r_bool = piece.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_snake_ladders DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor,
      play IMPORTING i_die1 TYPE i
                     i_die2 TYPE i,
      get_current_position RETURNING VALUE(r_result) TYPE i,
      set_new_position IMPORTING i_new_position TYPE i,
      remove_old_position IMPORTING i_current_position TYPE i.
  PRIVATE SECTION.
    DATA : game_board TYPE TABLE OF REF TO lcl_square.

ENDCLASS.

CLASS lcl_snake_ladders IMPLEMENTATION.

  METHOD constructor.
    DO 100 TIMES.
      APPEND NEW lcl_square( sy-index ) TO game_board.
    ENDDO.
  ENDMETHOD.

  METHOD play.
    DATA(lv_current_position) = get_current_position(  ).
    DATA(lv_new_position) = lv_current_position + i_die1 + i_die2.
    remove_old_position( lv_current_position ).
    set_new_position( lv_new_position ).
  ENDMETHOD.

  METHOD get_current_position.
    LOOP AT game_board INTO DATA(lo_square).
      IF lo_square->is_piece_present(  ).
        r_result = sy-tabix.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_new_position.
    READ TABLE game_board ASSIGNING FIELD-SYMBOL(<fs_square>) INDEX i_new_position.
    IF sy-subrc = 0.
      <fs_square>->set_piece(  ).
    ENDIF.
  ENDMETHOD.


  METHOD remove_old_position.
    READ TABLE game_board ASSIGNING FIELD-SYMBOL(<fs_square>) INDEX i_current_position.
    IF sy-subrc = 0.
      <fs_square>->remove_piece(  ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_snake_ladders DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      acceptance_test FOR TESTING.
ENDCLASS.


CLASS ltcl_snake_ladders IMPLEMENTATION.

  METHOD acceptance_test.

    DATA lv_sum_die TYPE i.

    DATA(lo_snakes_ladders) = NEW lcl_snake_ladders(  ).
    DATA(lo_dice1) = NEW lcl_dice(  ).
    DATA(lo_dice2) = NEW lcl_dice(  ).

    DO 7 TIMES.
      DATA(lv_die1) =  lo_dice1->roll_dice(  ).
      DATA(lv_die2) =  lo_dice2->roll_dice(  ).
      lo_snakes_ladders->play( i_die1 = lv_die1 i_die2 = lv_die2 ).
      lv_sum_die = lv_sum_die + lv_die1 + lv_die2.
    ENDDO.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_snakes_ladders->get_current_position(  )
        exp                  = lv_sum_die

    ).
  ENDMETHOD.

ENDCLASS.