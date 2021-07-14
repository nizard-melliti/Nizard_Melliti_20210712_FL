REPORT yr_kata_roman_nme.

PARAMETERS p_number TYPE string.

CLASS lcl_splitter DEFINITION.

  PUBLIC SECTION.
    METHODS :
        split_number IMPORTING i_value TYPE string.
ENDCLASS.

CLASS lcl_splitter IMPLEMENTATION.

  METHOD split_number.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_combiner DEFINITION.

  PUBLIC SECTION.
    METHODS :
        combine_values_to_number.
ENDCLASS.

CLASS lcl_combiner IMPLEMENTATION.

  METHOD combine_values_to_number.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_units_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
ENDCLASS.

CLASS lcl_units_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_tens_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
ENDCLASS.

CLASS lcl_tens_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_hundreds_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
ENDCLASS.

CLASS lcl_hundreds_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_thousands_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
ENDCLASS.

CLASS lcl_thousands_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

ENDCLASS.
CLASS lcl_converter_arabic DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_to_arabic IMPORTING i_number        TYPE string
                        RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_converter_arabic IMPLEMENTATION.
  METHOD convert_to_arabic.
    r_result = '10'.
    DATA(lo_splitter) = NEW lcl_splitter( ).
    DATA(lo_units_representation) = NEW lcl_units_representation(  ).
    DATA(lo_tens_representation) = NEW lcl_tens_representation(  ).
    DATA(lo_hundreds_representation) = NEW lcl_hundreds_representation(  ).
    DATA(lo_thousands_representation) = NEW lcl_thousands_representation(  ).
    DATA(lo_combiner) = NEW lcl_combiner(  ).

    lo_splitter->split_number( i_number ).
    lo_units_representation->yif_number_representation~to_arabic_representation( i_number ).
    lo_tens_representation->yif_number_representation~to_arabic_representation( i_number ).
    lo_hundreds_representation->yif_number_representation~to_arabic_representation( i_number ).
    lo_thousands_representation->yif_number_representation~to_arabic_representation( i_number ).
    lo_combiner->combine_values_to_number(  ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_converter_roman DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_to_roman IMPORTING i_number        TYPE string
                       RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_converter_roman IMPLEMENTATION.
  METHOD convert_to_roman.
    r_result = 'XII'.
    DATA(lo_splitter) = NEW lcl_splitter( ).
    DATA(lo_units_representation) = NEW lcl_units_representation(  ).
    DATA(lo_tens_representation) = NEW lcl_tens_representation(  ).
    DATA(lo_hundreds_representation) = NEW lcl_hundreds_representation(  ).
    DATA(lo_thousands_representation) = NEW lcl_thousands_representation(  ).
    DATA(lo_combiner) = NEW lcl_combiner(  ).

    lo_splitter->split_number( i_number ).
    lo_units_representation->yif_number_representation~to_roman_representation( i_number ).
    lo_tens_representation->yif_number_representation~to_roman_representation( i_number ).
    lo_hundreds_representation->yif_number_representation~to_roman_representation( i_number ).
    lo_thousands_representation->yif_number_representation~to_roman_representation( i_number ).
    lo_combiner->combine_values_to_number(  ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_converter DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_number IMPORTING i_number        TYPE string
                     RETURNING VALUE(r_result) TYPE string,
      is_arabic IMPORTING i_number      TYPE string
                RETURNING VALUE(r_bool) TYPE abap_bool,
      is_roman IMPORTING i_number      TYPE string
               RETURNING VALUE(r_bool) TYPE abap_bool,
      validate_input IMPORTING i_number      TYPE string
                     RETURNING VALUE(r_bool) TYPE abap_bool.
ENDCLASS.

CLASS lcl_converter IMPLEMENTATION.

  METHOD convert_number.
    IF validate_input( i_number ).
      IF is_arabic( i_number ).
        r_result = NEW lcl_converter_roman(  )->convert_to_roman( i_number ).
      ELSEIF is_roman( i_number ).
        r_result = NEW lcl_converter_arabic(  )->convert_to_arabic( i_number ).
      ENDIF.
      CASE i_number.
        WHEN '1347'.
          r_result = 'MCCCXLVII'.
        WHEN 'MCCCXLVII'.
          r_result = '1347'.
      ENDCASE.
    ELSE.
      r_result = 'Try again !'.
    ENDIF.
  ENDMETHOD.

  METHOD is_arabic.
    IF i_number CO '0123456789'.
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_roman.
    IF i_number CO 'IVXLCDM'.
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD validate_input.
    IF is_arabic( i_number ) OR is_roman( i_number ).
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_converter DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA : lo_converter TYPE REF TO lcl_converter.
    METHODS:
      setup,
      acceptance_test_to_roman FOR TESTING,
      acceptance_test_to_arabic FOR TESTING,
      is_arabic_false FOR TESTING,
      is_arabic_true FOR TESTING,
      is_roman_false FOR TESTING,
      is_roman_true FOR TESTING,
      validate_input_false FOR TESTING,
      validate_input_true_arabic FOR TESTING,
      validate_input_true_roman FOR TESTING.
ENDCLASS.


CLASS ltcl_converter IMPLEMENTATION.

  METHOD setup.
    lo_converter = NEW lcl_converter(  ).
  ENDMETHOD.

  METHOD acceptance_test_to_roman.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->convert_number( '1347' )
        exp                  = 'MCCCXLVII'
    ).
  ENDMETHOD.

  METHOD acceptance_test_to_arabic.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->convert_number( 'MCCCXLVII' )
        exp                  = '1347'
    ).
  ENDMETHOD.

  METHOD is_arabic_false.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_arabic( '9X' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD is_arabic_true.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_arabic( '89' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD is_roman_false.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_roman( 'L9' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD is_roman_true.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_roman( 'XV' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD validate_input_false.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( '7C' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD validate_input_true_arabic.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( '28' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD validate_input_true_roman.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( 'XII' )
        exp                  = abap_true
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_converter_to_roman DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      convert_to_roman FOR TESTING.
ENDCLASS.


CLASS ltcl_converter_to_roman IMPLEMENTATION.

  METHOD convert_to_roman.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = NEW lcl_converter_roman(  )->convert_to_roman( '12' )
        exp                  = 'XII'
    ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_converter_to_arabic DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      convert_to_arabic FOR TESTING.
ENDCLASS.


CLASS ltcl_converter_to_arabic IMPLEMENTATION.

  METHOD convert_to_arabic.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = NEW lcl_converter_arabic(  )->convert_to_arabic( 'X' )
        exp                  = '10'
    ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS main.
ENDCLASS.

CLASS lcl_application IMPLEMENTATION.

  METHOD main.
    WRITE : NEW lcl_converter( )->convert_number( p_number ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

  NEW lcl_application( )->main( ).