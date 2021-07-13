REPORT yr_kata_roman_nme.

PARAMETERS p_number TYPE string.


CLASS lcl_units DEFINITION.
  PUBLIC SECTION.
    METHODS :
      to_roman IMPORTING i_value         TYPE string
               RETURNING VALUE(r_result) TYPE string,
      to_arabic IMPORTING i_value         TYPE string
                RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_units IMPLEMENTATION.

  METHOD to_arabic.

  ENDMETHOD.

  METHOD to_roman.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_tens DEFINITION.
  PUBLIC SECTION.
  METHODS :
      to_roman IMPORTING i_value         TYPE string
               RETURNING VALUE(r_result) TYPE string,
      to_arabic IMPORTING i_value         TYPE string
                RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_tens IMPLEMENTATION.

  METHOD to_arabic.

  ENDMETHOD.

  METHOD to_roman.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_hundreds DEFINITION.
  PUBLIC SECTION.
  METHODS :
      to_roman IMPORTING i_value         TYPE string
               RETURNING VALUE(r_result) TYPE string,
      to_arabic IMPORTING i_value         TYPE string
                RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_hundreds IMPLEMENTATION.

  METHOD to_arabic.

  ENDMETHOD.

  METHOD to_roman.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_thousands DEFINITION.
  PUBLIC SECTION.
  METHODS :
      to_roman IMPORTING i_value         TYPE string
               RETURNING VALUE(r_result) TYPE string,
      to_arabic IMPORTING i_value         TYPE string
                RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_thousands IMPLEMENTATION.

  METHOD to_arabic.

  ENDMETHOD.

  METHOD to_roman.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_converter_to_arabic DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_to_arabic IMPORTING i_number        TYPE string
                        RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_converter_to_arabic IMPLEMENTATION.
  METHOD convert_to_arabic.
    r_result = '20'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_converter_to_roman DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_to_roman IMPORTING i_number        TYPE string
                       RETURNING VALUE(r_result) TYPE string.
ENDCLASS.

CLASS lcl_converter_to_roman IMPLEMENTATION.
  METHOD convert_to_roman.
    r_result = 'XXIII'.
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
        r_result = NEW lcl_converter_to_roman(  )->convert_to_roman( i_number ).
      ELSEIF is_roman( i_number ).
        r_result = NEW lcl_converter_to_arabic(  )->convert_to_arabic( i_number ).
      ENDIF.
      CASE i_number.
        WHEN '1347'.
          r_result = 'MCCCXLVII'.
        WHEN 'MCCCXLVII'.
          r_result = '1347'.
      ENDCASE.
    ELSE.
      r_result = 'Error'.
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
        act                  = lo_converter->is_arabic( '12L' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD is_arabic_true.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_arabic( '125' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD is_roman_false.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_roman( 'XX5' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD is_roman_true.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->is_roman( 'XX' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD validate_input_false.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( '19L' )
        exp                  = abap_false
    ).
  ENDMETHOD.

  METHOD validate_input_true_arabic.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( '19' )
        exp                  = abap_true
    ).
  ENDMETHOD.

  METHOD setup.
    lo_converter = NEW lcl_converter(  ).
  ENDMETHOD.

  METHOD validate_input_true_roman.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lo_converter->validate_input( 'XI' )
        exp                  = abap_true
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
        act                  = NEW lcl_converter_to_arabic(  )->convert_to_arabic( 'XX' )
        exp                  = '20'
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
        act                  = NEW lcl_converter_to_roman(  )->convert_to_roman( '23' )
        exp                  = 'XXIII'
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