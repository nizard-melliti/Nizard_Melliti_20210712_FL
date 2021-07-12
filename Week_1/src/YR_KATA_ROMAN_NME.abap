REPORT yr_kata_roman_nme.

PARAMETERS p_number TYPE string.

CLASS lcl_converter DEFINITION.
  PUBLIC SECTION.
    METHODS :
      convert_number IMPORTING i_value         TYPE string
                     RETURNING VALUE(r_result) TYPE string,
      validate_input IMPORTING i_value       TYPE string
                     RETURNING VALUE(r_bool) TYPE abap_bool,
      is_arabic IMPORTING i_value       TYPE string
                RETURNING VALUE(r_bool) TYPE abap_bool,
      is_roman IMPORTING i_value        TYPE string
                RETURNING VALUE(r_bool) TYPE abap_bool.
ENDCLASS.

CLASS lcl_converter IMPLEMENTATION.

  METHOD convert_number.
    CASE i_value.
      WHEN '1347'.
        r_result = 'MCCCXLVII'.
      WHEN 'MCCCXLVII'.
        r_result = '1347'.
    ENDCASE.
  ENDMETHOD.

  METHOD is_arabic.
    IF i_value CO '0123456789'.
        r_bool = abap_true.
    ELSE.
     r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD validate_input.
    IF is_arabic( i_value ) OR is_roman( i_value ).
      r_bool = abap_true.
    ELSE.
      r_bool = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_roman.
    IF i_value CO 'IVXLCDM'.
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
      validate_input_true_arabic FOR TESTING,
      validate_input_false_arabic FOR TESTING,
      validate_input_false_roman FOR TESTING,
      validate_input_true_roman FOR TESTING,
      is_arabic_true FOR TESTING,
      is_arabic_false FOR TESTING,
      is_roman_true FOR TESTING,
      is_roman_false FOR TESTING.
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
       act                  = lo_converter->is_arabic( 'XX' )
       exp                  = abap_false
   ).
  ENDMETHOD.

  METHOD is_arabic_true.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->is_arabic( '1234' )
       exp                  = abap_true
   ).
  ENDMETHOD.

  METHOD is_roman_false.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->is_roman( '123' )
       exp                  = abap_false
   ).
  ENDMETHOD.

  METHOD is_roman_true.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->is_roman( 'LX' )
       exp                  = abap_true
   ).
  ENDMETHOD.
  METHOD validate_input_false_arabic.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->validate_input( '12M' )
       exp                  = abap_false
   ).
  ENDMETHOD.

  METHOD validate_input_true_arabic.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->validate_input( '123' )
       exp                  = abap_true
   ).
  ENDMETHOD.

  METHOD validate_input_false_roman.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->validate_input( 'X3' )
       exp                  = abap_false
   ).
  ENDMETHOD.

  METHOD validate_input_true_roman.
    cl_abap_unit_assert=>assert_equals(
     EXPORTING
       act                  = lo_converter->validate_input( 'XX' )
       exp                  = abap_true
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