REPORT yr_kata_roman_nme.

PARAMETERS p_number TYPE string.

CLASS lcl_units_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
    METHODS :
      constructor IMPORTING i_value TYPE string.
  PRIVATE SECTION.
    DATA gv_value TYPE string.
ENDCLASS.
CLASS lcl_units_representation IMPLEMENTATION.
  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

  METHOD constructor.
    gv_value = i_value.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_tens_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
    METHODS :
      constructor IMPORTING i_value TYPE string.
  PRIVATE SECTION.
    DATA gv_value TYPE string.
ENDCLASS.

CLASS lcl_tens_representation IMPLEMENTATION.
  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.
  METHOD constructor.
    gv_value = i_value.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_hundreds_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
    METHODS :
      constructor IMPORTING i_value TYPE string.
  PRIVATE SECTION.
    DATA gv_value TYPE string.
ENDCLASS.

CLASS lcl_hundreds_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

  METHOD constructor.
    gv_value = i_value.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_thousands_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
    METHODS :
      constructor IMPORTING i_value TYPE string.
  PRIVATE SECTION.
    DATA gv_value TYPE string.
ENDCLASS.

CLASS lcl_thousands_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

  METHOD constructor.
    gv_value = i_value.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_number_representation DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor IMPORTING i_units_value     TYPE string
                            i_tens_value      TYPE string
                            i_hundreds_value  TYPE string
                            i_thousands_value TYPE string,
      convert_units_to_roman RETURNING VALUE(r_result) TYPE string,
      convert_tens_to_roman RETURNING VALUE(r_result) TYPE string,
      convert_hundreds_to_roman RETURNING VALUE(r_result) TYPE string,
      convert_thousands_to_roman RETURNING VALUE(r_result) TYPE string,
      convert_units_to_arabic RETURNING VALUE(r_result) TYPE string,
      convert_tens_to_arabic RETURNING VALUE(r_result) TYPE string,
      convert_hundreds_to_arabic RETURNING VALUE(r_result) TYPE string,
      convert_thousands_to_arabic RETURNING VALUE(r_result) TYPE string.
  PRIVATE SECTION.
    DATA : lo_units_representation     TYPE REF TO lcl_units_representation,
           lo_tens_representation      TYPE REF TO lcl_tens_representation,
           lo_hundreds_representation  TYPE REF TO lcl_hundreds_representation,
           lo_thousands_representation TYPE REF TO lcl_thousands_representation.

ENDCLASS.

CLASS lcl_number_representation IMPLEMENTATION.

  METHOD constructor.
    lo_units_representation = NEW #( i_units_value  ).
    lo_tens_representation = NEW #( i_tens_value ).
    lo_hundreds_representation = NEW #( i_hundreds_value ).
    lo_thousands_representation = NEW #( i_thousands_value ).
  ENDMETHOD.

  METHOD convert_hundreds_to_roman.
    lo_hundreds_representation->yif_number_representation~to_roman_representation( ).
  ENDMETHOD.

  METHOD convert_tens_to_roman.
    lo_tens_representation->yif_number_representation~to_roman_representation( ).
  ENDMETHOD.

  METHOD convert_thousands_to_roman.
    lo_thousands_representation->yif_number_representation~to_roman_representation( ).
  ENDMETHOD.

  METHOD convert_units_to_roman.
    lo_units_representation->yif_number_representation~to_roman_representation( ).
  ENDMETHOD.

  METHOD convert_hundreds_to_arabic.
    lo_hundreds_representation->yif_number_representation~to_arabic_representation( ).
  ENDMETHOD.

  METHOD convert_tens_to_arabic.
    lo_tens_representation->yif_number_representation~to_arabic_representation( ).
  ENDMETHOD.

  METHOD convert_thousands_to_arabic.
    lo_thousands_representation->yif_number_representation~to_arabic_representation( ).
  ENDMETHOD.

  METHOD convert_units_to_arabic.
    lo_units_representation->yif_number_representation~to_arabic_representation( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_splitter DEFINITION.

  PUBLIC SECTION.
    METHODS :
      split_number IMPORTING i_value                         TYPE string
                   RETURNING VALUE(ro_number_representation) TYPE REF TO lcl_number_representation.
ENDCLASS.

CLASS lcl_splitter IMPLEMENTATION.

  METHOD split_number.
    DATA : lv_units_value     TYPE string,
           lv_tens_value      TYPE string,
           lv_hundreds_value  TYPE string,
           lv_thousands_value TYPE string.


    ro_number_representation = NEW #( i_units_value = lv_units_value
                                      i_tens_value = lv_tens_value
                                      i_hundreds_value = lv_hundreds_value
                                      i_thousands_value = lv_thousands_value ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_combiner DEFINITION.

  PUBLIC SECTION.
    METHODS :
      combine_values_to_arabic  IMPORTING io_number_representation TYPE REF TO lcl_number_representation
                                RETURNING VALUE(r_result)          TYPE string,
      combine_values_to_roman  IMPORTING io_number_representation TYPE REF TO lcl_number_representation
                               RETURNING VALUE(r_result)          TYPE string.
ENDCLASS.

CLASS lcl_combiner IMPLEMENTATION.

  METHOD combine_values_to_arabic.

    DATA(lv_units_to_arabic) = io_number_representation->convert_units_to_arabic(  ).
    DATA(lv_tens_to_arabic) = io_number_representation->convert_units_to_arabic(  ).
    DATA(lv_hundreds_to_arabic) = io_number_representation->convert_units_to_arabic(  ).
    DATA(lv_thousands_to_arabic) = io_number_representation->convert_units_to_arabic(  ).

    CONCATENATE lv_thousands_to_arabic lv_hundreds_to_arabic lv_tens_to_arabic lv_units_to_arabic
                INTO r_result.
    CONDENSE r_result NO-GAPS.
  ENDMETHOD.

  METHOD combine_values_to_roman.
    DATA(lv_units_to_roman) = io_number_representation->convert_units_to_roman(  ).
    DATA(lv_tens_to_roman) = io_number_representation->convert_units_to_roman(  ).
    DATA(lv_hundreds_to_roman) = io_number_representation->convert_units_to_roman(  ).
    DATA(lv_thousands_to_roman) = io_number_representation->convert_units_to_roman(  ).

    CONCATENATE lv_thousands_to_roman lv_hundreds_to_roman lv_tens_to_roman lv_units_to_roman
                INTO r_result.
    CONDENSE r_result NO-GAPS.
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
    r_result = NEW lcl_combiner(  )->combine_values_to_arabic( NEW lcl_splitter( )->split_number( i_number ) ).
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
    r_result = NEW lcl_combiner(  )->combine_values_to_roman( NEW lcl_splitter( )->split_number( i_number ) ).
  ENDMETHOD.
ENDCLASS.