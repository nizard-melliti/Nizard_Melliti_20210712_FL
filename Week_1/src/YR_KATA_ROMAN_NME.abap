REPORT yr_kata_roman_nme.

PARAMETERS p_number TYPE string.

CLASS lcl_units_representation DEFINITION.
  PUBLIC SECTION.
    INTERFACES yif_number_representation.
    METHODS :
      constructor IMPORTING i_value TYPE string.
  PRIVATE SECTION.
    DATA lo_value TYPE string.
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
  PRIVATE SECTION.
    DATA lo_value TYPE string.
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
  PRIVATE SECTION.
    DATA lo_value TYPE string.
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
  PRIVATE SECTION.
    DATA lo_value TYPE string.
ENDCLASS.

CLASS lcl_thousands_representation IMPLEMENTATION.

  METHOD yif_number_representation~to_arabic_representation.

  ENDMETHOD.

  METHOD yif_number_representation~to_roman_representation.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_number_representation DEFINITION.

  PUBLIC SECTION.
    METHODS :
      constructor,
      convert_units_to_roman IMPORTING i_number        TYPE string
                             RETURNING VALUE(r_result) TYPE string,
      convert_tens_to_roman IMPORTING i_number        TYPE string
                            RETURNING VALUE(r_result) TYPE string,
      convert_hundreds_to_roman IMPORTING i_number        TYPE string
                                RETURNING VALUE(r_result) TYPE string,
      convert_thousands_to_roman IMPORTING i_number        TYPE string
                                 RETURNING VALUE(r_result) TYPE string,
      convert_units_to_arabic IMPORTING i_number        TYPE string
                              RETURNING VALUE(r_result) TYPE string,
      convert_tens_to_arabic IMPORTING i_number        TYPE string
                             RETURNING VALUE(r_result) TYPE string,
      convert_hundreds_to_arabic IMPORTING i_number        TYPE string
                                 RETURNING VALUE(r_result) TYPE string,
      convert_thousands_to_arabic IMPORTING i_number        TYPE string
                                  RETURNING VALUE(r_result) TYPE string.
  PRIVATE SECTION.
    DATA : lo_units_representation     TYPE REF TO lcl_units_representation,
           lo_tens_representation      TYPE REF TO lcl_tens_representation,
           lo_hundreds_representation  TYPE REF TO lcl_hundreds_representation,
           lo_thousands_representation TYPE REF TO lcl_thousands_representation.

ENDCLASS.

CLASS lcl_number_representation IMPLEMENTATION.

  METHOD constructor.
    lo_units_representation = NEW #(  ).
    lo_tens_representation = NEW #(  ).
    lo_hundreds_representation = NEW #(  ).
    lo_thousands_representation = NEW #(  ).
  ENDMETHOD.

  METHOD convert_hundreds_to_roman.
    lo_hundreds_representation->yif_number_representation~to_roman_representation( i_number ).
  ENDMETHOD.

  METHOD convert_tens_to_roman.
    lo_tens_representation->yif_number_representation~to_roman_representation( i_number ).
  ENDMETHOD.

  METHOD convert_thousands_to_roman.
    lo_thousands_representation->yif_number_representation~to_roman_representation( i_number ).
  ENDMETHOD.

  METHOD convert_units_to_roman.
    lo_units_representation->yif_number_representation~to_roman_representation( i_number ).
  ENDMETHOD.

  METHOD convert_hundreds_to_arabic.
    lo_hundreds_representation->yif_number_representation~to_arabic_representation( i_number ).
  ENDMETHOD.

  METHOD convert_tens_to_arabic.
    lo_tens_representation->yif_number_representation~to_arabic_representation( i_number ).
  ENDMETHOD.

  METHOD convert_thousands_to_arabic.
    lo_thousands_representation->yif_number_representation~to_arabic_representation( i_number ).
  ENDMETHOD.

  METHOD convert_units_to_arabic.
    lo_units_representation->yif_number_representation~to_arabic_representation( i_number ).
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

*    ro_number_representation
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

  ENDMETHOD.

  METHOD combine_values_to_roman.

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