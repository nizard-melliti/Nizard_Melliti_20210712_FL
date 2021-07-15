INTERFACE yif_number_representation
  PUBLIC .
  METHODS :

    to_roman_representation IMPORTING i_value TYPE string
                            RETURNING VALUE(r_result) TYPE string,

    to_arabic_representation IMPORTING i_value TYPE string
                             RETURNING VALUE(r_result) TYPE string.

ENDINTERFACE.