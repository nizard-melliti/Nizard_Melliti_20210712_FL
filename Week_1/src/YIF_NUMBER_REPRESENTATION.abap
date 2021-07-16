INTERFACE yif_number_representation
  PUBLIC .
  METHODS :

    to_roman_representation RETURNING VALUE(r_result) TYPE string,

    to_arabic_representation RETURNING VALUE(r_result) TYPE string.

ENDINTERFACE.