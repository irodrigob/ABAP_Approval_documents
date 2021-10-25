*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.04.2021 at 10:24:22
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V002.......................................*
TABLES: ZAPD_V002, *ZAPD_V002. "view work areas
CONTROLS: TCTRL_ZAPD_V002
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZAPD_V002. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZAPD_V002.
* Table for entries selected to show on screen
DATA: BEGIN OF ZAPD_V002_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V002_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZAPD_V002_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V002.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V002_TOTAL.

*.........table declarations:.................................*
TABLES: ZAPD_T002                      .
