*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.04.2021 at 10:59:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V004.......................................*
TABLES: ZAPD_V004, *ZAPD_V004. "view work areas
CONTROLS: TCTRL_ZAPD_V004
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZAPD_V004. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZAPD_V004.
* Table for entries selected to show on screen
DATA: BEGIN OF ZAPD_V004_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V004_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZAPD_V004_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V004.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V004_TOTAL.

*.........table declarations:.................................*
TABLES: ZAPD_T004                      .
TABLES: ZAPD_T004T                     .
