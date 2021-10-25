*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.03.2021 at 13:46:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V001.......................................*
TABLES: ZAPD_V001, *ZAPD_V001. "view work areas
CONTROLS: TCTRL_ZAPD_V001
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZAPD_V001. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZAPD_V001.
* Table for entries selected to show on screen
DATA: BEGIN OF ZAPD_V001_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V001_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZAPD_V001_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V001.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V001_TOTAL.

*.........table declarations:.................................*
TABLES: ZAPD_T001                      .
TABLES: ZAPD_T001T                     .
