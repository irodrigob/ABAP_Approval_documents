*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 23.04.2021 at 09:12:31
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V003_2.....................................*
TABLES: ZAPD_V003_2, *ZAPD_V003_2. "view work areas
CONTROLS: TCTRL_ZAPD_V003_2
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZAPD_V003_2. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZAPD_V003_2.
* Table for entries selected to show on screen
DATA: BEGIN OF ZAPD_V003_2_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V003_2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V003_2_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZAPD_V003_2_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V003_2.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V003_2_TOTAL.

*.........table declarations:.................................*
TABLES: ZAPD_T003                      .
TABLES: ZCA_T_FLDS_VIEW                .
TABLES: ZCA_T_FLDS_VIEWT               .
