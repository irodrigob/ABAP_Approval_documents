*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 06.04.2021 at 10:58:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V_CAT......................................*
TABLES: ZAPD_V_CAT, *ZAPD_V_CAT. "view work areas
CONTROLS: TCTRL_ZAPD_V_CAT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZAPD_V_CAT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZAPD_V_CAT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZAPD_V_CAT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V_CAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V_CAT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZAPD_V_CAT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZAPD_V_CAT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZAPD_V_CAT_TOTAL.

*.........table declarations:.................................*
TABLES: ZCA_T_FIELDCAT                 .
TABLES: ZCA_T_FIELDCATT                .
