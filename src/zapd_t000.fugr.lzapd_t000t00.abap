*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.03.2021 at 13:49:29
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_T000.......................................*
DATA:  BEGIN OF STATUS_ZAPD_T000                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPD_T000                     .
CONTROLS: TCTRL_ZAPD_T000
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPD_T000                     .
TABLES: ZAPD_T000                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
