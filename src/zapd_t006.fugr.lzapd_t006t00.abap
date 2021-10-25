*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 30.04.2021 at 10:39:38
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_T006.......................................*
DATA:  BEGIN OF STATUS_ZAPD_T006                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPD_T006                     .
CONTROLS: TCTRL_ZAPD_T006
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPD_T006                     .
TABLES: ZAPD_T006                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
