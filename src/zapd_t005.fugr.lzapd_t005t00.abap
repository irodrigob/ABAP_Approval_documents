*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 07.04.2021 at 18:32:18
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_T005.......................................*
DATA:  BEGIN OF STATUS_ZAPD_T005                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPD_T005                     .
CONTROLS: TCTRL_ZAPD_T005
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPD_T005                     .
TABLES: ZAPD_T005                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
