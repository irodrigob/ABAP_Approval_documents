*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.03.2021 at 17:49:01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_T002.......................................*
DATA:  BEGIN OF STATUS_ZAPD_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZAPD_T002                     .
CONTROLS: TCTRL_ZAPD_T002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZAPD_T002                     .
TABLES: ZAPD_T002                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
