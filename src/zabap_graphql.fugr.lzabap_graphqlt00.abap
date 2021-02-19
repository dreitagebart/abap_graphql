*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 17.02.2021 at 19:43:36
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMARA...........................................*
DATA:  BEGIN OF STATUS_ZMARA                         .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMARA                         .
CONTROLS: TCTRL_ZMARA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMARA                         .
TABLES: ZMARA                          .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
