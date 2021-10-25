*&---------------------------------------------------------------------*
*&  Include           LZAPD_FORMSF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_JOB_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_JOB_INFO  text
*----------------------------------------------------------------------*
FORM set_job_info USING pe_doc_info TYPE ssfcrespd
                        pe_job_info TYPE ssfcrescl.
  ms_doc_info = pe_doc_info.
  ms_job_info = pe_job_info.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_JOB_INFO_SAPSCRIPT
*&---------------------------------------------------------------------*
* Si viene de SAP Script relleno las estructuras de smartforms para que
* el proceso sea el mismo según se llame.
*----------------------------------------------------------------------*
FORM set_job_info_sapscript USING pe_otf TYPE tsfotf.
  ms_job_info-otfdata = pe_otf.

  " Si hay contenido le indico que hay paginas. Si el tamaño es 0,
  " indico que no hay paginas para que se gestione que el formulario
  " no tiene datos
  IF lines( pe_otf ) > 0.
    ms_doc_info-tdfpages = 1.
  ELSE.
    ms_doc_info-tdfpages = 0.
  ENDIF.
ENDFORM.
