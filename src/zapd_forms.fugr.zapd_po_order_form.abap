FUNCTION zapd_po_order_form.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IV_KSCHL) TYPE  NA_KSCHL
*"     REFERENCE(IV_EBELN) TYPE  EBELN
*"     REFERENCE(IV_LANGU) TYPE  SYLANGU DEFAULT SY-LANGU
*"     REFERENCE(IV_ENT_SCREEN) TYPE  SAP_BOOL DEFAULT ABAP_FALSE
*"  EXPORTING
*"     REFERENCE(EV_CONTENT) TYPE  XSTRING
*"     REFERENCE(EV_NUMBYTES) TYPE  I
*"----------------------------------------------------------------------
  DATA lv_program TYPE tnapr-pgnam.
  DATA lv_form TYPE tnapr-ronam.
  DATA lv_field TYPE c LENGTH 30.
  DATA lv_returncode TYPE sy-subrc.
  DATA lv_screen TYPE c.
  DATA lv_langu TYPE langu.

  CLEAR: ev_content, ev_numbytes, ms_doc_info, ms_job_info.

  SELECT SINGLE * INTO nast
    FROM nast
    WHERE objky = iv_ebeln
      AND kschl = iv_kschl.

  SELECT * INTO tnapr
    FROM tnapr UP TO 1 ROWS
    WHERE kschl = iv_kschl
         AND kappl = 'EF'
       ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc = 0.

    " Paso el parámetro a una variable porque segun el procedimiento que se llame dicha variable
    " es cambiada, si pasará el parámetro directamente daría un dump.
    lv_screen = iv_ent_screen.
    PERFORM (tnapr-ronam) IN PROGRAM (tnapr-pgnam) USING lv_returncode
                                                    lv_screen
                                                    IF FOUND.

    " Solo se devuelve el contenido si hay páginas
    IF ms_doc_info-tdfpages > 0.

      " Se recupera el spool generado
      zcl_ca_spools=>otf_2_pdf(
        EXPORTING
          it_otf                      = ms_job_info-otfdata
        IMPORTING
          ev_numbytes                 = ev_numbytes
          ev_pdf                      = ev_content ).

    ENDIF.

  ENDIF.
ENDFUNCTION.
