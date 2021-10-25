*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 06.04.2021 at 10:58:25
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZAPD_V_CAT......................................*
FORM GET_DATA_ZAPD_V_CAT.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZCA_T_FIELDCAT WHERE
    APPL EQ 'ZAPD' AND
(VIM_WHERETAB) .
    CLEAR ZAPD_V_CAT .
ZAPD_V_CAT-MANDT =
ZCA_T_FIELDCAT-MANDT .
ZAPD_V_CAT-APPL =
ZCA_T_FIELDCAT-APPL .
ZAPD_V_CAT-FLDNAME =
ZCA_T_FIELDCAT-FLDNAME .
ZAPD_V_CAT-FLDNAME_INT =
ZCA_T_FIELDCAT-FLDNAME_INT .
ZAPD_V_CAT-DATATYPE =
ZCA_T_FIELDCAT-DATATYPE .
ZAPD_V_CAT-LENG_FIELD =
ZCA_T_FIELDCAT-LENG_FIELD .
ZAPD_V_CAT-DECIMALS =
ZCA_T_FIELDCAT-DECIMALS .
ZAPD_V_CAT-LENG_VIS =
ZCA_T_FIELDCAT-LENG_VIS .
ZAPD_V_CAT-FIELD_DESC =
ZCA_T_FIELDCAT-FIELD_DESC .
ZAPD_V_CAT-CONV_EXIT =
ZCA_T_FIELDCAT-CONV_EXIT .
ZAPD_V_CAT-FIELD_REF =
ZCA_T_FIELDCAT-FIELD_REF .
ZAPD_V_CAT-FIELD_URL_DESC =
ZCA_T_FIELDCAT-FIELD_URL_DESC .
ZAPD_V_CAT-TYPETABLE =
ZCA_T_FIELDCAT-TYPETABLE .
ZAPD_V_CAT-EDIT =
ZCA_T_FIELDCAT-EDIT .
    SELECT SINGLE * FROM ZCA_T_FIELDCATT WHERE
APPL = ZCA_T_FIELDCAT-APPL AND
FLDNAME = ZCA_T_FIELDCAT-FLDNAME AND
SPRAS = SY-LANGU .
    IF SY-SUBRC EQ 0.
ZAPD_V_CAT-TXT_COLUMN =
ZCA_T_FIELDCATT-TXT_COLUMN .
ZAPD_V_CAT-TXT_INFO =
ZCA_T_FIELDCATT-TXT_INFO .
    ENDIF.
<VIM_TOTAL_STRUC> = ZAPD_V_CAT.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZAPD_V_CAT .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZAPD_V_CAT.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZAPD_V_CAT-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZCA_T_FIELDCAT WHERE
  APPL = ZAPD_V_CAT-APPL AND
  FLDNAME = ZAPD_V_CAT-FLDNAME .
    IF SY-SUBRC = 0.
    DELETE ZCA_T_FIELDCAT .
    ENDIF.
    DELETE FROM ZCA_T_FIELDCATT WHERE
    APPL = ZCA_T_FIELDCAT-APPL AND
    FLDNAME = ZCA_T_FIELDCAT-FLDNAME .
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZCA_T_FIELDCAT WHERE
  APPL = ZAPD_V_CAT-APPL AND
  FLDNAME = ZAPD_V_CAT-FLDNAME .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZCA_T_FIELDCAT.
    ENDIF.
ZCA_T_FIELDCAT-MANDT =
ZAPD_V_CAT-MANDT .
ZCA_T_FIELDCAT-APPL =
ZAPD_V_CAT-APPL .
ZCA_T_FIELDCAT-FLDNAME =
ZAPD_V_CAT-FLDNAME .
ZCA_T_FIELDCAT-FLDNAME_INT =
ZAPD_V_CAT-FLDNAME_INT .
ZCA_T_FIELDCAT-DATATYPE =
ZAPD_V_CAT-DATATYPE .
ZCA_T_FIELDCAT-LENG_FIELD =
ZAPD_V_CAT-LENG_FIELD .
ZCA_T_FIELDCAT-DECIMALS =
ZAPD_V_CAT-DECIMALS .
ZCA_T_FIELDCAT-LENG_VIS =
ZAPD_V_CAT-LENG_VIS .
ZCA_T_FIELDCAT-FIELD_DESC =
ZAPD_V_CAT-FIELD_DESC .
ZCA_T_FIELDCAT-CONV_EXIT =
ZAPD_V_CAT-CONV_EXIT .
ZCA_T_FIELDCAT-FIELD_REF =
ZAPD_V_CAT-FIELD_REF .
ZCA_T_FIELDCAT-FIELD_URL_DESC =
ZAPD_V_CAT-FIELD_URL_DESC .
ZCA_T_FIELDCAT-TYPETABLE =
ZAPD_V_CAT-TYPETABLE .
ZCA_T_FIELDCAT-EDIT =
ZAPD_V_CAT-EDIT .
    IF SY-SUBRC = 0.
    UPDATE ZCA_T_FIELDCAT ##WARN_OK.
    ELSE.
    INSERT ZCA_T_FIELDCAT .
    ENDIF.
    SELECT SINGLE FOR UPDATE * FROM ZCA_T_FIELDCATT WHERE
    APPL = ZCA_T_FIELDCAT-APPL AND
    FLDNAME = ZCA_T_FIELDCAT-FLDNAME AND
    SPRAS = SY-LANGU .
      IF SY-SUBRC <> 0.   "insert preprocessing: init WA
        CLEAR ZCA_T_FIELDCATT.
ZCA_T_FIELDCATT-APPL =
ZCA_T_FIELDCAT-APPL .
ZCA_T_FIELDCATT-FLDNAME =
ZCA_T_FIELDCAT-FLDNAME .
ZCA_T_FIELDCATT-SPRAS =
SY-LANGU .
      ENDIF.
ZCA_T_FIELDCATT-TXT_COLUMN =
ZAPD_V_CAT-TXT_COLUMN .
ZCA_T_FIELDCATT-TXT_INFO =
ZAPD_V_CAT-TXT_INFO .
    IF SY-SUBRC = 0.
    UPDATE ZCA_T_FIELDCATT ##WARN_OK.
    ELSE.
    INSERT ZCA_T_FIELDCATT .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZAPD_V_CAT-UPD_FLAG,
STATUS_ZAPD_V_CAT-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZAPD_V_CAT.
  SELECT SINGLE * FROM ZCA_T_FIELDCAT WHERE
APPL = ZAPD_V_CAT-APPL AND
FLDNAME = ZAPD_V_CAT-FLDNAME .
ZAPD_V_CAT-MANDT =
ZCA_T_FIELDCAT-MANDT .
ZAPD_V_CAT-APPL =
ZCA_T_FIELDCAT-APPL .
ZAPD_V_CAT-FLDNAME =
ZCA_T_FIELDCAT-FLDNAME .
ZAPD_V_CAT-FLDNAME_INT =
ZCA_T_FIELDCAT-FLDNAME_INT .
ZAPD_V_CAT-DATATYPE =
ZCA_T_FIELDCAT-DATATYPE .
ZAPD_V_CAT-LENG_FIELD =
ZCA_T_FIELDCAT-LENG_FIELD .
ZAPD_V_CAT-DECIMALS =
ZCA_T_FIELDCAT-DECIMALS .
ZAPD_V_CAT-LENG_VIS =
ZCA_T_FIELDCAT-LENG_VIS .
ZAPD_V_CAT-FIELD_DESC =
ZCA_T_FIELDCAT-FIELD_DESC .
ZAPD_V_CAT-CONV_EXIT =
ZCA_T_FIELDCAT-CONV_EXIT .
ZAPD_V_CAT-FIELD_REF =
ZCA_T_FIELDCAT-FIELD_REF .
ZAPD_V_CAT-FIELD_URL_DESC =
ZCA_T_FIELDCAT-FIELD_URL_DESC .
ZAPD_V_CAT-TYPETABLE =
ZCA_T_FIELDCAT-TYPETABLE .
ZAPD_V_CAT-EDIT =
ZCA_T_FIELDCAT-EDIT .
    SELECT SINGLE * FROM ZCA_T_FIELDCATT WHERE
APPL = ZCA_T_FIELDCAT-APPL AND
FLDNAME = ZCA_T_FIELDCAT-FLDNAME AND
SPRAS = SY-LANGU .
    IF SY-SUBRC EQ 0.
ZAPD_V_CAT-TXT_COLUMN =
ZCA_T_FIELDCATT-TXT_COLUMN .
ZAPD_V_CAT-TXT_INFO =
ZCA_T_FIELDCATT-TXT_INFO .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZAPD_V_CAT-TXT_COLUMN .
      CLEAR ZAPD_V_CAT-TXT_INFO .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZAPD_V_CAT USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZAPD_V_CAT-APPL TO
ZCA_T_FIELDCAT-APPL .
MOVE ZAPD_V_CAT-FLDNAME TO
ZCA_T_FIELDCAT-FLDNAME .
MOVE ZAPD_V_CAT-MANDT TO
ZCA_T_FIELDCAT-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZCA_T_FIELDCAT'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZCA_T_FIELDCAT TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZCA_T_FIELDCAT'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

MOVE ZCA_T_FIELDCAT-APPL TO
ZCA_T_FIELDCATT-APPL .
MOVE ZCA_T_FIELDCAT-FLDNAME TO
ZCA_T_FIELDCATT-FLDNAME .
MOVE SY-LANGU TO
ZCA_T_FIELDCATT-SPRAS .
MOVE ZAPD_V_CAT-MANDT TO
ZCA_T_FIELDCATT-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZCA_T_FIELDCATT'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZCA_T_FIELDCATT TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZCA_T_FIELDCATT'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
