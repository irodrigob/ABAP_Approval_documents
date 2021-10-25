*&---------------------------------------------------------------------*
*& Report ZAPD_R_UPDATE_BADGE_PUSH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zapd_r_update_badge_push.
TABLES: zapd_t007.

TYPES: BEGIN OF ts_data,
         username   TYPE zapd_t007-username,
         aedat      TYPE zapd_t007-aedat,
         aezet      TYPE zapd_t007-aezet,
         send_badge TYPE zapd_t007-send_badge,
         kpi_total  TYPE i,
         message    TYPE string,
         log        TYPE string,
       END OF ts_data.
TYPES: tt_data TYPE STANDARD TABLE OF ts_data WITH EMPTY KEY.

DATA mt_data TYPE tt_data.
DATA mo_alv TYPE REF TO zcl_ca_alv.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_appl  TYPE zpush_t001-appl OBLIGATORY.
SELECT-OPTIONS: s_user FOR zapd_t007-username.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-t02.
PARAMETERS p_test AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK bl2.

START-OF-SELECTION.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_data
         FROM zapd_t007
         WHERE username IN s_user.

  IF sy-subrc = 0.

    DATA(lo_push) = NEW zcl_push_main( iv_appl = p_appl ).

    LOOP AT mt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      NEW zcl_apd_user( iv_user = <ls_data>-username )->get_user_details( IMPORTING ev_user_langu = DATA(lv_langu) ).
      DATA(lt_kpis) = zcl_apd_apps=>get_kpi_all_apps( iv_user = <ls_data>-username ).
      <ls_data>-kpi_total = REDUCE #( INIT x = 0 FOR <wa> IN lt_kpis  NEXT x = x + <wa>-kpi ).

      " Solo se envia mensaje cuando el numero de KPI total calculada es distinto al enviado previamente. Y el KPI total calculada sea
      " distinto de cero.
      <ls_data>-message = COND #( WHEN <ls_data>-kpi_total > <ls_data>-send_badge AND <ls_data>-kpi_total NE 0
                                 THEN  zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                                                       i_id         = zif_apd_data=>cv_msg_id
                                                                       i_number     = '021'
                                                                       i_langu      = lv_langu )-message
                                 ELSE space ).

      IF p_test = abap_true.
        <ls_data>-log = TEXT-s01.
      ELSE.

        IF <ls_data>-send_badge NE <ls_data>-kpi_total OR <ls_data>-kpi_total IS INITIAL.
          lo_push->send_push( EXPORTING it_users = VALUE #( ( <ls_data>-username ) )
                                       it_notification = VALUE #( ( message = <ls_data>-message
                                                                    sound = COND #( WHEN <ls_data>-message IS NOT INITIAL
                                                                                    THEN zif_apd_data=>cs_push-default_sound
                                                                                    ELSE '' )
                                                                    badge = <ls_data>-kpi_total ) )
                             IMPORTING et_return = DATA(lt_return) ).
          IF lt_return IS NOT INITIAL.
            <ls_data>-log = lt_return[ 1 ]-message.
          ENDIF.

          UPDATE zapd_t007 SET send_badge = <ls_data>-kpi_total WHERE username = <ls_data>-username.
          COMMIT WORK AND WAIT.

        ENDIF.
      ENDIF.


    ENDLOOP.

  ENDIF.

END-OF-SELECTION.

  " En fondo no hay listado para evitar generacion masiva de spools.
  IF mt_data IS NOT INITIAL AND sy-batch IS INITIAL.


    mo_alv = NEW #( ).

    CALL METHOD mo_alv->crear_alv
      EXPORTING
        i_modo_listado  = if_salv_c_bool_sap=>false
        i_programa      = sy-repid
      CHANGING
        c_datos         = mt_data
      EXCEPTIONS
        error_crear_alv = 1
        OTHERS          = 2.

    CALL METHOD mo_alv->set_cols_optimizadas( ).

    mo_alv->set_titulo( CONV #( sy-title ) ).
    mo_alv->set_funciones_alv( abap_true ).
    mo_alv->set_gestion_layout( ).

    mo_alv->set_atributos_campo( EXPORTING i_campo = 'KPI_TOTAL' i_texto_todas = TEXT-c01 ).
    mo_alv->set_atributos_campo( EXPORTING i_campo = 'SEND_BADGE' i_texto_todas = TEXT-c03 ).
    mo_alv->set_atributos_campo( EXPORTING i_campo = 'MESSAGE' i_texto_todas = TEXT-c04 ).
    mo_alv->set_atributos_campo( EXPORTING i_campo = 'LOG' i_texto_todas = TEXT-c02 ).

    mo_alv->mostrar_alv( ).

  ENDIF.
