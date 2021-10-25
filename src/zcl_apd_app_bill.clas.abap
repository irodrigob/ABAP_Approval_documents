CLASS zcl_apd_app_bill DEFINITION
  PUBLIC
  INHERITING FROM zcl_apd_apps
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_apd_apps .

  PUBLIC SECTION.
    CONSTANTS cv_id_app TYPE zapd_t001-app VALUE '02' ##NO_TEXT.

    "! <p class="shorttext synchronized">Envio de notificación en el proceso de aprobación/rechazo</p>
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">ID de factura</p>
    "! @parameter iv_rejected | <p class="shorttext synchronized">Rechazado</p>
    CLASS-METHODS static_send_notif_appr_proc
      IMPORTING
        iv_invoice_guid TYPE /cockpit/dinv_guid
        iv_rejected     TYPE sap_bool.
    "! <p class="shorttext synchronized">Envio de notificación cuando se inicia el proceso</p>
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">ID de factura</p>
    CLASS-METHODS static_send_notif_start_proc
      IMPORTING
        iv_invoice_guid TYPE /cockpit/dinv_guid.
    METHODS zif_apd_app~get_apps_kpi
        REDEFINITION .
    METHODS zif_apd_app~get_app_info REDEFINITION.
    METHODS zif_apd_app~get_list REDEFINITION.
    METHODS zif_apd_app~get_detail REDEFINITION.
    METHODS zif_apd_app~get_pdf REDEFINITION.
    METHODS zif_apd_app~approve_document REDEFINITION.
    METHODS zif_apd_app~reject_document REDEFINITION.
    METHODS zif_apd_app~add_note REDEFINITION.
    METHODS zif_apd_app~get_notes REDEFINITION.
    METHODS zif_apd_app~send_notification REDEFINITION.
    METHODS zif_apd_app~get_my_backups REDEFINITION.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_header_data,
             invoice_guid    TYPE /cockpit/shdr_if-invoice_guid,
             doc_date        TYPE /cockpit/shdr_if-doc_date,
             pstng_date      TYPE /cockpit/shdr_if-pstng_date,
             ref_doc_no      TYPE /cockpit/shdr_if-ref_doc_no,
             comp_code       TYPE /cockpit/shdr_if-comp_code,
             gross_amount    TYPE /cockpit/shdr_if-gross_amount,
             net_amount      TYPE /cockpit/shdr_if-net_amount,
             currency        TYPE /cockpit/shdr_if-currency,
             sap_doc_no      TYPE /cockpit/shdr_if-sap_doc_no,
             fiscal_year     TYPE /cockpit/shdr_if-fiscal_year,
             vendor_no       TYPE /cockpit/shdr_if-vendor_no,
             vendor_name1    TYPE /cockpit/shdr_if-vendor_name1,
             comp_code_descr TYPE /cockpit/shdr_if-comp_code_descr,
             netdt           TYPE /cockpit/shdr_if-netdt,
             wc_step_id      TYPE /cockpit/swc_if-wc_step_id,
             wc_step_id_word TYPE zapd_t006-palabra,
             wc_step_name    TYPE /cockpit/swc_if-wc_step_name,
             po_number       TYPE /cockpit/shdr_if-po_number,
             invoice_ind     TYPE /cockpit/shdr_if-invoice_ind,
             pmnttrms        TYPE /cockpit/shdr_if-pmnttrms,
             pmnttrms_desc   TYPE string,
           END OF ts_header_data.
    TYPES: tt_header_data TYPE STANDARD TABLE OF ts_header_data WITH EMPTY KEY.
    TYPES: tt_webcycle TYPE STANDARD TABLE OF /cockpit/swc_if.
    TYPES: tt_header TYPE STANDARD TABLE OF /cockpit/shdr_if.
    TYPES: BEGIN OF ts_items_base,
             pos_no TYPE /cockpit/titem-pos_no,
           END OF ts_items_base.
    TYPES: BEGIN OF ts_items_mm_base,
             po_number       TYPE /cockpit/titem-po_number,
             po_item         TYPE /cockpit/titem-po_item,
             item_text       TYPE /cockpit/titem-item_text,
             item_amount     TYPE /cockpit/titem-item_amount,
             gross_amount_mm TYPE /cockpit/titem-gross_amount,
             quantity        TYPE /cockpit/titem-quantity,
             po_unit         TYPE /cockpit/titem-po_unit,
             material        TYPE /cockpit/titem-material,
             material_desc   TYPE string,
           END OF ts_items_mm_base.
    TYPES: BEGIN OF ts_items_mm.
        INCLUDE TYPE ts_items_base.
        INCLUDE TYPE ts_items_mm_base.
    TYPES: END OF ts_items_mm.
    TYPES: tt_items_mm TYPE STANDARD TABLE OF ts_items_mm WITH EMPTY KEY.
    TYPES: BEGIN OF ts_items_fi_base,
             gl_account      TYPE /cockpit/tacct-gl_account,
             gl_account_desc TYPE string,
             net_amount      TYPE /cockpit/tacct-net_amount,
             gross_amount    TYPE /cockpit/tacct-gross_amount,
             text            TYPE /cockpit/tacct-text,
             costcenter      TYPE /cockpit/tacct-costcenter,
             costcenter_desc TYPE string,
             zz_fkber        TYPE /cockpit/tacct-zz_fkber,
             zz_fkber_desc   TYPE string,
             profit_ctr      TYPE /cockpit/tacct-profit_ctr,
             profit_ctr_desc TYPE string,
           END OF ts_items_fi_base.
    TYPES: BEGIN OF ts_items_fi.
        INCLUDE TYPE ts_items_base.
        INCLUDE TYPE ts_items_fi_base.
    TYPES: END OF ts_items_fi.
    TYPES: tt_items_fi TYPE STANDARD TABLE OF ts_items_fi WITH EMPTY KEY.
    TYPES: BEGIN OF ts_items_detail.
        INCLUDE TYPE ts_items_base.
        INCLUDE TYPE ts_items_mm_base.
        INCLUDE TYPE ts_items_fi_base.
    TYPES:
      currency     TYPE /cockpit/tacct-currency,
      invoice_guid TYPE /cockpit/shdr_if-invoice_guid,
      END OF ts_items_detail.
    TYPES: tt_items_detail TYPE STANDARD TABLE OF ts_items_detail WITH EMPTY KEY.
    TYPES: BEGIN OF ts_username_desc,
             username TYPE syuname,
             fullname TYPE string,
           END OF ts_username_desc.
    TYPES: tt_username_desc TYPE STANDARD TABLE OF ts_username_desc WITH EMPTY KEY.
    DATA mt_user_fullname TYPE tt_username_desc.

    "! <p class="shorttext synchronized">El usuario es aprobador</p>
    "! @parameter rv_is | <p class="shorttext synchronized">Es aprobador?</p>
    METHODS is_user_approver
      RETURNING VALUE(rv_is) TYPE sap_bool.

    "! <p class="shorttext synchronized">Lectura de datos de cabecera de factura asignadas al usuario</p>
    "! @parameter et_data | <p class="shorttext synchronized">Datos de cabecera</p>
    METHODS get_inv_header_data_by_user
      EXPORTING
        et_data TYPE tt_header_data.
    "! <p class="shorttext synchronized">Completar datos de cabecera</p>
    "! @parameter ct_data | <p class="shorttext synchronized">Datos de cabecera</p>
    METHODS complete_header_data
      CHANGING
        ct_data TYPE zcl_apd_app_bill=>tt_header_data.
    "! <p class="shorttext synchronized">Lectura de datos de cabecera de factura por id</p>
    "! @parameter iv_invoice_gui | <p class="shorttext synchronized">Guid de factura</p>
    "! @parameter es_data | <p class="shorttext synchronized">Datos de cabecera</p>
    METHODS get_inv_header_data_by_id
      IMPORTING
        iv_invoice_gui TYPE /cockpit/sif-invoice_guid
      EXPORTING
        es_data        TYPE ts_header_data .
    "! <p class="shorttext synchronized">Lectura de datos de detalle de factura por id</p>
    "! @parameter is_header_data | <p class="shorttext synchronized">Datos de cabecera</p>
    "! "! @parameter et_items_mm | <p class="shorttext synchronized">Posiciones de material</p>
    "! "! @parameter et_items_fi | <p class="shorttext synchronized">Posiciones de finanzas</p>
    METHODS get_inv_detail_data_by_id
      IMPORTING
        is_header_data TYPE ts_header_data
      EXPORTING
        et_items       TYPE zcl_apd_app_bill=>tt_items_detail.

    "! <p class="shorttext synchronized">Lectura de las posiciones de material</p>
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">Guid de factura</p>
    "! @parameter et_items | <p class="shorttext synchronized">Posiciones</p>
    METHODS read_items_mm
      IMPORTING
        iv_invoice_guid TYPE /cockpit/sif-invoice_guid
      EXPORTING
        et_items        TYPE zcl_apd_app_bill=>tt_items_mm.
    "! <p class="shorttext synchronized">Lectura de las posiciones de finanzas</p>
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">Guid de factura</p>
    "! @parameter et_items | <p class="shorttext synchronized">Posiciones</p>
    METHODS read_items_fi
      IMPORTING
        iv_invoice_guid TYPE /cockpit/sif-invoice_guid
      EXPORTING
        et_items        TYPE zcl_apd_app_bill=>tt_items_fi.
    "! <p class="shorttext synchronized">Lectura de los datos del webcycle</p>
    "! El webcycle es la información de aprobación
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">Guid de factura</p>
    "! @parameter es_webcylce | <p class="shorttext synchronized">Información webcycle</p>
    METHODS read_webcycle
      IMPORTING
        iv_invoice_guid TYPE /cockpit/shdr_if-invoice_guid
      EXPORTING
        es_webcylce     TYPE /cockpit/swc_if.
    "! <p class="shorttext synchronized">Cambio de usuario de paso del webcycle</p>
    "! @parameter iv_user | <p class="shorttext synchronized">usuario</p>
    "! @parameter es_return | <p class="shorttext synchronized">Resultado del cambio</p>
    "! @parameter cs_webcycle | <p class="shorttext synchronized">Información webcycle</p>
    METHODS change_user_wc_step
      IMPORTING
        iv_user     TYPE /cockpit/dwc_user
      EXPORTING
        es_return   TYPE bapiret2
      CHANGING
        cs_webcycle TYPE /cockpit/swc_if.
    "! <p class="shorttext synchronized">Convierte un string a formato nota</p>
    "! @parameter iv_string | <p class="shorttext synchronized">Nota en formato string</p>
    "! @parameter rt_note | <p class="shorttext synchronized">Nota en tabla</p>
    METHODS convert_string_2_note
      IMPORTING
                iv_string_note TYPE string
      RETURNING VALUE(rt_note) TYPE /cockpit/lwc_note.
    "! <p class="shorttext synchronized">Información adicional para el detalle</p>
    "! @parameter is_header_data | <p class="shorttext synchronized">Datos de cabecera</p>
    "! @parameter es_additional_info | <p class="shorttext synchronized">Información adicional</p>
    METHODS get_additional_info_detail
      IMPORTING
        is_header_data     TYPE zcl_apd_app_bill=>ts_header_data
      EXPORTING
        es_additional_info TYPE zif_apd_app=>ts_additional_info.
    "! <p class="shorttext synchronized">Devuelve el nombre completo de un usuario</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter rv_fullname | <p class="shorttext synchronized">Nombre completo</p>
    METHODS get_fullname_user
      IMPORTING
        iv_user            TYPE syuname
      RETURNING
        VALUE(rv_fullname) TYPE string.
    "! <p class="shorttext synchronized">Convierte a texto la diferencia de fecha y hora</p>
    "! Hace lo mismo que en twitter. Que te pone días u horas de cuando se ha publicado un tweet.
    "! @parameter IV_DATE | <p class="shorttext synchronized">Fecha</p>
    "! @parameter iv_time | <p class="shorttext synchronized">Hora</p>
    "! @parameter rv_text | <p class="shorttext synchronized">Texto</p>
    METHODS convert_text_date_time
      IMPORTING
        iv_date        TYPE d
        iv_time        TYPE t
      RETURNING
        VALUE(rv_text) TYPE string.
    "! <p class="shorttext synchronized">Bloquea una factura</p>
    "! @parameter is_invoice_data | <p class="shorttext synchronized">Datos factura</p>
    "! @parameter es_return | <p class="shorttext synchronized">Retorno del bloqueo</p>
    METHODS enqueue_invoice_guid
      IMPORTING
        is_invoice_data TYPE zcl_apd_app_bill=>ts_header_data
      EXPORTING
        es_return       TYPE bapiret2.
    "! <p class="shorttext synchronized">Desbloquea una factura</p>
    "! @parameter iv_invoice_guid | <p class="shorttext synchronized">Id de factura</p>
    METHODS dequeue_invoice_guid
      IMPORTING
        iv_invoice_guid TYPE /cockpit/shdr_if-invoice_guid.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APD_APP_BILL IMPLEMENTATION.


  METHOD change_user_wc_step.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.

    CLEAR: es_return.

    CALL FUNCTION '/COCKPIT/WC_SUBSTITUTE'
      EXPORTING
        ic_wc_user     = iv_user
        ic_wc_usertype = /cockpit/if_ap_con=>user_sap
        ib_online      = abap_false
*      IMPORTING
*       eb_is_user_substitute = lb_is_user_substitute
      TABLES
        et_messages    = lt_return
      CHANGING
        cs_wc_step     = cs_webcycle
      EXCEPTIONS
        canceled       = 1
        OTHERS         = 2.

    " Si hay mensaje de error se devuelve. En caso contrario no se indica nada.
    READ TABLE lt_return INTO es_return WITH KEY type = zif_apd_data=>cv_msg_type_error.
    IF sy-subrc = 0.
      es_return-message = zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_error
                       i_id         = es_return-id
                       i_number     = es_return-number
                       i_message_v1 = es_return-message_v1
                       i_message_v2 = es_return-message_v2
                       i_message_v3 = es_return-message_v3
                       i_message_v4 = es_return-message_v4
                       i_langu      = mv_langu )-message.
    ENDIF.


  ENDMETHOD.


  METHOD complete_header_data.

    " El texto de la sociedad se saca del maestro se ignora lo que venga del maestro.
    SELECT bukrs, butxt INTO TABLE @DATA(lt_t001)
           FROM t001
           FOR ALL ENTRIES IN @ct_data
           WHERE bukrs = @ct_data-comp_code.

    " Y el texto del proveedor porque dependiendo de la función del cockpit no viene la descripción.
    SELECT lifnr, name1 INTO TABLE @DATA(lt_lfa1)
               FROM lfa1
               FOR ALL ENTRIES IN @ct_data
               WHERE lifnr = @ct_data-vendor_no           .

    " Descripción de las condiciones de pago
    SELECT zterm, text1 INTO TABLE @DATA(lt_t052u)
           FROM t052u
           FOR ALL ENTRIES IN @ct_data
           WHERE zterm = @ct_data-pmnttrms
                 AND spras = @mv_langu.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      READ TABLE lt_t001 ASSIGNING FIELD-SYMBOL(<ls_t001>) WITH KEY bukrs = <ls_data>-comp_code.
      IF sy-subrc = 0.
        <ls_data>-comp_code_descr = <ls_t001>-butxt.
      ENDIF.
      READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<ls_lfa1>) WITH KEY lifnr = <ls_data>-vendor_no.
      IF sy-subrc = 0.
        <ls_data>-vendor_name1 = <ls_lfa1>-name1.
      ENDIF.
      READ TABLE lt_t052u ASSIGNING FIELD-SYMBOL(<ls_t052u>) WITH KEY zterm = <ls_data>-pmnttrms.
      IF sy-subrc = 0.
        <ls_data>-pmnttrms_desc = <ls_t052u>-text1.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD convert_string_2_note.

    SPLIT iv_string_note AT space INTO TABLE DATA(lt_words).

    DATA ls_line TYPE /cockpit/swc_note.
    DESCRIBE FIELD ls_line-line LENGTH DATA(lv_field_len) IN CHARACTER MODE.
    LOOP AT lt_words ASSIGNING FIELD-SYMBOL(<ls_words>).
      IF ls_line-line IS INITIAL.
        ls_line-line = <ls_words>.
      ELSE.
        DATA(lv_len) = strlen( ls_line-line ) + strlen( <ls_words> ).
        IF lv_len > lv_field_len.
          INSERT ls_line INTO TABLE rt_note.
          ls_line-line = <ls_words>.
        ELSEIF lv_len = lv_field_len.
          ls_line-line = |{ ls_line-line } { <ls_words> }|.
          INSERT ls_line INTO TABLE rt_note.
          CLEAR ls_line.
        ELSE.
          ls_line-line = |{ ls_line-line } { <ls_words> }|.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF ls_line-line IS NOT INITIAL.
      INSERT ls_line INTO TABLE rt_note.
    ENDIF.

  ENDMETHOD.


  METHOD convert_text_date_time.
    DATA lv_datediff TYPE p.
    DATA lv_timediff TYPE p.

    CLEAR rv_text.

    CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
      EXPORTING
        date1            = iv_date
        time1            = iv_time
        date2            = sy-datum
        time2            = sy-uzeit
      IMPORTING
        datediff         = lv_datediff
        timediff         = lv_timediff
      EXCEPTIONS
        invalid_datetime = 1
        OTHERS           = 2.
    IF sy-subrc = 0.

      " Si hay días de diferencia se informa los días
      IF lv_datediff IS NOT INITIAL.
        rv_text = |{ TEXT-t02 } { lv_datediff }{ TEXT-t01 }|.
      ELSE.
        rv_text = |{ TEXT-t02 } { lv_timediff }{ TEXT-t03 }|.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD dequeue_invoice_guid.
    CALL FUNCTION 'DEQUEUE_/COCKPIT/LHEADER'
      EXPORTING
        invoice_guid = iv_invoice_guid.
  ENDMETHOD.


  METHOD enqueue_invoice_guid.

    CLEAR: es_return.

    CALL FUNCTION 'ENQUEUE_/COCKPIT/LHEADER'
      EXPORTING
        invoice_guid   = is_invoice_data-invoice_guid
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc NE 0.
      es_return-id = 'F5'.
      es_return-number = '302'.
      es_return-type = zif_apd_data=>cv_msg_type_error.
      es_return-message_v1 = is_invoice_data-ref_doc_no.
      es_return-message_v2 = is_invoice_data-comp_code.
      es_return-message_v3 = sy-msgv1.
    ELSE.
      " Si la invoice_guid esta bloqueado miro si esta bloqueado la factura financiera
      CALL FUNCTION 'ENQUEUE_EFBKPF'
        EXPORTING
          belnr          = is_invoice_data-sap_doc_no
          bukrs          = is_invoice_data-comp_code
          gjahr          = is_invoice_data-fiscal_year
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2.
      IF sy-subrc NE 0.
        es_return-id = 'F5'.
        es_return-number = '302'.
        es_return-type = zif_apd_data=>cv_msg_type_error.
        es_return-message_v1 = is_invoice_data-ref_doc_no.
        es_return-message_v2 = is_invoice_data-comp_code.
        es_return-message_v3 = sy-msgv1.

        " Desbloqueo a nivel de cockpit para no dejarlo bloqueado
        dequeue_invoice_guid( is_invoice_data-invoice_guid ).

      ELSE.
        " Si se bloquea bien, se desbloquea
        CALL FUNCTION 'DEQUEUE_EFBKPF'
          EXPORTING
            belnr = is_invoice_data-sap_doc_no
            bukrs = is_invoice_data-comp_code
            gjahr = is_invoice_data-fiscal_year.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_additional_info_detail.

    CLEAR: es_additional_info.

    " Numero de notas
    zif_apd_app~get_notes( EXPORTING iv_numdoc      = CONV #( is_header_data-invoice_guid )
                                     iv_only_header = abap_true
                           IMPORTING et_notes       = DATA(lt_notes) ).

    es_additional_info-number_notes = lines( lt_notes ).


  ENDMETHOD.


  METHOD get_fullname_user.
    READ TABLE mt_user_fullname ASSIGNING FIELD-SYMBOL(<ls_fullname>) WITH KEY username = iv_user.
    IF sy-subrc = 0.
      rv_fullname = <ls_fullname>-fullname.
    ELSE.
      INSERT VALUE #(  username = iv_user ) INTO TABLE mt_user_fullname ASSIGNING <ls_fullname>.
      NEW zcl_apd_user( iv_user = iv_user )->get_user_details( IMPORTING ev_fullname = <ls_fullname>-fullname
                                                               EXCEPTIONS OTHERS = 99 ).
      rv_fullname = <ls_fullname>-fullname.
    ENDIF.
  ENDMETHOD.


  METHOD get_inv_detail_data_by_id.

    CLEAR: et_items.


    " Datos para las posición de material
    IF is_header_data-po_number IS NOT INITIAL.
      read_items_mm( EXPORTING iv_invoice_guid = is_header_data-invoice_guid
                     IMPORTING et_items = DATA(lt_items_mm) ).

      LOOP AT lt_items_mm ASSIGNING FIELD-SYMBOL(<ls_items_mm>).
        INSERT CORRESPONDING #( is_header_data ) INTO TABLE et_items ASSIGNING FIELD-SYMBOL(<ls_items>).

        " Para los abonos el importe será negativo
        IF is_header_data-invoice_ind = abap_false.
          <ls_items>-item_amount = - <ls_items>-item_amount.
          <ls_items>-gross_amount_mm = - <ls_items>-gross_amount_mm.
        ENDIF.

        <ls_items> = CORRESPONDING #( BASE ( <ls_items> ) <ls_items_mm> ).
      ENDLOOP.

    ELSE.
      " Datos para las posiciones de cuentas
      read_items_fi( EXPORTING iv_invoice_guid = is_header_data-invoice_guid
                     IMPORTING et_items = DATA(lt_items_fi) ).

      LOOP AT lt_items_fi ASSIGNING FIELD-SYMBOL(<ls_items_fi>).
        INSERT CORRESPONDING #( is_header_data ) INTO TABLE et_items ASSIGNING <ls_items>.
        <ls_items> = CORRESPONDING #( BASE ( <ls_items> ) <ls_items_fi> ).
      ENDLOOP.

    ENDIF.




  ENDMETHOD.


  METHOD get_inv_header_data_by_id.
    DATA ls_header TYPE /cockpit/thdr .
    DATA lt_data TYPE tt_header_data.

    CLEAR: es_data.

    CALL FUNCTION '/COCKPIT/DB_HEADER_GET'
      EXPORTING
        i_invoice_guid = iv_invoice_gui
        ib_no_auth     = abap_true
      IMPORTING
        e_str_header   = ls_header
      EXCEPTIONS
        no_data_found  = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      INSERT CORRESPONDING #( ls_header ) INTO TABLE lt_data.

      " Completar datos de cabecera
      complete_header_data( CHANGING ct_data = lt_data ).

      es_data = lt_data[ 1 ].

      " Para los abonos el importe será negativo
      IF es_data-invoice_ind = abap_false.
        es_data-net_amount = - es_data-net_amount.
        es_data-gross_amount = - es_data-gross_amount.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_inv_header_data_by_user.
    DATA lt_header TYPE tt_header.
    DATA lt_webcycle TYPE tt_webcycle.

    DATA lv_user TYPE /cockpit/dwc_user .

    CLEAR: et_data.

    " Leemos las palabras de búsqueda del WC ID
    SELECT * INTO TABLE @DATA(lt_wc_id_word)
           FROM zapd_t006.

    lv_user = mv_user.
    CALL FUNCTION '/COCKPIT/WC_API_GET_BY_USER'
      EXPORTING
        ic_user_type = /cockpit/if_ap_con=>user_sap
        ic_user      = lv_user
      TABLES
        et_webcycle  = lt_webcycle[]
        et_header    = lt_header[].


    " Quitamos las que no son de aprobación
    DELETE lt_webcycle WHERE wc_step_id NS 'APP'.

    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<ls_header>).
      READ TABLE lt_webcycle ASSIGNING FIELD-SYMBOL(<ls_webcycle>) WITH KEY invoice_guid = <ls_header>-invoice_guid.
      IF sy-subrc = 0.
        INSERT CORRESPONDING #( <ls_header> ) INTO TABLE et_data ASSIGNING FIELD-SYMBOL(<ls_data>).
        <ls_data> = CORRESPONDING #( BASE ( <ls_data> ) <ls_webcycle> ).

        " Buscamos la palabra
        READ TABLE lt_wc_id_word ASSIGNING FIELD-SYMBOL(<ls_wc_id_word>) WITH KEY wc_step_id = <ls_data>-wc_step_id.
        IF sy-subrc = 0.
          <ls_data>-wc_step_id_word = <ls_wc_id_word>-palabra.
        ENDIF.

        " Si es abono ponemos en negativo el importe
        IF <ls_header>-invoice_ind EQ abap_false.
          <ls_data>-gross_amount = - <ls_data>-gross_amount.
          <ls_data>-net_amount = - <ls_data>-net_amount.
        ENDIF.
      ENDIF.

    ENDLOOP.

    " Completar datos de cabecera
    complete_header_data( CHANGING ct_data = et_data ).

  ENDMETHOD.


  METHOD is_user_approver.

    rv_is = abap_false.

    " Hay que mirar tres tablas
    SELECT @abap_true INTO @rv_is UP TO 1 ROWS
           FROM zeic_puig_wf_bes
           WHERE wc_app_user = @mv_user
           ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc NE 0.
      SELECT @abap_true INTO @rv_is UP TO 1 ROWS
             FROM zeic_puig_wf_dep
             WHERE wc_user = @mv_user
             ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc NE 0.
        SELECT @abap_true INTO @rv_is UP TO 1 ROWS
        FROM zeic_puig_wf_fin
        WHERE wc_user = @mv_user
        ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF sy-subrc NE 0.
          " Busco los backups titulares que sustituye el usuario
          IF lines( zif_apd_app~get_my_backups( mv_user ) ) > 0.
            rv_is = abap_true.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD read_items_fi.
    DATA lt_account TYPE STANDARD TABLE OF /cockpit/tacct.

    CLEAR: et_items.

    CALL FUNCTION '/COCKPIT/DB_ACCT_GET'
      EXPORTING
        i_invoice_guid = iv_invoice_guid
      TABLES
        e_tab_account  = lt_account
      EXCEPTIONS
        no_data_found  = 0
        OTHERS         = 1.

    IF sy-subrc = 0 AND lt_account IS NOT INITIAL.

      DATA(lv_bukrs) = lt_account[ 1 ]-comp_code.
      SELECT  a~ktopl, b~kokrs INTO @DATA(ls_company) UP TO 1 ROWS
         FROM t001 AS a INNER JOIN tka02 AS b ON
              b~bukrs = a~bukrs
         WHERE a~bukrs = @lv_bukrs.
      ENDSELECT.

      SELECT saknr, txt50 INTO TABLE @DATA(lt_account_desc)
             FROM skat
             FOR ALL ENTRIES IN @lt_account
             WHERE saknr = @lt_account-gl_account
                   AND spras = @mv_langu
                   AND ktopl = @ls_company-ktopl.

      SELECT kostl, ltext INTO TABLE @DATA(lt_ceco_desc)
             FROM cskt
              FOR ALL ENTRIES IN @lt_account
               WHERE kostl = @lt_account-costcenter
                     AND kokrs = @ls_company-kokrs
                     AND spras = @mv_langu.

      SELECT prctr, ltext INTO TABLE @DATA(lt_cebe_desc)
             FROM cepct
             FOR ALL ENTRIES IN @lt_account
             WHERE prctr = @lt_account-profit_ctr
                   AND datbi >= @sy-datum
                   AND spras = @mv_langu.

      SELECT fkber, fkbtx INTO TABLE @DATA(lt_areafunc_desc)
             FROM tfkbt
             FOR ALL ENTRIES IN @lt_account
             WHERE fkber = @lt_account-zz_fkber
                   AND spras = @mv_langu.


      LOOP AT lt_account ASSIGNING FIELD-SYMBOL(<ls_account>).
        INSERT CORRESPONDING #( <ls_account> ) INTO TABLE et_items ASSIGNING FIELD-SYMBOL(<ls_items>).

        IF <ls_account>-shkzg = 'H'. " Haber se pone en negativo
          <ls_items>-gross_amount = - <ls_items>-gross_amount.
          <ls_items>-net_amount = - <ls_items>-net_amount.
        ENDIF.

        IF <ls_account>-gl_account IS NOT INITIAL.
          READ TABLE lt_account_desc ASSIGNING FIELD-SYMBOL(<ls_account_desc>) WITH KEY saknr = <ls_items>-gl_account.
          IF sy-subrc = 0.
            <ls_items>-gl_account_desc = <ls_account_desc>-txt50.
          ENDIF.
        ENDIF.

        IF <ls_account>-costcenter IS NOT INITIAL.
          READ TABLE lt_ceco_desc ASSIGNING FIELD-SYMBOL(<ls_ceco_desc>) WITH KEY kostl = <ls_items>-costcenter.
          IF sy-subrc = 0.
            <ls_items>-costcenter_desc = <ls_ceco_desc>-ltext.
          ENDIF.
        ENDIF.

        IF <ls_account>-profit_ctr IS NOT INITIAL.
          READ TABLE lt_cebe_desc ASSIGNING FIELD-SYMBOL(<ls_cebe_desc>) WITH KEY prctr = <ls_items>-profit_ctr.
          IF sy-subrc = 0.
            <ls_items>-profit_ctr_desc = <ls_cebe_desc>-ltext.
          ENDIF.
        ENDIF.

        IF <ls_account>-zz_fkber IS NOT INITIAL.
          READ TABLE lt_areafunc_desc ASSIGNING FIELD-SYMBOL(<ls_areafunc_desc>) WITH KEY fkber = <ls_items>-zz_fkber.
          IF sy-subrc = 0.
            <ls_items>-zz_fkber_desc = <ls_areafunc_desc>-fkbtx.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_items_mm.
    DATA: lt_item TYPE STANDARD TABLE OF /cockpit/titem.

    CLEAR: et_items.

    CALL FUNCTION '/COCKPIT/DB_ITEM_GET'
      EXPORTING
        i_invoice_guid = iv_invoice_guid
      TABLES
        e_tab_item     = lt_item
      EXCEPTIONS
        no_data_found  = 0
        OTHERS         = 1.
    IF sy-subrc = 0.
      " Textos del material
      SELECT matnr, maktx INTO TABLE @DATA(lt_makt)
              FROM makt
              FOR ALL ENTRIES IN @lt_item
              WHERE matnr = @lt_item-material.


      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
        INSERT CORRESPONDING #( <ls_item> ) INTO TABLE et_items ASSIGNING FIELD-SYMBOL(<ls_item_mm>).
        <ls_item_mm>-gross_amount_mm = <ls_item>-gross_amount.
        IF sy-subrc = 0.
          READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<ls_makt>) WITH KEY matnr = <ls_item>-material.
          IF sy-subrc = 0.
            <ls_item_mm>-material_desc = <ls_makt>-matnr.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD read_webcycle.

    CLEAR: es_webcylce.

    DATA(lv_user) = CONV /cockpit/dwc_user( mv_user ).

    CALL FUNCTION '/COCKPIT/WC_API_DATA_GET'
      EXPORTING
        ic_guid      = iv_invoice_guid
        ic_user_type = /cockpit/if_ap_con=>user_sap
        ic_user      = lv_user
        ic_language  = mv_langu
      IMPORTING
        es_webcycle  = es_webcylce
      EXCEPTIONS
        not_found    = 1
        no_flow      = 2
        OTHERS       = 3.


  ENDMETHOD.


  METHOD static_send_notif_appr_proc.
    " El job que lanza el coche escoba se encarga de la notificación
*    DATA lt_users_notif_process TYPE zif_apd_app=>tt_user_notif_process.
*    DATA ls_wc_status TYPE /cockpit/swc_status.
*    DATA lt_status_curr TYPE STANDARD TABLE OF /cockpit/swc_status_past.
*    DATA lt_status_past TYPE STANDARD TABLE OF /cockpit/swc_status_past.
*
*    " Captura cualquier posible excepción para que no afecte al proceso que llama a este método
*    TRY.
*
*        " Este método se llamará desde la función /COCKPIT/WC_SET_APPROVED y /COCKPIT/WC_SET_REJECTED, ese es su objetivo principal aunque por su construcción
*        " puede ser llamado desde cualquier sitio. En ambas funciones la llamada se hará al final de las misma con un enhacement.
*
*        CALL FUNCTION '/COCKPIT/WC_STATUS_GET'
*          EXPORTING
*            ic_guid        = iv_invoice_guid
*          IMPORTING
*            es_wc_status   = ls_wc_status
*          TABLES
*            et_status_curr = lt_status_curr
*            et_status_past = lt_status_past
*          EXCEPTIONS
*            error_occured  = 1
*            OTHERS         = 2.
*        IF sy-subrc = 0.
*
*
*          " Desde donde se llama este método el proceso lo que viene en ET_STATUS_CURR es lo que nosotros es el pending, es decir, lo que se tiene que aprobar.
*          " Si la factura esta rechazada no habrá en datos actuales.
*          lt_users_notif_process = VALUE #( FOR <wa> IN lt_status_curr ( username = <wa>-wc_user pending = abap_true ) ).
*
*
*          " En et_status_past tenemos los status a pasado, lo que nos interesa es el último aprobado.
*          IF lt_status_past IS NOT INITIAL.
*            " Para ello hay que hacer los siguientes pasos:
*            " Primero vamos quitar duplicados por paso. Porque por cada paso sale dos veces el usuario: envio mail y aprobación
*            SORT lt_status_past BY wc_step_id DESCENDING wc_user.
*            DELETE ADJACENT DUPLICATES FROM lt_status_past COMPARING wc_step_id wc_user.
*
*            " El ID de pasos tienen con un nombre y numero final, ejemplo: FI_APP_S3, FI_APP_S2 y FI_APP_S1. Al ordenar y eliminar duplicados
*            " tengo el paso más reciente arriba del todo.
*            DATA(lv_wc_step_id) = lt_status_past[ 1 ]-wc_step_id.
*            lt_users_notif_process = VALUE #( BASE lt_users_notif_process FOR <wa> IN lt_status_past WHERE ( wc_step_id = lv_wc_step_id )
*                                              ( username = <wa>-wc_user approved = abap_true ) ).
*          ENDIF.
*
*          DATA(lo_app_bill) = NEW zcl_apd_app_bill( iv_app = cv_id_app ).
*
*          lo_app_bill->zif_apd_app~send_notification(
*               EXPORTING
*                 iv_num_doc      = CONV #( iv_invoice_guid )
*                 iv_doc_rejected = iv_rejected
*                 it_users        = lt_users_notif_process
*               IMPORTING
*                 et_return       = DATA(lt_return) ).
*
*        ENDIF.
*
*      CATCH cx_root.
*    ENDTRY.

  ENDMETHOD.


  METHOD static_send_notif_start_proc.

    " El job que lanza el coche escoba se encarga de la notificación
*    DATA lt_users_notif_process TYPE zif_apd_app=>tt_user_notif_process.
*    DATA ls_wc_status TYPE /cockpit/swc_status.
*    DATA lt_status_curr TYPE STANDARD TABLE OF /cockpit/swc_status_past.
*    DATA lt_status_past TYPE STANDARD TABLE OF /cockpit/swc_status_past.
*
*    " Captura cualquier posible excepción para que no afecte al proceso que llama a este método
*    TRY.
*
*        " Este método se llamará desde la función /COCKPIT/WC_SET_APPROVED y /COCKPIT/WC_SET_REJECTED, ese es su objetivo principal aunque por su construcción
*        " puede ser llamado desde cualquier sitio. En ambas funciones la llamada se hará al final de las misma con un enhacement.
*
*        CALL FUNCTION '/COCKPIT/WC_STATUS_GET'
*          EXPORTING
*            ic_guid        = iv_invoice_guid
*          IMPORTING
*            es_wc_status   = ls_wc_status
*          TABLES
*            et_status_curr = lt_status_curr
*            et_status_past = lt_status_past
*          EXCEPTIONS
*            error_occured  = 1
*            OTHERS         = 2.
*        IF sy-subrc = 0.
*
*
*          " Desde donde se llama este método el proceso lo que viene en ET_STATUS_CURR es lo que nosotros es el pending, es decir, lo que se tiene que aprobar.
*          " Si la factura esta rechazada no habrá en datos actuales.
*          lt_users_notif_process = VALUE #( FOR <wa> IN lt_status_curr ( username = <wa>-wc_user pending = abap_true ) ).
*
*          " Nota IRB: Los usuarios que devuelve en el past se ignoran porque no habrá nada porque es cuando se inicial el workflow
*
*          DATA(lo_app_bill) = NEW zcl_apd_app_bill( iv_app = cv_id_app ).
*
*          lo_app_bill->zif_apd_app~send_notification(
*               EXPORTING
*                 iv_num_doc      = CONV #( iv_invoice_guid )
*                 iv_doc_rejected = abap_false
*                 it_users        = lt_users_notif_process
*               IMPORTING
*                 et_return       = DATA(lt_return) ).
*
*        ENDIF.
*
*      CATCH cx_root.
*    ENDTRY.
  ENDMETHOD.


  METHOD zif_apd_app~add_note.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.
    " leemos los datos del webcycle para determinar si el usuario de la tarea es el mismo
    " que el actual. Si son distintos se cambia. Tal como lo hace la transacción /cockpit/wc
    read_webcycle(
      EXPORTING
        iv_invoice_guid = CONV #( iv_numdoc )
      IMPORTING
        es_webcylce     = DATA(ls_webcycle) ).

    IF ls_webcycle-wc_user NE mv_user. " Si el usuario es distinto lo cambio
      change_user_wc_step(
        EXPORTING
          iv_user     = CONV #( mv_user )
        IMPORTING
          es_return   = DATA(ls_return)
        CHANGING
          cs_webcycle = ls_webcycle  ).

      IF ls_return IS INITIAL.
        INSERT ls_return INTO TABLE et_return.
      ENDIF.
    ENDIF.

    " Si no hay errores se añade la nota.
    IF et_return IS INITIAL.

      " Convertimos el string de la nota al formato que espera la función
      DATA(lt_note) = convert_string_2_note( iv_note ).
      DATA(lv_description) = CONV /cockpit/dtext_desc( iv_description ).

      DATA(lv_invoice_guid) = CONV /cockpit/dinv_guid( iv_numdoc ).
      CALL FUNCTION '/COCKPIT/TEXT_CREATE_OFFLINE'
        EXPORTING
          ic_invoice_guid = lv_invoice_guid
          ic_text_type    = zif_apd_data=>cs_app-invoice-wc_texttype_note
          ic_description  = lv_description
*         is_header       = is_header
          ii_wc_subpos    = ls_webcycle-wc_subpos
        TABLES
          it_note         = lt_note
          et_messages     = lt_return.

      " Fuerzo el commit
      COMMIT WORK AND WAIT.

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type = zif_apd_data=>cv_msg_type_error.
      IF sy-subrc = 0.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                             i_id         = <ls_return>-id
                             i_number     = <ls_return>-number
                             i_message_v1 = <ls_return>-message_v1
                             i_message_v2 = <ls_return>-message_v2
                             i_message_v3 = <ls_return>-message_v3
                             i_message_v4 = <ls_return>-message_v4
                             i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
        <ls_return_out>-parameter = iv_numdoc.

      ELSE.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                     i_id         = zif_apd_data=>cv_msg_id
                                     i_number     = '015'
                                     i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
        <ls_return_out>-parameter = iv_numdoc.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD zif_apd_app~approve_document.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.
    DATA lt_next_steps TYPE /cockpit/lwc_if.

    DATA(lv_errors_invoice) = abap_false.
    DATA(lv_ok_bill) = 0.
    DATA(lv_num_bill) = lines( it_numdoc ).
    LOOP AT it_numdoc ASSIGNING FIELD-SYMBOL(<ls_numdoc>).
      get_inv_header_data_by_id( EXPORTING iv_invoice_gui = CONV #( <ls_numdoc> )
                               IMPORTING es_data = DATA(ls_data) ).

      IF ls_data IS NOT INITIAL.

        " Se mira si la factura esta bloqueada
        enqueue_invoice_guid( EXPORTING is_invoice_data = ls_data
                           IMPORTING es_return = DATA(ls_return_locked) ).

        IF ls_return_locked IS INITIAL.
          " Leo los datos del webcycle para poder hacer la aprobación
          read_webcycle( EXPORTING iv_invoice_guid = ls_data-invoice_guid
                          IMPORTING es_webcylce = DATA(ls_wc) ).

          " Si el usuario del paso es distinto al que va a realizar la aprobación hay que cambiarlo para que quede en la foto que lo
          " ha realizado el suplente. Esto lo hace la transacción /cockpit/wc.

          " Por el mensaje unico se controla de manera distinta los errores de cambio de usuario
          DATA(lv_error_change_user) = abap_false.

          IF ls_wc-wc_user NE mv_user.
            change_user_wc_step( EXPORTING iv_user = CONV #( mv_user )
                                 IMPORTING es_return = DATA(ls_return_change_user)
                                 CHANGING cs_webcycle = ls_wc ).
            IF ls_return_change_user IS NOT INITIAL.
              lv_error_change_user = abap_true.
*            ls_return_change_user-parameter = ls_data-invoice_guid.
*            INSERT ls_return_change_user INTO TABLE et_return.

            ENDIF.
          ENDIF.

          " Si la tabla de mensajes esta vacia es que no se han producido errores en los pasos previos
          IF lv_error_change_user = abap_false.

            " Se obtiene los pasos siguientes. Si da un error no se podrá hacer el proceso
            CALL FUNCTION '/COCKPIT/WC_NEXT_STEPS_GET'
              EXPORTING
                is_wc_step    = ls_wc
                ib_auto_steps = abap_true
                ic_action     = zif_apd_data=>cs_app-invoice-actions-continue
              TABLES
                et_next_steps = lt_next_steps
                et_messages   = lt_return
              EXCEPTIONS
                error_occured = 1
                OTHERS        = 2.

            IF sy-subrc = 0.

              CALL FUNCTION '/COCKPIT/WC_SET_APPROVED'
                EXPORTING
                  is_wc_step    = ls_wc
                  ib_online     = abap_true
                TABLES
                  et_messages   = lt_return
                EXCEPTIONS
                  canceled      = 1
                  error_occured = 2
                  OTHERS        = 3.
              DATA(lv_subrc) = sy-subrc.

              " Se mira si hay algun mensaje de error
              READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type = zif_apd_data=>cv_msg_type_error.
              IF sy-subrc = 0.
                lv_errors_invoice = abap_true.
                " Solo se muestra un solo mensaje
*            INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
*                                 i_id         = <ls_return>-id
*                                 i_number     = <ls_return>-number
*                                 i_message_v1 = <ls_return>-message_v1
*                                 i_message_v2 = <ls_return>-message_v2
*                                 i_message_v3 = <ls_return>-message_v3
*                                 i_message_v4 = <ls_return>-message_v4
*                                 i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
*            <ls_return_out>-parameter = ls_data-invoice_guid.

              ELSE.
                " Por lo que he visto las excepciones se lanzan cuando hay mensaje de error. Aún así, prefiero controlar
                " que haya excepcion pero no tenga mensaje de error.
                IF lv_subrc = 0.
                  lv_ok_bill = lv_ok_bill + 1.
                  " Solo se muestra un solo mensaje
*              INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
*                              i_id         = zif_apd_data=>cv_msg_id
*                              i_number     = '014'
*                              i_message_v1 = |{ ls_data-sap_doc_no ALPHA = OUT }|
*                              i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
*              <ls_return_out>-parameter = ls_data-invoice_guid.

                ELSE.
                  lv_errors_invoice = abap_true.
*              INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
*                                   i_id         = zif_apd_data=>cv_msg_id
*                                   i_number     = '013'
*                                   i_message_v1 = |{ ls_data-sap_doc_no ALPHA = OUT }|
*                                   i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
*              <ls_return_out>-parameter = ls_data-invoice_guid.

                ENDIF.
              ENDIF.
            ELSE.
              " Si da error se indica que hay errores en el proces.
              lv_errors_invoice = abap_true.
            ENDIF.
          ELSE.
            lv_errors_invoice = abap_true.
          ENDIF.

          " Desbloqueo la factura
          dequeue_invoice_guid( ls_data-invoice_guid ).

        ELSE.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                               i_id         = ls_return_locked-id
                               i_number     = ls_return_locked-number
                               i_message_v1 = ls_return_locked-message_v1
                               i_message_v2 = ls_return_locked-message_v2
                               i_message_v3 = ls_return_locked-message_v3
                               i_message_v4 = ls_return_locked-message_v4
                               i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
          <ls_return_out>-parameter = ls_data-invoice_guid.
        ENDIF.




      ENDIF.
    ENDLOOP.

    " Si todo esta bien se saca el mensaje generico
    IF lv_ok_bill = lv_num_bill.
      INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                         i_id         = zif_apd_data=>cv_msg_id
                                         i_number     = '019'
                                         i_langu      = mv_langu ) INTO TABLE et_return.
    ELSE.
      " A petición de Maite no se saca el mensaje que va todo bien cuando hay errores junto a pedidos aprobados.
*      " Si hay alguna facturra que se ha aprobado entonces y no hay errores genéricos (significa que solo hay errores de bloqueos)
*      " añado el mensaje que ha ido todo bien
*      IF lv_ok_bill > 0 AND lv_errors_invoice = abap_false.
*        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
*                                           i_id         = zif_apd_data=>cv_msg_id
*                                           i_number     = '019'
*                                           i_langu      = mv_langu ) INTO TABLE et_return.
*      ENDIF.
      " Si se han producido errores saco el mensaje de error genérico
      IF lv_errors_invoice = abap_true.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                           i_id         = zif_apd_data=>cv_msg_id
                                           i_number     = '020'
                                           i_langu      = mv_langu ) INTO TABLE et_return.

      ENDIF.

    ENDIF.


    " Lanzamos coche escoba para las notificaciones
    send_massive_notif( ).

  ENDMETHOD.


  METHOD zif_apd_app~get_apps_kpi.

    CLEAR rv_kpi.

    " Lectura de datos de cabecera
    get_inv_header_data_by_user( IMPORTING et_data = DATA(lt_data) ).

    rv_kpi = lines( lt_data ).

  ENDMETHOD.


  METHOD zif_apd_app~get_app_info.

    CLEAR et_info.

    IF is_user_approver( ).
      read_app_info( EXPORTING it_r_app = VALUE #( ( sign = 'I' option = 'EQ' low = cv_id_app ) )
                      IMPORTING et_info = et_info ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_apd_app~get_detail.

    " Datos de cabecera
    get_inv_header_data_by_id( EXPORTING iv_invoice_gui = CONV #( iv_numdoc )
                               IMPORTING es_data = DATA(ls_data) ).

    " Lectura del detalle
    get_inv_detail_data_by_id( EXPORTING is_header_data = ls_data
                               IMPORTING et_items = DATA(lt_items) ).

    " Sacamos el tipo de documento segun si hay pedido en la cabecera
    DATA(lv_doc_type) = COND #( WHEN ls_data-po_number IS NOT INITIAL
                                THEN zif_apd_data=>cs_app-invoice-doc_type-order
                                ELSE zif_apd_data=>cs_app-invoice-doc_type-financial ).

    " Obtenemos el catalogo de campo dependiendo del tipo de pedido
    get_view_fcat( EXPORTING iv_level    = zif_apd_data=>cs_app-level-detail
                              iv_doc_type = lv_doc_type
                    IMPORTING et_fcat     = DATA(lt_fcat) ).

    " Convierte el catalogo de campos al formato de salida
    convert_fcat_2_output( EXPORTING it_fcat = lt_fcat
                           IMPORTING et_fcat_output = es_list-catalog ).


    " Añadimos la información de cabecera
    convert_struc_2_row_data( EXPORTING is_data = ls_data
                                        it_fcat = VALUE #( FOR <wa> IN lt_fcat WHERE ( show_header = abap_true ) ( <wa> ) )
                              IMPORTING es_row_data = DATA(ls_row_data) ).
    ls_row_data-row = 0.
    INSERT ls_row_data INTO TABLE es_list-data.

    " Los datos principales
    convert_itab_2_row_data( EXPORTING it_data = lt_items
                                         it_fcat = VALUE #( FOR <wa1> IN lt_fcat WHERE ( show_header = abap_false ) ( <wa1> ) )
                            IMPORTING et_row_data = DATA(lt_row_data) ).
    INSERT LINES OF lt_row_data INTO TABLE es_list-data.

    " Acciones
    get_actions( EXPORTING iv_level = zif_apd_data=>cs_app-level-detail
                           iv_doc_type = lv_doc_type
                 IMPORTING et_actions = es_list-actions ).

    " Se obtiene la información adicional para el detalle
    get_additional_info_detail( EXPORTING is_header_data = ls_data
                                IMPORTING es_additional_info = es_list-add_info ).

  ENDMETHOD.


  METHOD zif_apd_app~get_list.

    CLEAR: es_list.

    " Datos de la aplicación
    read_app_info( EXPORTING it_r_app = VALUE #( ( sign = 'I' option = 'EQ' low = cv_id_app ) )
                     IMPORTING et_info = DATA(lt_info) ).

    " Datos de las facturas
    get_inv_header_data_by_user( IMPORTING et_data = DATA(lt_data) ).

    READ TABLE lt_info ASSIGNING FIELD-SYMBOL(<ls_info>) INDEX 1.
    es_list-title = <ls_info>-title.

    " Se obtiene el catalogo de campos
    get_view_fcat( EXPORTING iv_level    = zif_apd_data=>cs_app-level-header
                             iv_doc_type = space
                   IMPORTING et_fcat     = DATA(lt_fcat) ).

    " Se convierten los datos al formato de salida
    convert_itab_2_row_data( EXPORTING it_data = lt_data
                                       it_fcat = lt_fcat
                             IMPORTING et_row_data = es_list-data  ).

    " Convierte el catalogo de campos al formato de salida
    convert_fcat_2_output( EXPORTING it_fcat = lt_fcat
                           IMPORTING et_fcat_output = es_list-catalog ).

    " Acciones
    get_actions( EXPORTING iv_level = zif_apd_data=>cs_app-level-header
                           iv_doc_type = space
                 IMPORTING et_actions = es_list-actions ).

  ENDMETHOD.


  METHOD zif_apd_app~get_my_backups.

    CLEAR er_backups.

    " Tabla de suplentes cockpit
    SELECT 'I' AS sign, 'EQ' AS option, wc_user AS low
           INTO TABLE @er_backups
           FROM /cockpit/twc_ums
           WHERE substitute_user      = @iv_user
               AND substitute_type = @/cockpit/if_ap_con=>user_sap
               AND valid_from   LE @sy-datum
               AND valid_to     GE @sy-datum
               AND active       = @abap_true.

    " Tabla de suplentes de HR
    SELECT 'I' AS sign, 'EQ' AS option, us_name AS low
        APPENDING TABLE @er_backups
        FROM hrus_d2
        WHERE rep_name      = @iv_user
            AND begda   LE @sy-datum
            AND endda     GE @sy-datum.

  ENDMETHOD.


  METHOD zif_apd_app~get_notes.
    DATA lt_texts TYPE STANDARD TABLE OF /cockpit/stxthdr_disp.
    DATA lt_body TYPE STANDARD TABLE OF /cockpit/swc_note.

    CLEAR et_notes.

    " Sacamos el listado de notas
    DATA(lv_invoice_guid) = CONV /cockpit/dinv_guid( iv_numdoc ).
    CALL FUNCTION '/COCKPIT/DB_TEXT_GETLIST'
      EXPORTING
        ic_invoice_guid   = lv_invoice_guid
        ib_note_types     = abap_true
        ib_sort_timestamp = abap_true
      TABLES
        et_txt_header     = lt_texts.

    LOOP AT lt_texts ASSIGNING FIELD-SYMBOL(<ls_texts>).
      APPEND INITIAL LINE TO et_notes ASSIGNING FIELD-SYMBOL(<ls_notes>).

      <ls_notes>-text_guid = <ls_texts>-text_guid.
      <ls_notes>-description = COND #( WHEN <ls_texts>-text_desc IS NOT INITIAL THEN <ls_texts>-text_desc ELSE <ls_texts>-text_titl ).
      <ls_notes>-time = <ls_texts>-time.
      <ls_notes>-date = <ls_texts>-date.
      <ls_notes>-user = <ls_texts>-cr_user.
      <ls_notes>-date_time_text = convert_text_date_time( iv_date = <ls_notes>-date
                                                          iv_time = <ls_notes>-time ).
      <ls_notes>-user_desc = get_fullname_user( CONV #( <ls_notes>-user ) ).
      <ls_notes>-user_desc = COND #( WHEN <ls_notes>-user_desc IS INITIAL THEN <ls_notes>-user ELSE <ls_notes>-user_desc ).

      " Ahora se saca el body, salvo que se indique por parámetro que se quiere los datos de cabecera
      IF iv_only_header = abap_false.
        CALL FUNCTION '/COCKPIT/DB_TEXT_GETDETAIL'
          EXPORTING
            ic_text_guid = <ls_notes>-text_guid
          TABLES
            et_txt_body  = lt_body.

        LOOP AT lt_body ASSIGNING FIELD-SYMBOL(<ls_body>).

          IF <ls_notes>-note IS INITIAL.
            <ls_notes>-note = <ls_body>-line.
          ELSE. " Añado salto de línea a cada texto
            <ls_notes>-note = |{ <ls_notes>-note }\n{ <ls_body>-line }|.
          ENDIF.

        ENDLOOP.

        CLEAR lt_body.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_apd_app~get_pdf.
    DATA lv_length TYPE sy-tabix.
    DATA lv_mimetype TYPE /cockpit/tmime-mime_type.
    DATA lv_ic_guid TYPE /cockpit/thdr-invoice_guid.
    DATA lt_content TYPE STANDARD TABLE OF tbl1024.
    DATA lv_xcontent TYPE xstring.
    DATA lv_file_ext TYPE sopcklsti1-doc_type.

    CLEAR: et_return, ev_url.

    " Del churro del documento me voy a la cabecera para obtener el numero de SAP, para que poderlo pasar a la
    " generación de la URL.
    get_inv_header_data_by_id( EXPORTING iv_invoice_gui = CONV #( iv_numdoc )
                               IMPORTING es_data = DATA(ls_data) ).

    IF ls_data IS NOT INITIAL.

      lv_ic_guid = iv_numdoc.
      CALL FUNCTION '/COCKPIT/WC_API_PICTURE_GET'
        EXPORTING
          ic_guid       = lv_ic_guid
          ib_url_anyway = 'Y' " Para que devuelva el contenido no la URL
        IMPORTING
          ei_length     = lv_length
          ec_mime_type  = lv_mimetype
*         ec_url        =
          ec_file_ext   = lv_file_ext
        TABLES
          et_content    = lt_content
        EXCEPTIONS
          not_found     = 1
          OTHERS        = 2.

      IF sy-subrc = 0.

        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_length
          IMPORTING
            buffer       = lv_xcontent
          TABLES
            binary_tab   = lt_content
          EXCEPTIONS
            failed       = 1
            OTHERS       = 2.

        IF sy-subrc = 0.



          " Se obtiene la URL a partir del binario.
          get_url_from_binary_content( EXPORTING iv_extension = lv_file_ext
                                                 iv_mimetype = CONV #( lv_mimetype )
                                                 iv_content = lv_xcontent
                                                 iv_numbytes = lv_length
                                                 iv_numdoc = CONV #( ls_data-sap_doc_no )
                                       IMPORTING ev_url = ev_url ).


        ELSE.
          RAISE EXCEPTION TYPE zcx_apd
            EXPORTING
              textid = zcx_apd=>get_document_not_possible.

        ENDIF.
      ELSE.

        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid = zcx_apd=>get_document_not_possible.

      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid = zcx_apd=>invoice_not_exist.
    ENDIF.
  ENDMETHOD.


  METHOD zif_apd_app~reject_document.
    DATA lt_return TYPE STANDARD TABLE OF bapiret2.

    LOOP AT it_numdoc ASSIGNING FIELD-SYMBOL(<ls_numdoc>).
      get_inv_header_data_by_id( EXPORTING iv_invoice_gui = CONV #( <ls_numdoc> )
                               IMPORTING es_data = DATA(ls_data) ).

      IF ls_data IS NOT INITIAL.

        " Se mira si la factura esta bloqueada
        enqueue_invoice_guid( EXPORTING is_invoice_data = ls_data
                           IMPORTING es_return = DATA(ls_return_locked) ).

        IF ls_return_locked IS INITIAL.

          " Leo los datos del webcycle para poder hacer la aprobación
          read_webcycle( EXPORTING iv_invoice_guid = ls_data-invoice_guid
                          IMPORTING es_webcylce = DATA(ls_wc) ).

          " Si el usuario del paso es distinto al que va a realizar la aprobación hay que cambiarlo para que quede en la foto que lo
          " ha realizado el suplente. Esto lo hace la transacción /cockpit/wc.

          " Por el mensaje unico se controla de manera distinta los errores de cambio de usuario
          DATA(lv_error_change_user) = abap_false.
          IF ls_wc-wc_user NE mv_user.
            change_user_wc_step( EXPORTING iv_user = CONV #( mv_user )
                                 IMPORTING es_return = DATA(ls_return_change_user)
                                 CHANGING cs_webcycle = ls_wc ).
            IF ls_return_change_user IS NOT INITIAL.


              ls_return_change_user-parameter = ls_data-invoice_guid.
              INSERT ls_return_change_user INTO TABLE et_return.

            ENDIF.
          ENDIF.


          IF et_return IS INITIAL.

            " Añadimos la nota del rechazo
            zif_apd_app~add_note(
              EXPORTING
                iv_numdoc      = <ls_numdoc>
                iv_description = zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_success
                                                                 i_id = zif_apd_data=>cv_msg_id
                                                                 i_number     = '010'
                                                                 i_message_v1 = ls_data-sap_doc_no
                                                                 i_langu      = mv_langu )-message
                iv_note        = iv_reason
              IMPORTING
                et_return      = DATA(lt_return_note) ).

            READ TABLE lt_return_note ASSIGNING FIELD-SYMBOL(<ls_return_note>) WITH KEY type = zif_apd_data=>cv_msg_type_error.
            IF sy-subrc NE 0.

              CALL FUNCTION '/COCKPIT/WC_SET_REJECTED'
                EXPORTING
                  is_wc_step    = ls_wc
                  ib_online     = abap_false
                TABLES
                  et_messages   = lt_return
                EXCEPTIONS
                  canceled      = 1
                  error_occured = 2
                  OTHERS        = 3.
              DATA(lv_subrc) = sy-subrc.

              " Se mira si hay algun mensaje de error
              READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WITH KEY type = zif_apd_data=>cv_msg_type_error.
              IF sy-subrc = 0.

                " Solo se muestra un mensaje global
                INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                     i_id         = <ls_return>-id
                                     i_number     = <ls_return>-number
                                     i_message_v1 = <ls_return>-message_v1
                                     i_message_v2 = <ls_return>-message_v2
                                     i_message_v3 = <ls_return>-message_v3
                                     i_message_v4 = <ls_return>-message_v4
                                     i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
                <ls_return_out>-parameter = ls_data-invoice_guid.

              ELSE.
                " Por lo que he visto las excepciones se lanzan cuando hay mensaje de error. Aún así, prefiero controlar
                " que haya excepcion pero no tenga mensaje de error.
                IF lv_subrc = 0.
                  " Solo se muestra un mensaje generico
                  INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                  i_id         = zif_apd_data=>cv_msg_id
                                  i_number     = '016'
                                  i_message_v1 = |{ ls_data-sap_doc_no ALPHA = OUT }|
                                  i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                  <ls_return_out>-parameter = ls_data-invoice_guid.

                ELSE.

                  " Solo se muestra un mensaje generico
                  INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                       i_id         = zif_apd_data=>cv_msg_id
                                       i_number     = '013'
                                       i_message_v1 = |{ ls_data-sap_doc_no ALPHA = OUT }|
                                       i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                  <ls_return_out>-parameter = ls_data-invoice_guid.

                ENDIF.
              ENDIF.

            ENDIF.
          ENDIF.

          " Desbloqueo la factura
          dequeue_invoice_guid( ls_data-invoice_guid ).

        ELSE.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                        i_id         = ls_return_locked-id
                                        i_number     = ls_return_locked-number
                                        i_message_v1 = ls_return_locked-message_v1
                                        i_message_v2 = ls_return_locked-message_v2
                                        i_message_v3 = ls_return_locked-message_v3
                                        i_message_v4 = ls_return_locked-message_v4
                                        i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
          <ls_return_out>-parameter = ls_data-invoice_guid.
        ENDIF.

      ENDIF.
    ENDLOOP.

    " Lanzamos coche escoba para las notificaciones
    send_massive_notif( ).
  ENDMETHOD.


  METHOD zif_apd_app~send_notification.
    DATA lt_notif TYPE tt_user_notifications.
    DATA lt_subs TYPE /eby/icwc_lsubstitution.

    LOOP AT it_users ASSIGNING FIELD-SYMBOL(<ls_users>).

      NEW zcl_apd_user( iv_user = <ls_users>-username )->get_user_details( IMPORTING ev_user_langu = DATA(lv_langu) ).

      DATA(lv_message) = VALUE string(  ).
      IF <ls_users>-approved = abap_true.
        " En aprobado no se envia mensaje
      ELSEIF <ls_users>-pending = abap_true.
        lv_message = zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                                     i_id         = zif_apd_data=>cv_msg_id
                                                     i_number     = '021'
                                                     i_langu      = lv_langu )-message.
      ENDIF.

      " Se añade el usuario principal
      DATA(lt_kpis) = zcl_apd_apps=>get_kpi_all_apps( iv_user = <ls_users>-username ).
      INSERT VALUE #( username = <ls_users>-username
                      badge = REDUCE #( INIT x = 0 FOR <wa> IN lt_kpis NEXT x = x + <wa>-kpi )
                      message = lv_message ) INTO TABLE lt_notif.

      " Buscamos los sustitutos para enviarles la notificación
      DATA(lv_user) = CONV /cockpit/dwc_user( <ls_users>-username ).
      CLEAR lt_subs.

      " Busco los backups titulares que sustituye el usuario
      DATA(lt_backups) = zif_apd_app~get_my_backups( <ls_users>-username ).

      " Busco del titular cuales son sus backups
      CALL FUNCTION '/COCKPIT/WC_SUBSTITUTE_GET'
        EXPORTING
          i_user          = lv_user
          i_user_type     = /cockpit/if_ap_con=>user_sap
          ib_read_active  = abap_true
        TABLES
          et_substitution = lt_subs
        EXCEPTIONS
          not_found       = 1
          OTHERS          = 2.

      lt_backups = VALUE #( BASE lt_backups FOR <wa1> IN lt_subs ( low = <wa1>-wc_user ) ).


      LOOP AT lt_backups ASSIGNING FIELD-SYMBOL(<ls_subs>).
        " Si el subsituto ya esta en las notificaciones no lo añado
        READ TABLE lt_notif TRANSPORTING NO FIELDS WITH KEY username = <ls_subs>-low.
        IF sy-subrc NE 0.

          NEW zcl_apd_user( iv_user = <ls_subs>-low )->get_user_details( IMPORTING ev_user_langu = lv_langu ).

          lv_message = zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                                       i_id         = zif_apd_data=>cv_msg_id
                                                       i_number     = '021'
                                                       i_langu      = lv_langu )-message.


          lt_kpis = zcl_apd_apps=>get_kpi_all_apps( iv_user = <ls_subs>-low ).
          INSERT VALUE #( username = <ls_subs>-low
                          badge = REDUCE #( INIT x = 0 FOR <wa> IN lt_kpis NEXT x = x + <wa>-kpi )
                          message = lv_message ) INTO TABLE lt_notif.
        ENDIF.
      ENDLOOP.


    ENDLOOP.

    SORT lt_notif.
    DELETE ADJACENT DUPLICATES FROM lt_notif COMPARING ALL FIELDS.

    " Envio de la notificación.
    send_notification( EXPORTING it_notificacion = lt_notif
                       IMPORTING et_return = et_return ).
  ENDMETHOD.
ENDCLASS.
