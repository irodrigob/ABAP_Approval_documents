CLASS zcl_apd_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_user_info,
        token    TYPE string,
        decimals TYPE val_text,
        date     TYPE val_text,
        fullname TYPE string,
        user_sap TYPE syuname,
      END OF ts_user_info .

    METHODS constructor
      IMPORTING
        !iv_user  TYPE syuname DEFAULT sy-uname
        !iv_langu TYPE sylangu DEFAULT sy-langu .
    METHODS get_user_info
      IMPORTING
        !iv_langu     TYPE sylangu OPTIONAL
      EXPORTING
        !es_user_info TYPE ts_user_info
        !es_return    TYPE bapiret2 .
    METHODS get_user_details
      EXPORTING
        !ev_fullname   TYPE string
        !ev_email      TYPE ad_smtpadr
        !es_return     TYPE bapiret2
        !ev_user_langu TYPE sylangu
      EXCEPTIONS
        error_user .
  PROTECTED SECTION.

    DATA mv_user TYPE syuname .
    DATA mv_langu TYPE sylangu .
    "! <p class="shorttext synchronized">Register user</p>
    METHODS register_user.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APD_USER IMPLEMENTATION.


  METHOD constructor.

    mv_user      = iv_user.
    mv_langu     = iv_langu.

  ENDMETHOD.


  METHOD get_user_details.

    DATA: lt_return  TYPE bapiret2_tab,
          lt_addsmtp TYPE STANDARD TABLE OF bapiadsmtp.

    DATA ls_address TYPE bapiaddr3.

    FIELD-SYMBOLS: <ls_addsmtp> LIKE LINE OF lt_addsmtp.

    CLEAR: ev_fullname, es_return.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = mv_user
      IMPORTING
        address  = ls_address
      TABLES
        addsmtp  = lt_addsmtp
        return   = lt_return[].

    READ TABLE lt_return INTO es_return WITH KEY type = zif_dcm_data=>cv_msg_type_error.
    IF sy-subrc NE 0.

      ev_fullname = ls_address-fullname.
      ev_user_langu = ls_address-langu_p.

      IF ev_user_langu IS INITIAL.
        ev_user_langu = ls_address-langu.
      ENDIF.

      READ TABLE lt_addsmtp ASSIGNING <ls_addsmtp> INDEX 1.
      IF <ls_addsmtp> IS ASSIGNED.
        ev_email = <ls_addsmtp>-e_mail.
      ENDIF.
    ELSE.


      SELECT b~smtp_addr, c~langu, d~name_text, d~langu AS langu_pers INTO @DATA(ls_user_info) UP TO 1 ROWS
           FROM usr21 AS a
           LEFT OUTER JOIN adr6 AS b ON
               b~addrnumber = a~addrnumber
               AND b~persnumber = a~persnumber
          LEFT OUTER JOIN adrc AS c ON
               c~addrnumber = a~addrnumber
        LEFT OUTER JOIN adrp AS d ON
            d~persnumber = a~persnumber
           WHERE a~bname = @mv_user.
      ENDSELECT.
      IF sy-subrc = 0.
        ev_email = ls_user_info-smtp_addr.
        ev_user_langu = COND #( WHEN ls_user_info-langu_pers IS NOT INITIAL THEN ls_user_info-langu_pers ELSE ls_user_info-langu ).
        ev_fullname = ls_user_info-name_text.
      ENDIF.


    ENDIF.

  ENDMETHOD.


  METHOD get_user_info.

    DATA ls_return TYPE bapiret2.

    CLEAR: es_user_info, es_return.

* Establecemos el idioma
    IF iv_langu IS NOT INITIAL.
      mv_langu = iv_langu.
    ENDIF.

* usuario conexion SAP
    es_user_info-user_sap = mv_user.

* Recuperamos el nombre completo del usuario
    CALL METHOD get_user_details
      IMPORTING
        ev_fullname = es_user_info-fullname
        es_return   = ls_return
      EXCEPTIONS
        error_user  = 1
        OTHERS      = 2.


    IF ls_return IS INITIAL.

* Generamos el token
      es_user_info-token = zcl_ca_token=>generar( zif_apd_data=>cv_app_ui5 ).

* Establecemos la configuración de fechas y decimales
      zcl_ca_utilidades=>get_user_date_en( EXPORTING user = mv_user
                                           IMPORTING date = es_user_info-date ).
      zcl_ca_utilidades=>get_user_decimals_en( EXPORTING user = mv_user
                                               IMPORTING decimals = es_user_info-decimals ).

      " Se registra el usuario en la tabla para saber cuando es la ultima vez que ha entrado
      register_user( ).


    ELSE.
      es_return = zcl_ca_utilidades=>fill_return(  i_type   = ls_return-type
                                                   i_id     = ls_return-id
                                                   i_number = ls_return-number
                                                   i_langu  = mv_langu
                                                   i_message_v1 = ls_return-message_v1
                                                   i_message_v2 = ls_return-message_v2
                                                   i_message_v3 = ls_return-message_v3
                                                   i_message_v4 = ls_return-message_v4 ).
    ENDIF.
  ENDMETHOD.


  METHOD register_user.

    SELECT SINGLE * INTO @DATA(ls_user)
           FROM zapd_t007
           WHERE username = @mv_user.

    IF sy-subrc NE 0.
      ls_user = VALUE zapd_t007( username = mv_user
                                       aedat = sy-datum
                                       aezet = sy-uzeit ).


    ELSE.
      ls_user-aedat = sy-datum.
      ls_user-aezet = sy-uzeit.
    ENDIF.

    MODIFY zapd_t007 FROM ls_user.
    COMMIT WORK AND WAIT.

  ENDMETHOD.
ENDCLASS.
