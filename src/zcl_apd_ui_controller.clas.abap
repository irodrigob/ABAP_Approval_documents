CLASS zcl_apd_ui_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_user_info
      IMPORTING
        !langu     TYPE sy-langu
        !user      TYPE syuname
      EXPORTING
        !user_info TYPE zcl_apd_user=>ts_user_info
        !return    TYPE bapiret2_t .
    CLASS-METHODS get_apps
      IMPORTING
        !langu  TYPE sy-langu
        !user   TYPE syuname
      EXPORTING
        !apps   TYPE zcl_apd_apps=>tt_apps
        !return TYPE bapiret2_t .
    CLASS-METHODS get_apps_kpi
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
      EXPORTING
        !return TYPE bapiret2_t
        !number TYPE numc5 .
    "! <p class="shorttext synchronized">Devuelve listado de documentos pendientes aprobar</p>
    "! Aparte del listado también se devuelve acciones, o cualquier otra información necesaria
    "! para el listado
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter list | <p class="shorttext synchronized">Listado</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
    CLASS-METHODS get_list
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
      EXPORTING
        !list   TYPE zif_apd_app=>ts_header_list
        !return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Devuelve el detalle de un número de documento</p>
    "! Aparte del listado también se devuelve acciones, o cualquier otra información necesaria
    "! para el listado
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter list | <p class="shorttext synchronized">Listado</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
    CLASS-METHODS get_detail
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
        !numdoc TYPE string
      EXPORTING
        !list   TYPE zif_apd_app=>ts_detail_list
        !return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Devuelve la URL del PDF del documento pasado</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter url | <p class="shorttext synchronized">URL</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
    CLASS-METHODS get_pdf
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
        !numdoc TYPE string
      EXPORTING
        !url    TYPE string
        !return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Aprobación del documento</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
    CLASS-METHODS approve_document
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
        !numdoc TYPE string
      EXPORTING
        !return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Rechazo del documento</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
    CLASS-METHODS reject_document
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
        !numdoc TYPE string
        !reason TYPE string
      EXPORTING
        !return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Añade una nota</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter description | <p class="shorttext synchronized">Descripción</p>
    "! @parameter note | <p class="shorttext synchronized">Cuerpo de la nota</p>
    "! @parameter return | <p class="shorttext synchronized">Resultado de la nota</p>
    CLASS-METHODS add_note
      IMPORTING
        !langu       TYPE sylangu OPTIONAL
        !user        TYPE syuname
        !app         TYPE zapd_e_app
        !numdoc      TYPE string
        !description TYPE string OPTIONAL
        !note        TYPE string
      EXPORTING
        !return      TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Listado de notas</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
    "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
    "! @parameter notes | <p class="shorttext synchronized">Notas</p>
    CLASS-METHODS get_notes
      IMPORTING
        !langu  TYPE sylangu OPTIONAL
        !user   TYPE syuname
        !app    TYPE zapd_e_app
        !numdoc TYPE string
      EXPORTING
        !notes  TYPE zif_apd_app=>tt_notes.
  PROTECTED SECTION.

    CLASS-DATA mo_cx_apd TYPE REF TO zcx_apd .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APD_UI_CONTROLLER IMPLEMENTATION.


  METHOD add_note.
    DATA lr_bloq_user TYPE RANGE OF uname.
    CLEAR: return.

    " Usuarios bloqueados para realizas acciones
    zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'BLOQ_USER_%'
                                                      CHANGING t_ranges = lr_bloq_user ).

    IF user NOT IN lr_bloq_user[].

      TRY.

          zcl_apd_apps=>get_instance(
            EXPORTING
              iv_app   = app
              iv_langu = langu
              iv_user  = user
            IMPORTING
              eo_app   = DATA(lo_app) ).

          lo_app->add_note( EXPORTING iv_numdoc = numdoc
                                      iv_description = description
                                      iv_note = note
                              IMPORTING et_return = return ).

        CATCH zcx_apd INTO mo_cx_apd.

          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                                 i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                                 i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                                 i_message_v1 = mo_cx_apd->mv_msgv1
                                                 i_message_v2 = mo_cx_apd->mv_msgv2
                                                 i_message_v3 = mo_cx_apd->mv_msgv3
                                                 i_message_v4 = mo_cx_apd->mv_msgv4
                                                 i_langu      = langu ) INTO TABLE return.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD approve_document.
    DATA lt_numdocs TYPE zif_apd_data=>tt_numdoc.
    DATA lr_bloq_user TYPE RANGE OF uname.

    CLEAR: return.

    " Usuarios bloqueados para realizas acciones
    zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'BLOQ_USER_%'
                                                      CHANGING t_ranges = lr_bloq_user ).

    IF user NOT IN lr_bloq_user[].

      TRY.

          zcl_apd_apps=>get_instance(
            EXPORTING
              iv_app   = app
              iv_langu = langu
              iv_user  = user
            IMPORTING
              eo_app   = DATA(lo_app) ).

          " Se separán los numeros de documentos que vienen
          SPLIT numdoc AT zif_apd_data=>cs_app-character_sep_numdoc INTO TABLE lt_numdocs.

          lo_app->approve_document( EXPORTING it_numdoc = lt_numdocs
                              IMPORTING et_return = return ).


        CATCH zcx_apd INTO mo_cx_apd.

          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                                 i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                                 i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                                 i_message_v1 = mo_cx_apd->mv_msgv1
                                                 i_message_v2 = mo_cx_apd->mv_msgv2
                                                 i_message_v3 = mo_cx_apd->mv_msgv3
                                                 i_message_v4 = mo_cx_apd->mv_msgv4
                                                 i_langu      = langu ) INTO TABLE return.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD get_apps.
    TRY.

        CALL METHOD zcl_apd_apps=>get_apps
          EXPORTING
            iv_langu = langu
            iv_user  = user
          IMPORTING
            et_apps  = apps.

      CATCH zcx_apd INTO mo_cx_apd.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                               i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                               i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                               i_message_v1 = mo_cx_apd->mv_msgv1
                                               i_message_v2 = mo_cx_apd->mv_msgv2
                                               i_message_v3 = mo_cx_apd->mv_msgv3
                                               i_message_v4 = mo_cx_apd->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.
  ENDMETHOD.


  METHOD get_apps_kpi.

    TRY.

        zcl_apd_apps=>get_instance(
          EXPORTING
            iv_app   = app
            iv_langu = langu
            iv_user  = user
          IMPORTING
            eo_app   = DATA(lo_app) ).

        number = lo_app->get_apps_kpi( ).


      CATCH zcx_apd INTO mo_cx_apd.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                               i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                               i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                               i_message_v1 = mo_cx_apd->mv_msgv1
                                               i_message_v2 = mo_cx_apd->mv_msgv2
                                               i_message_v3 = mo_cx_apd->mv_msgv3
                                               i_message_v4 = mo_cx_apd->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.


  ENDMETHOD.


  METHOD get_detail.
    CLEAR: list,return.

    TRY.

        zcl_apd_apps=>get_instance(
          EXPORTING
            iv_app   = app
            iv_langu = langu
            iv_user  = user
          IMPORTING
            eo_app   = DATA(lo_app) ).

        lo_app->get_detail( EXPORTING iv_numdoc = numdoc
                            IMPORTING es_list = list ).


      CATCH zcx_apd INTO mo_cx_apd.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                               i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                               i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                               i_message_v1 = mo_cx_apd->mv_msgv1
                                               i_message_v2 = mo_cx_apd->mv_msgv2
                                               i_message_v3 = mo_cx_apd->mv_msgv3
                                               i_message_v4 = mo_cx_apd->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.

  ENDMETHOD.


  METHOD get_list.

    CLEAR: list,return.

    TRY.

        zcl_apd_apps=>get_instance(
          EXPORTING
            iv_app   = app
            iv_langu = langu
            iv_user  = user
          IMPORTING
            eo_app   = DATA(lo_app) ).

        lo_app->get_list( IMPORTING es_list = list ).


      CATCH zcx_apd INTO mo_cx_apd.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                               i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                               i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                               i_message_v1 = mo_cx_apd->mv_msgv1
                                               i_message_v2 = mo_cx_apd->mv_msgv2
                                               i_message_v3 = mo_cx_apd->mv_msgv3
                                               i_message_v4 = mo_cx_apd->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.


  ENDMETHOD.


  METHOD get_notes.

    TRY.

        zcl_apd_apps=>get_instance(
          EXPORTING
            iv_app   = app
            iv_langu = langu
            iv_user  = user
          IMPORTING
            eo_app   = DATA(lo_app) ).

        lo_app->get_notes( EXPORTING iv_numdoc = numdoc
                           IMPORTING et_notes = notes ).

      CATCH zcx_apd INTO mo_cx_apd.

    ENDTRY.

  ENDMETHOD.


  METHOD get_pdf.
    CLEAR: url,return.

    TRY.

        zcl_apd_apps=>get_instance(
          EXPORTING
            iv_app   = app
            iv_langu = langu
            iv_user  = user
          IMPORTING
            eo_app   = DATA(lo_app) ).

        lo_app->get_pdf( EXPORTING iv_numdoc = numdoc
                            IMPORTING ev_url = url ).


      CATCH zcx_apd INTO mo_cx_apd.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                               i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                               i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                               i_message_v1 = mo_cx_apd->mv_msgv1
                                               i_message_v2 = mo_cx_apd->mv_msgv2
                                               i_message_v3 = mo_cx_apd->mv_msgv3
                                               i_message_v4 = mo_cx_apd->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.
  ENDMETHOD.


  METHOD get_user_info.

* Si no hay usuario informado manda el de sistema. En caso contrario el del parámetro.
    IF user IS INITIAL.
      DATA(lv_user) = CONV syuname( sy-uname ).
    ELSE.
      lv_user = user.
    ENDIF.

    DATA(lo_user) = NEW zcl_apd_user(  iv_user  = lv_user  iv_langu = langu ).

    lo_user->get_user_info(
      IMPORTING
        es_user_info = user_info
        es_return    = DATA(ls_return) ).

    IF ls_return IS NOT INITIAL.
      APPEND ls_return TO return.
    ENDIF.

    FREE lo_user.
  ENDMETHOD.


  METHOD reject_document.
    DATA lt_numdocs TYPE zif_apd_data=>tt_numdoc.
    DATA lr_bloq_user TYPE RANGE OF uname.
    CLEAR: return.


    " Usuarios bloqueados para realizas acciones
    zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'BLOQ_USER_%'
                                                      CHANGING t_ranges = lr_bloq_user ).

    IF user NOT IN lr_bloq_user[].
      TRY.


          zcl_apd_apps=>get_instance(
            EXPORTING
              iv_app   = app
              iv_langu = langu
              iv_user  = user
            IMPORTING
              eo_app   = DATA(lo_app) ).

          " Se separán los numeros de documentos que vienen
          SPLIT numdoc AT zif_apd_data=>cs_app-character_sep_numdoc INTO TABLE lt_numdocs.

          lo_app->reject_document( EXPORTING it_numdoc = lt_numdocs
                                             iv_reason = reason
                              IMPORTING et_return = return ).


        CATCH zcx_apd INTO mo_cx_apd.

          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                                 i_id         = mo_cx_apd->if_t100_message~t100key-msgid
                                                 i_number     = mo_cx_apd->if_t100_message~t100key-msgno
                                                 i_message_v1 = mo_cx_apd->mv_msgv1
                                                 i_message_v2 = mo_cx_apd->mv_msgv2
                                                 i_message_v3 = mo_cx_apd->mv_msgv3
                                                 i_message_v4 = mo_cx_apd->mv_msgv4
                                                 i_langu      = langu ) INTO TABLE return.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
