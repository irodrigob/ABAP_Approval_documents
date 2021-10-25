INTERFACE zif_apd_app
  PUBLIC .

  TYPES: BEGIN OF ts_app_info,
           app   TYPE zapd_t001-app,
           title TYPE zapd_t001t-descripcion,
           icon  TYPE zapd_t001-icono,
           orden TYPE zapd_t001-orden,
         END OF ts_app_info.
  TYPES: tt_app_info TYPE STANDARD TABLE OF ts_app_info WITH EMPTY KEY.
  TYPES:
    tr_backup_users TYPE RANGE OF zapd_e_backup .
  TYPES: BEGIN OF ts_value_field,
           field TYPE string,
           value TYPE string,
         END OF ts_value_field.
  TYPES: tt_value_field TYPE STANDARD TABLE OF ts_value_field WITH EMPTY KEY.
  TYPES: BEGIN OF ts_value_row,
           row  TYPE sytabix,
           data TYPE tt_value_field,
         END OF ts_value_row.
  TYPES: tt_value_row TYPE STANDARD TABLE OF ts_value_row WITH EMPTY KEY.
  TYPES: BEGIN OF ts_fieldcatalog,
           field     TYPE fieldname,
           desc      TYPE string,
           order     TYPE zca_e_position,
           datatype  TYPE zca_e_fcat_datatype,
           field_ref TYPE zca_e_fcat_field_ref,
           visible   TYPE sap_bool.
      INCLUDE TYPE zapd_s001.
  TYPES:
           END OF ts_fieldcatalog.
  TYPES tt_fieldcatalog TYPE STANDARD TABLE OF ts_fieldcatalog WITH EMPTY KEY.
  TYPES: BEGIN OF ts_actions,
           action      TYPE zapd_t004-accion,
           label       TYPE zapd_t004t-descripcion,
           icon        TYPE zapd_t004-icon,
           position    TYPE zca_e_position,
           action_type TYPE zapd_t004-act_type,
           is_massive  TYPE zapd_t004-massive,
         END OF ts_actions.
  TYPES tt_actions TYPE STANDARD TABLE OF ts_actions WITH EMPTY KEY.
  TYPES: BEGIN OF ts_additional_info,
           number_notes TYPE i,
         END OF ts_additional_info.
  TYPES: BEGIN OF ts_header_list,
           title   TYPE string,
           data    TYPE tt_value_row,
           catalog TYPE tt_fieldcatalog,
           actions TYPE tt_actions,
         END OF ts_header_list.
  TYPES: BEGIN OF ts_detail_list,
           title    TYPE string,
           data     TYPE tt_value_row,
           catalog  TYPE tt_fieldcatalog,
           actions  TYPE tt_actions,
           add_info TYPE ts_additional_info,
         END OF ts_detail_list.
  TYPES: BEGIN OF ts_notes,
           text_guid      TYPE /cockpit/dinv_guid,
           description    TYPE string,
           note           TYPE string,
           user           TYPE /cockpit/dwc_user_disp,
           user_desc      TYPE string,
           date           TYPE datum,
           time           TYPE uzeit,
           date_time_text TYPE string,
         END OF ts_notes.
  TYPES: tt_notes TYPE STANDARD TABLE OF ts_notes WITH EMPTY KEY.
  TYPES: BEGIN OF ts_user_notif_process,
           username TYPE syuname,
           approved TYPE sap_bool,
           pending  TYPE sap_bool,
         END OF ts_user_notif_process.
  TYPES: tt_user_notif_process TYPE STANDARD TABLE OF ts_user_notif_process WITH EMPTY KEY.

  METHODS get_apps_kpi
    RETURNING
      VALUE(rv_kpi) TYPE numc5 .
  "! <p class="shorttext synchronized">Devuelve listado de documentos pendientes aprobar</p>
  "! Aparte del listado también se devuelve acciones, o cualquier otra información necesaria
  "! para el listado
  "! @parameter es_list | <p class="shorttext synchronized">Listado</p>
  METHODS get_list
    EXPORTING es_list TYPE ts_header_list.

  "! <p class="shorttext synchronized">Devuelve e detalle del documento pasado por parámetro</p>
  "! Aparte del listado también se devuelve acciones, o cualquier otra información necesaria
  "! para el listado
  "! @parameter es_list | <p class="shorttext synchronized">Listado</p>
  METHODS get_detail
    IMPORTING iv_numdoc TYPE any
    EXPORTING es_list   TYPE ts_detail_list
    RAISING   zcx_apd .


  METHODS get_backups
    IMPORTING
      !iv_user          TYPE syuname OPTIONAL
    RETURNING
      VALUE(er_backups) TYPE tr_backup_users .

"! <p class="shorttext synchronized">Devuelve los backups asignados por el usuario</p>
  "! @parameter es_list | <p class="shorttext synchronized">Listado</p>
  METHODS get_my_backups
    IMPORTING
      !iv_user          TYPE syuname OPTIONAL
    RETURNING
      VALUE(er_backups) TYPE tr_backup_users .
  "! <p class="shorttext synchronized">Devuelve información sobre la aplicación</p>
  "! La información será nombre, titulo, icono, orden, etc. Puede ser que no devuelva
  "! nada porque el usuario no tenga que verla por motivos que cada clase implementa.
  "! @parameter et_info | <p class="shorttext synchronized">Información de la aplicación</p>
  METHODS get_app_info
    EXPORTING
      et_info TYPE tt_app_info.

  "! <p class="shorttext synchronized">Devuelve la URL del PDF del documento pasado</p>
  "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
  "! @parameter user | <p class="shorttext synchronized">Usuario</p>
  "! @parameter app | <p class="shorttext synchronized">Aplicación</p>
  "! @parameter numdoc | <p class="shorttext synchronized">Número de documento</p>
  "! @parameter url | <p class="shorttext synchronized">URL</p>
  "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
  METHODS get_pdf
    IMPORTING
              !iv_numdoc TYPE string
    EXPORTING
              !ev_url    TYPE string
              !et_return TYPE bapiret2_t
    RAISING   zcx_apd .

  "! <p class="shorttext synchronized">Aprobación del documento</p>
  "! @parameter it_numdoc | <p class="shorttext synchronized">Pedidos</p>
  "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
  METHODS approve_document
    IMPORTING
              !it_numdoc TYPE zif_apd_data=>tt_numdoc
    EXPORTING
              !et_return TYPE bapiret2_t
    RAISING   zcx_apd .
  "! <p class="shorttext synchronized">Rechazo del documento</p>
  "! @parameter it_numdoc | <p class="shorttext synchronized">Pedidos</p>
  "! @parameter return | <p class="shorttext synchronized">Mensaje de retorno</p>
  METHODS reject_document
    IMPORTING
              !it_numdoc TYPE zif_apd_data=>tt_numdoc
              !iv_reason TYPE string
    EXPORTING
              !et_return TYPE bapiret2_t
    RAISING   zcx_apd.
  "! <p class="shorttext synchronized">Añade una nota</p>
  "! @parameter it_numdoc | <p class="shorttext synchronized">Número de documento</p>
  "! @parameter iv_description | <p class="shorttext synchronized">Descripción</p>
  "! @parameter iv_note | <p class="shorttext synchronized">Cuerpo de la nota</p>
  "! @parameter et_return | <p class="shorttext synchronized">Resultado de la nota</p>
  METHODS add_note
    IMPORTING
              !iv_numdoc      TYPE string
              !iv_description TYPE string OPTIONAL
              !iv_note        TYPE string
    EXPORTING
              !et_return      TYPE bapiret2_t
    RAISING   zcx_apd.
  "! <p class="shorttext synchronized">Listado de notas</p>
  "! @parameter it_numdoc | <p class="shorttext synchronized">Número de documento</p>
  "! @parameter iv_only_header | <p class="shorttext synchronized">Solo datos de cabecera</p>
  "! @parameter et_notes | <p class="shorttext synchronized">Notas</p>
  METHODS get_notes
    IMPORTING
      !iv_numdoc      TYPE string
      !iv_only_header TYPE sap_bool DEFAULT abap_false
    EXPORTING
      !et_notes       TYPE zif_apd_app=>tt_notes.
  "! <p class="shorttext synchronized">Envio de notificaciones</p>
  "! El tipo de notificación dependerá si el usuario ha aprobado o tiene pendiente de aprobar. Incluso
  "! si se ha rechazado el pedido
  "! @parameter it_numdoc | <p class="shorttext synchronized">Número de documento</p>
  "! @parameter iv_doc_rejected | <p class="shorttext synchronized">Documento rechazado</p>
  "! @parameter it_users | <p class="shorttext synchronized">Usuario a notificar</p>
  "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
  METHODS send_notification
    IMPORTING
      !iv_num_doc      TYPE string
      !iv_doc_rejected TYPE sap_bool
      !it_users        TYPE tt_user_notif_process
    EXPORTING
      et_return        TYPE bapiret2_t.
ENDINTERFACE.
