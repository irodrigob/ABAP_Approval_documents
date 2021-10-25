INTERFACE zif_apd_data
  PUBLIC .

  TYPES: tr_app_aprov TYPE RANGE OF zapd_t001-app.
  TYPES: tt_ebeln TYPE STANDARD TABLE OF ekko-ebeln.
  TYPES tt_numdoc TYPE STANDARD TABLE OF string.

  CONSTANTS cv_app_ui5 TYPE zca_e_app_ui5 VALUE 'APD' ##NO_TEXT.
  CONSTANTS cv_msg_type_error TYPE bapi_mtype VALUE 'E' ##NO_TEXT.
  CONSTANTS cv_msg_type_dump TYPE bapi_mtype VALUE 'X' ##NO_TEXT.
  CONSTANTS cv_msg_type_success TYPE bapi_mtype VALUE 'S' ##NO_TEXT.
  CONSTANTS cv_msg_type_warning TYPE bapi_mtype VALUE 'W' ##NO_TEXT.
  CONSTANTS cv_msg_type_info TYPE bapi_mtype VALUE 'I' ##NO_TEXT.
  CONSTANTS cv_msg_id TYPE arbgb VALUE 'ZAPD' ##NO_TEXT.
  CONSTANTS: BEGIN OF cs_app,
               fcat_app             TYPE zca_e_appl VALUE 'ZAPD',
               character_sep_numdoc TYPE c LENGTH 1 VALUE ',',
               BEGIN OF cs_fields,
                 doc_number TYPE fieldname VALUE 'DOCNUM',
               END OF cs_fields,
               BEGIN OF level,
                 header TYPE zapd_e_level VALUE 'CAB',
                 detail TYPE zapd_e_level VALUE 'DET',
               END OF level,
               BEGIN OF action_type,
                 pdf     TYPE zapd_e_tipo_accion VALUE 'PDF',
                 approve TYPE zapd_e_tipo_accion VALUE 'APROVE',
                 reject  TYPE zapd_e_tipo_accion VALUE 'REJECT',
                 url     TYPE zapd_e_tipo_accion VALUE 'URL',
                 note    TYPE zapd_e_tipo_accion VALUE 'NOTE',
               END OF action_type,
               BEGIN OF po_order,
                 kappl TYPE kappl VALUE 'EF',
                 BEGIN OF doc_type,
                   services  TYPE zapd_t002-doc_type VALUE 'SERV',
                   material  TYPE zapd_t002-doc_type VALUE 'MAT',
                   inversion TYPE zapd_t002-doc_type VALUE 'INV',
                 END OF doc_type,
               END OF po_order,
               BEGIN OF invoice,
                 BEGIN OF doc_type,
                   financial TYPE zapd_t002-doc_type VALUE 'FI',
                   order     TYPE zapd_t002-doc_type VALUE 'MM',
                 END OF doc_type,
                 wc_texttype_note TYPE /cockpit/ctxtyp-text_type VALUE '10',
                 BEGIN OF actions,
                   continue TYPE /cockpit/swc_dummy-action_type VALUE 'C',
                 END OF actions,
               END OF invoice,
             END OF cs_app.
  CONSTANTS: BEGIN OF cs_push,
               appl          TYPE zpush_e_appl VALUE 'ONECLICK',
               default_sound TYPE string VALUE 'default',
             END OF cs_push.

ENDINTERFACE.
