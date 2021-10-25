CLASS zcl_apd_app_po DEFINITION
  PUBLIC
  INHERITING FROM zcl_apd_apps
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_apd_apps .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_rel_code_steps,
             rel_code TYPE frgco,
             username TYPE syuname,
             approved TYPE sap_bool,
           END OF ts_rel_code_steps.
    TYPES: tt_rel_code_steps TYPE STANDARD TABLE OF ts_rel_code_steps WITH EMPTY KEY.


    ALIASES get_backups
      FOR zif_apd_app~get_backups .

    CONSTANTS cv_id_app TYPE zapd_t001-app VALUE '01' ##NO_TEXT.

    METHODS zif_apd_app~get_apps_kpi
        REDEFINITION .
    METHODS zif_apd_app~get_app_info
        REDEFINITION .
    METHODS zif_apd_app~get_backups
        REDEFINITION .
    METHODS zif_apd_app~get_my_backups REDEFINITION.
    METHODS zif_apd_app~get_list REDEFINITION.
    METHODS zif_apd_app~get_detail REDEFINITION.
    METHODS zif_apd_app~get_pdf REDEFINITION.
    METHODS zif_apd_app~approve_document REDEFINITION.
    METHODS zif_apd_app~reject_document REDEFINITION.
    METHODS zif_apd_app~send_notification REDEFINITION.
    "! <p class="shorttext synchronized">Devuelve los datos de liberación del handler</p>
    "! @parameter io_handler | <p class="shorttext synchronized">handler de la cabecera de MM</p>
    "! @parameter ev_frgot | <p class="shorttext synchronized">Categoria del objeto</p>
    "! @parameter ev_group | <p class="shorttext synchronized">Grupo de liberación</p>
    "! @parameter ev_group_desc | <p class="shorttext synchronized">Descripción grupo de liberación</p>
    "! @parameter ev_strategy | <p class="shorttext synchronized">Estrategia liberación</p>
    "! @parameter ev_strategy_desc | <p class="shorttext synchronized">Descripción estrategía liberación</p>
    "! @parameter et_steps | <p class="shorttext synchronized">Paso de aprobación</p>
    METHODS get_release_info_from_handler
      IMPORTING
        io_handler       TYPE REF TO cl_po_header_handle_mm
      EXPORTING
        es_header_data   TYPE mepoheader
        ev_frgot         TYPE frgot
        ev_group         TYPE frggr
        ev_group_desc    TYPE frggt
        ev_strategy      TYPE frgsx
        ev_strategy_desc TYPE frgxt
        et_steps         TYPE tt_rel_code_steps
        et_release_codes TYPE merel_t_codes .

    "! <p class="shorttext synchronized">Proceso desde la bapi de liberación</p>
    "! Este proceso trata la casuística de liberación vía BAPI que entra por la exit de
    "! liberación. Para el resto el proceso es el de la BADI
    "! @parameter iv_ebeln | <p class="shorttext synchronized">Número de pedido</p>
    CLASS-METHODS static_rel_action_from_bapi
      IMPORTING iv_ebeln TYPE ekko-ebeln.

  PROTECTED SECTION.

    TYPES:
      tr_ebeln TYPE RANGE OF ekko-ebeln .
    TYPES:
      tr_matnr TYPE RANGE OF ekpo-matnr .
    TYPES:
      tt_release_codes TYPE STANDARD TABLE OF t16fw WITH EMPTY KEY .
    TYPES:
      tt_release_req TYPE STANDARD TABLE OF t16fv WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_header_data,
        ebeln      TYPE ekko-ebeln,
        bsart      TYPE ekko-bsart,
        lifnr      TYPE ekko-lifnr,
        lifnr_desc TYPE lfa1-name1,
        ekorg      TYPE ekko-ekorg,
        bukrs      TYPE t001-bukrs,
        bukrs_desc TYPE t001-butxt,
        amount     TYPE dmbtr,
        waers      TYPE ekko-waers,
        aedat      TYPE ekko-aedat,
        ernam      TYPE ekko-ernam,
        bstyp      TYPE ekko-bstyp,
      END OF ts_header_data .
    TYPES:
      tt_header_data TYPE STANDARD TABLE OF ts_header_data WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_detail_data,
        ebeln           TYPE ekpo-ebeln,
        ebelp           TYPE ekpo-ebelp,
        matnr           TYPE ekpo-matnr,
        netwr           TYPE ekpo-netpr,
        waers           TYPE ekko-waers,
        unit_price      TYPE ekpo-netpr,
        txz01           TYPE ekpo-txz01,
        menge           TYPE ekpo-menge,
        meins           TYPE ekpo-meins,
        sakto           TYPE ekkn-sakto,
        sakto_desc      TYPE string,
        kostl           TYPE ekkn-kostl,
        kostl_desc      TYPE string,
        prctr           TYPE ekkn-prctr,
        prctr_desc      TYPE string,
        fkber           TYPE ekkn-fkber,
        fkber_desc      TYPE string,
        delivery_place  TYPE string,
        eindt           TYPE eket-eindt,
        kndnr           TYPE ce4pa00_acct-kndnr,
        kndnr_desc      TYPE string,
        knttp           TYPE ekpo-knttp,
        anln1           TYPE string,
        anln1_desc      TYPE string,
        aufnr           TYPE ekkn-aufnr,
        aufnr_desc      TYPE string,
        zzimaposnr      TYPE ekkn-zzimaposnr,
        zzimaposnr_desc TYPE string,
      END OF ts_detail_data .
    TYPES:
      tt_detail_data TYPE STANDARD TABLE OF ts_detail_data WITH EMPTY KEY .
    TYPES:
      tt_ekko TYPE STANDARD TABLE OF ekko WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_ekpo,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        matnr TYPE ekpo-matnr,
        netwr TYPE ekpo-netwr,
        netpr TYPE ekpo-netpr,
        peinh TYPE ekpo-peinh,
        txz01 TYPE ekpo-txz01,
        menge TYPE ekpo-menge,
        meins TYPE ekpo-meins,
        adrn2 TYPE ekpo-adrn2,
        knttp TYPE ekpo-knttp,
        brtwr TYPE ekpo-brtwr,
        ktmng TYPE ekpo-ktmng,
      END OF ts_ekpo .
    TYPES:
      tt_ekpo TYPE STANDARD TABLE OF ts_ekpo WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_eket,
        ebeln TYPE eket-ebeln,
        ebelp TYPE eket-ebelp,
        etenr TYPE eket-etenr,
        eindt TYPE eket-eindt,
      END OF ts_eket .
    TYPES:
      tt_eket TYPE STANDARD TABLE OF ts_eket WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_ekkn,
        ebeln           TYPE ekkn-ebeln,
        ebelp           TYPE ekkn-ebelp,
        zekkn           TYPE ekkn-zekkn,
        sakto           TYPE ekkn-sakto,
        sakto_desc      TYPE string,
        kostl           TYPE ekkn-kostl,
        kostl_desc      TYPE string,
        prctr           TYPE ekkn-prctr,
        prctr_desc      TYPE string,
        fkber           TYPE ekkn-fkber,
        fkber_desc      TYPE string,
        paobjnr         TYPE ekkn-paobjnr,
        anln1           TYPE ekkn-anln1,
        anln2           TYPE ekkn-anln1,
        anln1_desc      TYPE string,
        aufnr           TYPE ekkn-aufnr,
        aufnr_desc      TYPE string,
        zzimaposnr      TYPE ekkn-zzimaposnr,
        zzimaposnr_desc TYPE string,
      END OF ts_ekkn .
    TYPES:
      tt_ekkn TYPE STANDARD TABLE OF ts_ekkn WITH EMPTY KEY .
    TYPES: BEGIN OF ts_objectpa_info,
             paobjnr    TYPE ce4pa00_acct-paobjnr,
             kndnr      TYPE ce4pa00_acct-kndnr,
             kndnr_desc TYPE string,
           END OF ts_objectpa_info.
    TYPES: tt_objectpa_info TYPE STANDARD TABLE OF ts_objectpa_info WITH EMPTY KEY.
    TYPES:
      tt_address TYPE STANDARD TABLE OF sadr WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_makt,
        matnr TYPE matnr,
        maktx TYPE maktx,
      END OF ts_makt .
    TYPES:
      tt_makt TYPE STANDARD TABLE OF ts_makt WITH EMPTY KEY .
    "! <p class="shorttext synchronized">Determina si el proceso de liberacion se llama vía BAPI</p>
    "! @parameter rv_is | <p class="shorttext synchronized">Se llama</p>
    CLASS-METHODS call_rel_action_from_bapi
      RETURNING
        VALUE(rv_is) TYPE abap_bool.
    DATA:
      mt_r_service_bsart TYPE RANGE OF ekko-bsart .
    DATA mt_r_inversion_knttp TYPE RANGE OF ekpo-knttp.
    DATA mt_address TYPE tt_address .

    "! <p class="shorttext synchronized">Obtienen los códigos de liberación de usuario y backups</p>
    "! @parameter et_release_codes | <p class="shorttext synchronized">Código liberación</p>
    METHODS get_release_codes
      EXPORTING
        !et_release_codes TYPE zcl_apd_app_po=>tt_release_codes .
    "! <p class="shorttext synchronized">Obtiene los requisitos de liberación</p>
    "! Datos necesarios para buscar los pedidps
    "! @parameter et_release_codes | <p class="shorttext synchronized">Código liberación</p>
    METHODS get_release_req
      EXPORTING
        !et_release_req TYPE zcl_apd_app_po=>tt_release_req .
    "! <p class="shorttext synchronized">Datos de cabecera del pedido</p>
    "! @parameter it_r_ebeln | <p class="shorttext synchronized">Numeros de pedido</p>
    "! @parameter et_data | <p class="shorttext synchronized">Datos de los pedidos</p>
    METHODS get_purchase_header_info
      IMPORTING
        !it_r_ebeln TYPE tr_ebeln OPTIONAL
      EXPORTING
        !et_data    TYPE zcl_apd_app_po=>tt_header_data .
    "! <p class="shorttext synchronized">lectura de la ekko</p>
    "! Se leen los pedidos pendientes de aprobar por el usuario o de los que es sustituto
    "! @parameter it_r_ebeln | <p class="shorttext synchronized">Numeros de pedido</p>
    "! @parameter et_ekko | <p class="shorttext synchronized">Datos cabecera de pedido</p>
    "! @parameter et_release_req | <p class="shorttext synchronized">Requisitos de liberación</p>
    METHODS read_ekko
      IMPORTING
        !it_r_ebeln     TYPE tr_ebeln OPTIONAL
      EXPORTING
        !et_ekko        TYPE tt_ekko
        !et_release_req TYPE tt_release_req .
    "! <p class="shorttext synchronized">lectura de la ekpo</p>
    "! Datos de posición del pedido
    "! @parameter it_r_ebeln | <p class="shorttext synchronized">Numeros de pedido</p>
    "! @parameter et_ekpo | <p class="shorttext synchronized">Posiciones del pedido</p>
    METHODS read_ekpo
      IMPORTING
        !it_r_ebeln TYPE tr_ebeln OPTIONAL
      EXPORTING
        !et_ekpo    TYPE tt_ekpo .
    "! <p class="shorttext synchronized">lectura de repartos</p>
    "! @parameter it_r_ebeln | <p class="shorttext synchronized">Numeros de pedido</p>
    "! @parameter et_eket | <p class="shorttext synchronized">Repartos del pedido</p>
    METHODS read_eket
      IMPORTING
        !it_r_ebeln TYPE tr_ebeln OPTIONAL
      EXPORTING
        !et_eket    TYPE tt_eket .
    "! <p class="shorttext synchronized">Lectura de imputaciones</p>
    "! @parameter iv_bukrs | <p class="shorttext synchronized">Sociedad</p>
    "! @parameter it_r_ebeln | <p class="shorttext synchronized">Numeros de pedido</p>
    "! @parameter et_ekkn | <p class="shorttext synchronized">Imputaciones</p>
    "! @parameter et_objectpa_info | <p class="shorttext synchronized">Datos del objecto PA</p>
    METHODS read_ekkn
      IMPORTING
        !iv_bukrs         TYPE ekko-bukrs
        !it_r_ebeln       TYPE tr_ebeln OPTIONAL
      EXPORTING
        !et_ekkn          TYPE tt_ekkn
        !et_objectpa_info TYPE tt_objectpa_info .
    "! <p class="shorttext synchronized">Datos del detalle del pedido</p>
    "! @parameter is_header_data | <p class="shorttext synchronized">Cabecera del pedido</p>
    "! @parameter et_data | <p class="shorttext synchronized">Datos de los pedidos</p>
    METHODS get_purchase_detail_info
      IMPORTING
        !is_header_data TYPE ts_header_data
      EXPORTING
        !et_data        TYPE zcl_apd_app_po=>tt_detail_data .
    "! <p class="shorttext synchronized">Devuelve si el pedido es de servicios</p>
    "! @parameter iv_bsart | <p class="shorttext synchronized">Clase de pedido</p>
    "! @parameter rv_is | <p class="shorttext synchronized">Es de servicios</p>
    METHODS is_po_service
      IMPORTING
        !iv_bsart    TYPE ekko-bsart
      RETURNING
        VALUE(rv_is) TYPE sap_bool .
    "! <p class="shorttext synchronized">Devuelve si el pedido es de inversion</p>
    "! @parameter iv_knttp | <p class="shorttext synchronized">Tipo de imputación</p>
    "! @parameter rv_is | <p class="shorttext synchronized">Es de servicios</p>
    METHODS is_po_inversion
      IMPORTING
        !iv_knttp    TYPE ekpo-knttp
      RETURNING
        VALUE(rv_is) TYPE sap_bool .
    "! <p class="shorttext synchronized">Datos de dirección</p>
    "! @parameter iv_addrnumber | <p class="shorttext synchronized">Clase de pedido</p>
    "! @parameter es_address | <p class="shorttext synchronized">Datos dirección</p>
    METHODS get_address_data
      IMPORTING
        !iv_addrnumber TYPE ekpo-adrn2
      EXPORTING
        !es_address    TYPE sadr .
    METHODS read_makt
      IMPORTING
        iv_langu   TYPE sy-langu
        it_r_matnr TYPE tr_matnr
      EXPORTING
        et_makt    TYPE tt_makt .
    "! <p class="shorttext synchronized">Envio mail de rechazo</p>
    "! @parameter is_ekko | <p class="shorttext synchronized">Datos del pedido</p>
    "! @parameter iv_reason | <p class="shorttext synchronized">Resultad</p>
    METHODS send_mail_reject
      IMPORTING
        is_ekko   TYPE ekko
        iv_reason TYPE string
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Obtiene el handler de la cabecera del pedido a partir de su numero</p>
    "! @parameter iv_ebeln | <p class="shorttext synchronized">Numero de pedido</p>
    "! @parameter rv_handler | <p class="shorttext synchronized">Clase que gestiona la cabecera</p>
    METHODS get_handler_header_from_po
      IMPORTING iv_ebeln          TYPE ekko-ebeln
      RETURNING VALUE(rv_handler) TYPE REF TO cl_po_header_handle_mm.
    "! <p class="shorttext synchronized">Obtiene el handler de la cabecera del pedido desde la exit/badis</p>
    "! @parameter rv_handler | <p class="shorttext synchronized">Clase que gestiona la cabecera</p>
    METHODS get_handler_header_from_badi
      RETURNING VALUE(rv_handler) TYPE REF TO cl_po_header_handle_mm.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_apd_app_po IMPLEMENTATION.


  METHOD call_rel_action_from_bapi.
    DATA lt_callstack TYPE abap_callstack.
    DATA lt_r_function TYPE RANGE OF string.
    DATA lt_r_excep_tcode TYPE RANGE OF sytcode.
    rv_is = abap_false.

    " Recuperamos las bapi de liberación donde el proceso de notificación no va por BADI
    zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'BAPI_RELEASE_%'
                                                    CHANGING t_ranges = lt_r_function ).

* Si no hay datos ni me molesto en continuar.
*    IF lt_r_function IS NOT INITIAL.
*
*      CALL FUNCTION 'SYSTEM_CALLSTACK'
*        IMPORTING
*          callstack = lt_callstack.
*
*      LOOP AT lt_callstack TRANSPORTING NO FIELDS
*                              WHERE blocktype = 'FUNCTION'
*                                    AND  blockname IN lt_r_function.
*        EXIT.
*      ENDLOOP.
*      IF sy-subrc = 0.

    " Ahora miro si en la pila de llamadas esta el propio de obtener datos, si es así le digo que no esta para
    " que no se produzca un bucle infinito. El motivo es que la lectura de datos  entra en la misma exit que la BAPI.
    LOOP AT lt_callstack TRANSPORTING NO FIELDS WHERE blocktype = 'METHOD'
                                                      AND blockname = 'GET_HANDLER_HEADER_FROM_PO'
                                                      AND mainprogram CS 'ZCL_APD_APP_PO'.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      rv_is = abap_true.
    ENDIF.
*      ELSE.
*        " Si no existe miro si se esta llamando desde las transaccion que entran por los mismos sitios que la BAPIs. En ese
*        " caso lo que hago es mirar si se llama desde el obtener datos de la clase de obtener datos. El mismo problema que se explica antes del bucle infinito.
*        zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'TCODE_EXIT_BAPI_REL_ACT_%'
*                                                 CHANGING t_ranges = lt_r_excep_tcode ).
*        IF sy-tcode IN lt_r_excep_tcode AND lt_r_excep_tcode IS NOT INITIAL.
*          LOOP AT lt_callstack TRANSPORTING NO FIELDS WHERE blocktype = 'METHOD'
*                                                           AND blockname = 'GET_HANDLER_HEADER_FROM_PO'
*                                                           AND mainprogram CS 'ZCL_APD_APP_PO'.
*            EXIT.
*          ENDLOOP.
*          IF sy-subrc NE 0.
*            rv_is = abap_true.
*          ENDIF.
*        ENDIF.
*      ENDIF.

*    ENDIF.
  ENDMETHOD.


  METHOD get_address_data.

    CLEAR: es_address.

    READ TABLE mt_address INTO es_address WITH KEY adrnr = iv_addrnumber.
    IF sy-subrc NE 0.
      DATA(ls_adr_sel) = VALUE addr1_sel( addrnumber = iv_addrnumber date = '00010101' ).
      CALL FUNCTION 'ADDR_GET'
        EXPORTING
          address_selection = ls_adr_sel
          read_sadr_only    = abap_false
        IMPORTING
          sadr              = es_address
        EXCEPTIONS
          parameter_error   = 1                " Incorrect parameter values
          address_not_exist = 2                " Address does not exist
          version_not_exist = 3                " International version of the address does not exist
          internal_error    = 4                " Serious internal error (MESSAGE A...)
          address_blocked   = 5                " Address is blocked
          OTHERS            = 6.
      IF sy-subrc = 0.
        INSERT es_address INTO TABLE mt_address.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_handler_header_from_badi.
    FIELD-SYMBOLS <lo_handle> TYPE REF TO cl_screen_view_mm.
    FIELD-SYMBOLS <lo_handle_user_mm> TYPE REF TO cl_po_header_handle_mm.

    CLEAR: rv_handler.

    TRY.
        " Recupero la clase donde se gestiona todas las vistas del pedido de compras
        DATA(lv_class) = |(SAPLMEGUI)CALL_VIEW|.
        ASSIGN (lv_class) TO <lo_handle>.
        IF <lo_handle> IS ASSIGNED.

          " Recupero las subvistas
          DATA(lt_subviews) = <lo_handle>->get_subviews(  ).

          " En el index 1 tenemos la vista que gestiona el documento de compras
          READ TABLE lt_subviews ASSIGNING FIELD-SYMBOL(<ls_subviews>) WITH KEY index = 1.
          IF sy-subrc = 0.
            " En el atributo MY_MODEL de la clase local tenemos la clase que gestiona la cabecera de pedido de compras
            ASSIGN <ls_subviews>-view->('MY_MODEL') TO FIELD-SYMBOL(<my_model>).

            IF sy-subrc = 0.

              " Del modelo lo pasa al parámetro de salida
              rv_handler = CAST cl_po_header_handle_mm( <my_model> ).

            ENDIF.
          ENDIF.

        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD get_handler_header_from_po.
    DATA lv_tcode TYPE sytcode.
    DATA lv_trtyp TYPE t160-trtyp.
    DATA lt_r_bstyp_contract TYPE RANGE OF ekko-bstyp.

    CLEAR: rv_handler.

    IF iv_ebeln IS NOT INITIAL.

      TRY.
          " Saco el tipo de documento de compras porque dependerá de la transacción que se informe para
          " recuperar el handler de MM
          SELECT bstyp INTO @DATA(lv_bstyp) UP TO 1 ROWS
                 FROM ekko
                 WHERE ebeln = @iv_ebeln
                 ORDER BY PRIMARY KEY.
          ENDSELECT.
          IF sy-subrc = 0.

            " La transacción que se le pasará dependerá del tipo de documento

            zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_CONTRACTS_BSTYP_%'
                                                                CHANGING  t_ranges = lt_r_bstyp_contract ).
            IF lv_bstyp IN lt_r_bstyp_contract AND lt_r_bstyp_contract IS NOT INITIAL.
              zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'HANDLER_TCODE_CONTRACT'
                                                    IMPORTING c_valor = lv_tcode ).

            ELSE.
              zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'HANDLER_TCODE_PO'
                                                                IMPORTING c_valor = lv_tcode ).
            ENDIF.


            lv_trtyp = 'A'. " Visualizar

            rv_handler = NEW cl_po_header_handle_mm( im_po_number = iv_ebeln ).

            rv_handler->po_initialize( ).
            rv_handler->po_read( EXPORTING im_tcode     = lv_tcode
                                         im_trtyp     = lv_trtyp
                                         im_aktyp     = lv_trtyp
                                         im_po_number = iv_ebeln ).

          ENDIF.

        CATCH cx_root.

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_purchase_detail_info.
    DATA lt_r_bstyp_contract TYPE RANGE OF ekko-bstyp.

    CLEAR: et_data.


    " Obtenemos los datos de las posiciones del pedido
    read_ekpo( EXPORTING it_r_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = is_header_data-ebeln ) )
               IMPORTING et_ekpo    = DATA(lt_ekpo) ).

    " Datos de los repartos
    read_eket( EXPORTING it_r_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = is_header_data-ebeln ) )
               IMPORTING et_eket    = DATA(lt_eket) ).

    " Datos de las imputaciones
    read_ekkn( EXPORTING it_r_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = is_header_data-ebeln ) )
                         iv_bukrs = is_header_data-bukrs
                   IMPORTING et_ekkn    = DATA(lt_ekkn)
                             et_objectpa_info = DATA(lt_objectpa_info) ).

    zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_DOC_TYPE_%'
                                                              CHANGING  t_ranges = lt_r_bstyp_contract ).

    LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<ls_ekpo>).
      INSERT CORRESPONDING #( <ls_ekpo> ) INTO TABLE et_data ASSIGNING FIELD-SYMBOL(<ls_data>).

      " En contratos el valor neto sale del valor bruto porque el neto esta en blanco
      IF is_header_data-bstyp IN lt_r_bstyp_contract AND lt_r_bstyp_contract IS NOT INITIAL.
        <ls_data>-netwr = <ls_ekpo>-brtwr.
      ENDIF.

      <ls_data>-waers = is_header_data-waers. " Moneda

      " Datos de reparto
      READ TABLE lt_eket ASSIGNING FIELD-SYMBOL(<ls_eket>) WITH KEY ebelp = <ls_ekpo>-ebelp.
      IF sy-subrc = 0.
        <ls_data>-eindt = <ls_eket>-eindt.
      ENDIF.


      " Datos de imputacion
      READ TABLE lt_ekkn ASSIGNING FIELD-SYMBOL(<ls_ekkn>) WITH KEY ebelp = <ls_ekpo>-ebelp.
      IF sy-subrc = 0.
        <ls_data> = CORRESPONDING #( BASE ( <ls_data> ) <ls_ekkn> ).

        " Datos del objeto PA
        READ TABLE lt_objectpa_info ASSIGNING FIELD-SYMBOL(<ls_objectpa_info>) WITH KEY paobjnr = <ls_ekkn>-paobjnr.
        IF sy-subrc = 0.
          <ls_data> = CORRESPONDING #( BASE ( <ls_data> ) <ls_objectpa_info> ).
        ENDIF.

        " El tema de activo lo trato de manera distinto porque concateno el numero de activo y subnumero
        <ls_data>-anln1 = |{ <ls_ekkn>-anln1 ALPHA = OUT } { <ls_ekkn>-anln2 ALPHA = OUT }|.
      ENDIF.


      " Datos de la direccion
      IF <ls_ekpo>-adrn2 IS NOT INITIAL.
        get_address_data( EXPORTING iv_addrnumber = <ls_ekpo>-adrn2
                          IMPORTING es_address    = DATA(ls_address) ).
        <ls_data>-delivery_place = ls_address-name1.
      ENDIF.

      " Precio unitario
      IF <ls_ekpo>-peinh IS NOT INITIAL.
        <ls_data>-unit_price = <ls_ekpo>-netpr / <ls_ekpo>-peinh.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_purchase_header_info.
    DATA lt_r_ebeln TYPE tr_ebeln.
    DATA lt_r_lifnr TYPE RANGE OF lfa1-lifnr.
    DATA lt_r_bukrs TYPE RANGE OF bukrs.
    DATA lt_r_bstyp_contract TYPE RANGE OF ekko-bstyp.

    CLEAR: et_data.

    " Obtenemos los datos de la cabecera del pedido
    read_ekko( EXPORTING it_r_ebeln = it_r_ebeln
               IMPORTING et_ekko = DATA(lt_ekko) ).

    IF lt_ekko IS NOT INITIAL.

      " Extraemos datos para montar ranges
      LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_ekko>-ebeln ) INTO TABLE lt_r_ebeln.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_ekko>-lifnr ) INTO TABLE lt_r_lifnr.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_ekko>-bukrs ) INTO TABLE lt_r_bukrs.
      ENDLOOP.

      " Sacamos los datos de posición
      read_ekpo( EXPORTING it_r_ebeln = lt_r_ebeln
                 IMPORTING et_ekpo = DATA(lt_ekpo) ).

      " Descripcion de los proveedores
      SELECT lifnr,name1 INTO TABLE @DATA(lt_vendors)
             FROM lfa1
             WHERE lifnr IN @lt_r_lifnr.

      SELECT bukrs, butxt INTO TABLE @DATA(lt_t001)
      FROM t001
      WHERE bukrs IN @lt_r_bukrs.

      " Tipo de documento de contratos
      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_CONTRACTS_BSTYP_%'
                                                          CHANGING  t_ranges = lt_r_bstyp_contract ).

      LOOP AT lt_ekko ASSIGNING <ls_ekko>.
        READ TABLE lt_ekpo TRANSPORTING NO FIELDS WITH KEY ebeln = <ls_ekko>-ebeln.
        IF sy-subrc EQ 0.
          APPEND INITIAL LINE TO et_data ASSIGNING FIELD-SYMBOL(<ls_data>).
          <ls_data> = CORRESPONDING #( <ls_ekko> ).

          " Descripción del vendedor
          READ TABLE lt_vendors ASSIGNING FIELD-SYMBOL(<ls_vendors>) WITH KEY lifnr = <ls_data>-lifnr.
          IF sy-subrc = 0.
            <ls_data>-lifnr_desc = <ls_vendors>-name1.
          ENDIF.

          " Descripcion sociedad
          READ TABLE lt_t001 ASSIGNING FIELD-SYMBOL(<ls_t001>) WITH KEY bukrs = <ls_data>-bukrs.
          IF sy-subrc = 0.
            <ls_data>-bukrs_desc = <ls_t001>-butxt.
          ENDIF.


          " Importe
          " En contratos el valor neto sale del valor bruto porque el neto esta en blanco
          IF <ls_ekko>-bstyp IN lt_r_bstyp_contract AND lt_r_bstyp_contract IS NOT INITIAL.
            <ls_data>-amount = REDUCE #( INIT x = 0 FOR <wa> IN lt_ekpo WHERE ( ebeln = <ls_ekko>-ebeln ) NEXT x = x + <wa>-brtwr ).
          ELSE.
            <ls_data>-amount = REDUCE #( INIT x = 0 FOR <wa> IN lt_ekpo WHERE ( ebeln = <ls_ekko>-ebeln ) NEXT x = x + <wa>-netwr ).
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_release_codes.

    CLEAR: et_release_codes.

    " Recuperamos los usuarios de los cuales soy backup
    DATA(lr_users) = get_backups( ).
    INSERT VALUE #( sign = 'I' option = 'EQ' low = mv_user ) INTO TABLE lr_users.

    SELECT *
      FROM t16fw
      INTO TABLE et_release_codes
      WHERE objid IN lr_users.

  ENDMETHOD.


  METHOD get_release_info_from_handler.

    CLEAR: ev_frgot, ev_group, ev_group_desc, ev_strategy, ev_strategy_desc, es_header_data, et_steps.
    CLEAR: et_release_codes.

    IF io_handler IS BOUND.

      TRY.

          " Obtengo los datos de la cabecera
          io_handler->get_data( IMPORTING ex_data = es_header_data ).

          " Si el pedido no tiene estrategias de liberación no se realiza ningun paso
          IF es_header_data-frggr IS NOT INITIAL AND es_header_data-frgsx IS NOT INITIAL.

            " Obtengo las clases que gestionan las liberaciones
            io_handler->if_releasable_mm~get_data( IMPORTING ex_strategy = DATA(lo_strategy)
                                                                 ex_state = DATA(lo_state) ).

            IF lo_strategy IS BOUND AND lo_state IS BOUND.

              " Información general
              lo_strategy->get_info(
                IMPORTING
                  ex_frgot            = ev_frgot
                  ex_group            = ev_group
                  ex_group_desc       = ev_group_desc
                  ex_strategy         = ev_strategy
                  ex_strategy_desc    = ev_strategy_desc
                  ex_codes            = et_release_codes ).


              " Los estados. En esta tabla están quien ha realizado ya la aprobación.
              DATA(lt_states_codes) = lo_state->get_state( ).

              " Los aprobadores se devuelven en ex_codes. Lo que hago es obtener el usuario SAP de cada uno.
              LOOP AT et_release_codes ASSIGNING FIELD-SYMBOL(<ls_codes>).
                " Sacamos los usuarios SAP del codigo de liberación
                DATA(lt_responsibles) = io_handler->if_releasable_mm~get_responsible( <ls_codes>-rel_code ).

                " Miramos si para ese paso se ha realizado la aprobación
                DATA(lv_approved) = abap_false.
                READ TABLE lt_states_codes TRANSPORTING NO FIELDS WITH KEY rel_code = <ls_codes>-rel_code.
                IF sy-subrc = 0.
                  lv_approved = abap_true.
                ENDIF.

                LOOP AT lt_responsibles ASSIGNING FIELD-SYMBOL(<ls_responsibles>).
                  INSERT VALUE #( rel_code = <ls_codes>-rel_code username = <ls_responsibles>-objid approved = lv_approved ) INTO TABLE et_steps.
                ENDLOOP.
                IF sy-subrc NE 0. " Si no existe es porque no hay usuario SAP pero hay que añadir el codigo de liberación
                  INSERT VALUE #( rel_code = <ls_codes>-rel_code approved = lv_approved ) INTO TABLE et_steps.
                ENDIF.
                CLEAR lt_responsibles.
              ENDLOOP.
            ENDIF.

          ENDIF.

        CATCH cx_root.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_release_req.
    DATA lt_t16fv_aux TYPE STANDARD TABLE OF t16fv.
    DATA lt_frggr TYPE RANGE OF frggr.

    CLEAR: et_release_req.

    " Recuperamos los codigos de liberación asignan a mi usuario o quien hago de backup
    get_release_codes( IMPORTING et_release_codes = DATA(lt_release_codes) ).

    LOOP AT lt_release_codes ASSIGNING FIELD-SYMBOL(<fs_t16fw>).
      CLEAR: lt_t16fv_aux, lt_frggr.

      INSERT VALUE #( sign = 'I' option = 'EQ' low = <fs_t16fw>-frggr ) INTO TABLE lt_frggr.

      CALL FUNCTION 'ME_REL_CHECK_MANY'
        EXPORTING
          i_frgot       = '2'
          i_frgco       = <fs_t16fw>-frgco
        TABLES
          t_frggr       = lt_frggr
          t_t16fv       = lt_t16fv_aux
        EXCEPTIONS
          error_message = 98.

      APPEND LINES OF lt_t16fv_aux TO et_release_req.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_po_inversion.
    rv_is = abap_false.

    IF mt_r_inversion_knttp IS INITIAL.
      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_KNTTP_INV_%'
                                                       CHANGING t_ranges = mt_r_inversion_knttp ).
    ENDIF.

    IF iv_knttp IN mt_r_inversion_knttp AND mt_r_inversion_knttp IS NOT INITIAL.
      rv_is = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_po_service.

    rv_is = abap_false.

    IF mt_r_service_bsart IS INITIAL.
      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_SERVICE_BSART_%'
                                                       CHANGING t_ranges = mt_r_service_bsart ).
    ENDIF.

    IF iv_bsart IN mt_r_service_bsart AND mt_r_service_bsart IS NOT INITIAL.
      rv_is = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD read_eket.
    CLEAR: et_eket.

    SELECT  ebeln ebelp etenr eindt INTO TABLE et_eket
            FROM eket
            WHERE ebeln IN it_r_ebeln.

  ENDMETHOD.


  METHOD read_ekkn.

    CLEAR: et_ekkn, et_objectpa_info.

    SELECT  ktopl INTO @DATA(lv_ktopl) UP TO 1 ROWS
           FROM t001
           WHERE bukrs = @iv_bukrs
           ORDER BY PRIMARY KEY.
    ENDSELECT.

    SELECT a~ebeln a~ebelp a~zekkn a~sakto b~txt50 AS sakto_desc a~kostl c~ltext AS kostl_desc a~prctr d~ltext AS prctr_desc
           a~fkber e~fkbtx AS fkber_desc a~paobjnr a~anln1 a~anln2 h~txt50 AS anln1_desc a~aufnr i~ktext AS aufnr_desc
           a~zzimaposnr
           INTO CORRESPONDING FIELDS OF TABLE et_ekkn
           FROM ekkn AS a LEFT OUTER JOIN skat AS b ON
                b~saknr = a~sakto
                AND b~ktopl = lv_ktopl
                AND b~spras = mv_langu
                LEFT OUTER JOIN cskt AS c ON
                c~kostl = a~kostl
                AND c~kokrs = a~kokrs
                AND c~spras = mv_langu
                LEFT OUTER JOIN cepct AS d ON
                d~prctr = a~prctr
                AND d~datbi >= sy-datum
                AND d~kokrs = a~kokrs
                AND d~spras = mv_langu
                LEFT OUTER JOIN tfkbt AS e ON
                e~fkber = a~fkber
                AND e~spras = mv_langu
                LEFT OUTER JOIN anla AS h ON
                h~anln1 = a~anln1
                AND h~anln2 = a~anln2
                AND h~bukrs = iv_bukrs
                LEFT OUTER JOIN aufk AS i ON
                i~aufnr = a~aufnr
           WHERE a~ebeln IN it_r_ebeln.

    " Hay casos donde puede haber varias imputaciones. En ese caso se toma la primera, por
    " ello borro duplicados por pedidos para tener un solo registro.
    SORT et_ekkn BY ebeln ebelp zekkn.
    DELETE ADJACENT DUPLICATES FROM et_ekkn COMPARING ebeln ebelp.

    IF sy-subrc = 0.

      " Datos de PA de la posición. Como los datos que se manejarán son pocos hago uso del for all entries
      SELECT a~paobjnr a~kndnr b~name1 AS kndnr_desc
             INTO TABLE et_objectpa_info
             FROM ce4pa00_acct AS a LEFT OUTER JOIN kna1 AS b ON
                  b~kunnr = a~kndnr
             FOR ALL ENTRIES IN et_ekkn
             WHERE paobjnr = et_ekkn-paobjnr.

      " Busco los textos del OMP. No tengo en cuenta el idioma porque habrá cierta lógica.
      SELECT posnr, spras, txt50 INTO TABLE @DATA(lt_imakt)
             FROM imakt
             FOR ALL ENTRIES IN @et_ekkn
             WHERE posnr = @et_ekkn-zzimaposnr.
      IF sy-subrc = 0.
        LOOP AT et_ekkn ASSIGNING FIELD-SYMBOL(<ls_ekkn>).
          " Primero se busca el texto en el idioma de la clase. Si no existe, en el primero que se encuentre.
          READ TABLE lt_imakt ASSIGNING FIELD-SYMBOL(<ls_imakt>) WITH KEY posnr = <ls_ekkn>-zzimaposnr
                                                                          spras = mv_langu.
          IF sy-subrc NE 0.
            READ TABLE lt_imakt ASSIGNING <ls_imakt> WITH KEY posnr = <ls_ekkn>-zzimaposnr.
          ENDIF.
          IF <ls_imakt> IS ASSIGNED.
            <ls_ekkn>-zzimaposnr_desc = <ls_imakt>-txt50.
          ENDIF.
          UNASSIGN <ls_imakt>.

        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD read_ekko.
    DATA lr_procstat     TYPE RANGE OF meprocstate.
    DATA lt_r_bstyp TYPE RANGE OF ekko-bstyp.
    DATA lt_r_frgke TYPE RANGE OF ekko-frgke.
    DATA lt_r_frggr TYPE RANGE OF ekko-frggr.
    DATA lt_r_frgsx TYPE RANGE OF ekko-frgsx.
    DATA ls_ekko_perform TYPE ekko.
    DATA lt_ekko_perform LIKE TABLE OF ls_ekko_perform.
    DATA ls_zus_perform  TYPE t16fv.
    DATA lt_zus_perform  LIKE TABLE OF ls_zus_perform.
    DATA lt_r_ebeln TYPE tr_ebeln.


    CLEAR: et_ekko, et_release_req.

    " Obtiene los requisitos de liberación
    get_release_req( IMPORTING et_release_req = et_release_req ).

    SORT et_release_req BY frggr frgsx.

    IF et_release_req IS NOT INITIAL.

      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PROCSTAT_%'
                                                       CHANGING  t_ranges = lr_procstat ).
      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_DOC_TYPE_%'
                                                          CHANGING  t_ranges = lt_r_bstyp ).
      zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'PO_IND_LIBERACION_%'
                                                                CHANGING  t_ranges = lt_r_frgke ).

      " Paso el grupo y estrategia a ranges para mejor la búsqueda
      LOOP AT et_release_req ASSIGNING FIELD-SYMBOL(<ls_release_req>).
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_release_req>-frggr ) INTO TABLE lt_r_frggr.
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_release_req>-frgsx ) INTO TABLE lt_r_frgsx.
      ENDLOOP.
      SORT lt_r_frggr.
      DELETE ADJACENT DUPLICATES FROM lt_r_frggr COMPARING ALL FIELDS.
      SORT lt_r_frgsx.
      DELETE ADJACENT DUPLICATES FROM lt_r_frgsx COMPARING ALL FIELDS.

*      SELECT * INTO TABLE @DATA(lt_ekko)
*        FROM ekko
*        FOR ALL ENTRIES IN @et_release_req
*         WHERE frgrl EQ @abap_true
*           AND frggr EQ @et_release_req-frggr
*           AND frgsx EQ @et_release_req-frgsx
*           AND ekko~bstyp IN @lt_r_bstyp
*           AND loekz EQ @space
*           AND ebeln IN @it_r_ebeln
*           AND frgke IN @lt_r_frgke
*           AND ( procstat IN @lr_procstat OR procstat IS NULL ).

      SELECT * INTO TABLE @DATA(lt_ekko)
        FROM ekko
         WHERE frgrl EQ @abap_true
           AND frggr IN @lt_r_frggr
           AND frgsx IN @lt_r_frgsx
           AND ekko~bstyp IN @lt_r_bstyp
           AND loekz EQ @space
           AND ebeln IN @it_r_ebeln
           AND frgke IN @lt_r_frgke
           AND ( procstat IN @lr_procstat OR procstat IS NULL ).

      IF sy-subrc = 0.

        " Recorremos los pedidos para:
        "- verificar que cumple con las combinaciones de grupo y estrategia
        "- Extraer datos para montar ranges
        LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).
          DATA(lv_tabix) = sy-tabix.
          READ TABLE et_release_req TRANSPORTING NO FIELDS
                                    WITH KEY frggr = <ls_ekko>-frggr
                                             frgsx = <ls_ekko>-frgsx.
          IF sy-subrc NE 0.
            DELETE lt_ekko INDEX lv_tabix.
          ENDIF.
        ENDLOOP.

        IF lt_ekko IS NOT INITIAL.

          " Se llama al mismo método de la ZME28 para verificar los códigos y permisos
          lt_ekko_perform = CORRESPONDING #( lt_ekko ).
          lt_zus_perform = CORRESPONDING #( et_release_req ).
          SORT lt_zus_perform BY mandt frggr frgsx.

          PERFORM f_verificar_cond_lib_ui5 IN PROGRAM zmml059  USING    lt_zus_perform[]
                                                                  CHANGING lt_ekko_perform[] IF FOUND.

          IF lt_ekko_perform IS NOT INITIAL.
            et_ekko = CORRESPONDING #( lt_ekko_perform ).

            lt_r_ebeln = VALUE #( FOR <wa> IN et_ekko ( sign = 'I' option = 'EQ' low = <wa>-ebeln ) ).


            " Sacamos los datos de posición, para ver si tiene alguna valida.
            read_ekpo( EXPORTING it_r_ebeln = lt_r_ebeln
                       IMPORTING et_ekpo = DATA(lt_ekpo) ).

            LOOP AT et_ekko ASSIGNING FIELD-SYMBOL(<fs_ekko>).
              DATA(lv_index) = sy-tabix.
              READ TABLE lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>) WITH KEY ebeln = <fs_ekko>-ebeln.
              IF sy-subrc NE 0.
                DELETE et_ekko INDEX lv_index.
              ENDIF.
            ENDLOOP.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD read_ekpo.

    CLEAR: et_ekpo.

    SELECT ebeln ebelp matnr netwr netpr peinh txz01 menge meins adrn2 knttp brtwr ktmng
           INTO TABLE et_ekpo
           FROM ekpo
           WHERE ebeln IN it_r_ebeln
                 AND loekz = space
           ORDER BY ebeln ebelp.
    IF sy-subrc = 0.

      " INIMOD 18/10/2021
      " Para contratos el campo cantidad viene en el campo KTMNG, por ello
      " si el campo cantidad normal esta en blanco y el de cantidad prevista informado
      " se rellena en el campo cantidad el de cantidad prevista.
      LOOP AT et_ekpo ASSIGNING FIELD-SYMBOL(<ls_ekpo>) WHERE ktmng IS NOT INITIAL
                                                              AND menge IS INITIAL.
        <ls_ekpo>-menge = <ls_ekpo>-ktmng.
      ENDLOOP.

      " FINMOD 18/10/2021
    ENDIF.
  ENDMETHOD.


  METHOD read_makt.

    CLEAR: et_makt.

    SELECT matnr maktx INTO TABLE et_makt
           FROM makt
           WHERE matnr IN it_r_matnr
           AND spras = iv_langu.
  ENDMETHOD.


  METHOD send_mail_reject.


    " Se busca el usuario del grupo de compras
    SELECT SINGLE zusuario FROM zm013 INTO @DATA(lv_zusuario)
     WHERE ekgrp = @is_ekko-ekgrp.
    IF sy-subrc NE 0. " Si no existe se envia al creador del pedido
      lv_zusuario = is_ekko-ernam.
    ENDIF.

    IF lv_zusuario IS NOT INITIAL.

      " Se recupera el mail del usuario
      TRY.
          DATA(lo_user) = NEW zcl_apd_user( iv_user = lv_zusuario ).
          lo_user->get_user_details( IMPORTING ev_email = DATA(lv_mail)
                                     EXCEPTIONS OTHERS = 99 ).
        CATCH cx_root.
      ENDTRY.

      " Si no tiene mail no se hará el envio pero tampoco se lanzará error. El motivo
      " es que el usuario puede no existir en el sistema, por el motivo que sea, por ello
      " no debe ser impedimento que no se puede rechazar el pedido
      IF lv_mail IS NOT INITIAL.

        DATA(lt_destinatarios) = VALUE zzz_t_destinataris_mail( ( tipo_usuario = 'U' envio_express = abap_true ad_smtpadr = lv_mail ) ).

      ENDIF.

      " Asunto
      DATA(lv_subject) = CONV so_obj_des( zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                                   i_id         = zif_apd_data=>cv_msg_id
                                                   i_number     = '010'
                                                   i_message_v1 = |{ is_ekko-ebeln ALPHA = OUT }|
                                                   i_langu      = mv_langu )-message ).

      " Cuerpo
      DATA(lt_body) = cl_bcs_convert=>string_to_soli( iv_reason ).

      " Envio de mail
      CALL FUNCTION 'ZZZ_F_ENVIAR_MAIL'
        EXPORTING
          pe_asunto        = lv_subject
          te_cos_mail      = lt_body[]
          te_destinatarios = lt_destinatarios[].

    ENDIF.

  ENDMETHOD.


  METHOD static_rel_action_from_bapi.
    DATA lt_users_notif_process TYPE zif_apd_app=>tt_user_notif_process.
    DATA lt_r_excep_tcode TYPE RANGE OF sytcode.

    " Solo se realizará el proceso cuando se llame vía determinadas BAPIs o transacciones que se comportan como las BAPIs
    IF call_rel_action_from_bapi( ).

      " Instancio la clase del proceso
      DATA(lo_app_po) = NEW zcl_apd_app_po( iv_app = zcl_apd_app_po=>cv_id_app ).

      DATA(lo_handler) = lo_app_po->get_handler_header_from_po( iv_ebeln ).

      IF lo_handler IS BOUND.

        lo_app_po->get_release_info_from_handler( EXPORTING io_handler = lo_handler
                                                  IMPORTING es_header_data = DATA(ls_header)
                                                            et_release_codes = DATA(lt_release_codes)
                                                            et_steps = DATA(lt_steps) ).

        " El problema que hay en la BAPI es que los datos aun no están actualizados por lo tanto tengo que descartar
        " lo que ha sido ya aprobado.
        DELETE lt_steps WHERE approved = abap_true.
        IF lt_steps IS NOT INITIAL. " No debería de ocurrir pero por si acaso.

          " El paso que se esta aprobando el código del primero registro que hay en la tabla de pasos.
          DATA(lv_rel_code_approved) = lt_steps[ 1 ]-rel_code.
          lt_users_notif_process = VALUE #( FOR <wa> IN lt_steps WHERE ( rel_code = lv_rel_code_approved  ) ( username = <wa>-username approved = abap_true  ) ).

          " Los siguiente en aprobar el siguiente codigo de liberación al actual.
          DATA(lv_rel_code_pending) = VALUE frgco(  ).
          READ TABLE lt_release_codes TRANSPORTING NO FIELDS WITH KEY rel_code = lv_rel_code_approved.
          IF sy-subrc = 0.
            DATA(lv_tabix) = sy-tabix + 1.
            " Controlo que no lea un indice que no exista.
            IF lv_tabix <= lines( lt_release_codes  ).
              lv_rel_code_pending = lt_release_codes[ lv_tabix ]-rel_code.
              lt_users_notif_process = VALUE #( BASE lt_users_notif_process
                                            FOR <wa> IN lt_steps WHERE ( rel_code = lv_rel_code_pending  ) ( username = <wa>-username pending = abap_true  ) ).
            ENDIF.

          ENDIF.

          " Si tengo a quien enviar la notificación, lanzo el proceso de envio
          IF lt_users_notif_process IS NOT INITIAL.

            lo_app_po->zif_apd_app~send_notification(
              EXPORTING
                iv_num_doc      = CONV #( ls_header-ebeln )
                iv_doc_rejected = abap_false
                it_users        = lt_users_notif_process
              IMPORTING
                et_return       = DATA(lt_return) ).

          ENDIF.

        ENDIF.
      ENDIF.

      " Hay ciertas casuísticas que no es posible controlar en la BADI o que va a complicar el código de tal manera que va hacer
      " dificil el mantenimiento. Ejemplos:
      " 1) Que cambien datos del pedido que haga que los aprobadores ya no sean los mismos. Haciendo
      " casi imposible saber quien tenia la aprobación y quien no.
      " 2) Borrado de todas las posiciones haciendo que no sea necesario la aprobación
      " En esos casos lo que se va hacer es lanzar una actualización masiva de badge para todos los usuarios
      " excepto lo que eviará notificación.
      "EXPORTING it_exclude_user = VALUE #( FOR <wa2> IN lt_users_notif_process ( <wa2>-username ) )
*    zcl_apd_apps=>send_massive_notif( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_apd_app~approve_document.
    DATA lt_return TYPE STANDARD TABLE OF bapireturn WITH EMPTY KEY.

    read_ekko( EXPORTING it_r_ebeln = VALUE #( FOR <wa> IN it_numdoc ( sign = 'I' option = 'EQ' low = |{ <wa> ALPHA = IN }| ) )
                              IMPORTING et_ekko = DATA(lt_ekko) ).

    IF lt_ekko IS NOT INITIAL.
      DATA(lv_errors_po) = abap_false.
      DATA(lv_ok_po) = 0.
      DATA(lv_num_po) = lines( lt_ekko ).
      LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).

        DATA(lo_handler) = get_handler_header_from_po( <ls_ekko>-ebeln ).

        IF lo_handler IS BOUND.

          get_release_info_from_handler( EXPORTING io_handler = lo_handler
                                         IMPORTING es_header_data = DATA(ls_header)
                                                   et_release_codes = DATA(lt_release_codes)
                                                   et_steps = DATA(lt_steps) ).

          " Quitamos todos los pasos ya aprobados. De esta manera tendremos en la primera posición el codigo de liberación
          " que toca para el pedido.
          DELETE lt_steps WHERE approved = abap_true.
          IF lt_steps IS NOT INITIAL.

            CALL FUNCTION 'BAPI_PO_RELEASE'
              EXPORTING
                purchaseorder          = <ls_ekko>-ebeln
                po_rel_code            = lt_steps[ 1 ]-rel_code
                use_exceptions         = abap_true
              EXCEPTIONS
                authority_check_fail   = 1
                document_not_found     = 2
                enqueue_fail           = 3
                prerequisite_fail      = 4
                release_already_posted = 5
                responsibility_fail    = 6
                error_message          = 7
                OTHERS                 = 8.
            IF sy-subrc = 0.
              lv_ok_po = lv_ok_po + 1.
*          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
*                                         i_id         = zif_apd_data=>cv_msg_id
*                                         i_number     = '007'
*                                         i_message_v1 = |{ <ls_ekko>-ebeln ALPHA = OUT }|
*                                         i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
*          <ls_return_out>-parameter = <ls_ekko>-ebeln.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.

            ELSE.
              " Si se un error de bloqueo el mensaje se añade directamente pero no se marcará que hay errores en la creación para no
              " generar mensajes duplicados
              IF sy-subrc = 3.
                INSERT zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_error
                                                       i_id         = sy-msgid
                                                       i_number     = sy-msgno
                                                       i_message_v1 = sy-msgv1
                                                       i_message_v2 = sy-msgv2
                                                       i_message_v3 = sy-msgv3
                                                       i_message_v4 = sy-msgv4
                                                       i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
                <ls_return_out>-parameter = <ls_ekko>-ebeln.

              ELSE.
                lv_errors_po = abap_true.
              ENDIF.

              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

            ENDIF.
          ELSE.
            " Error porque el pedido esta 100% aprobado
            lv_errors_po = abap_true.
          ENDIF.
        ELSE.
          lv_errors_po = abap_true.
        ENDIF.
      ENDLOOP.

      IF lv_ok_po = lv_num_po. " Si todos los pedidos han sido aprobados entonces se saca un mensaje generico

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                         i_id         = zif_apd_data=>cv_msg_id
                                         i_number     = '017'
                                         i_langu      = mv_langu ) INTO TABLE et_return.
      ELSE. " Se han producido errores.
        " A petición de Maite no se saca el mensaje que va todo bien cuando hay errores junto a pedidos aprobados.
*        " Si hay algún pedido aprobado y no hay errores genericos (significa que solo hay errores de blqoueo)
*        " entonces saco el mismo mensaje que cuando va todo bien.
*        IF lv_ok_po > 0 AND lv_errors_po = abap_false.
*          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
*                                           i_id         = zif_apd_data=>cv_msg_id
*                                           i_number     = '017'
*                                           i_langu      = mv_langu ) INTO TABLE et_return.
*        ENDIF.

        " Si hay errores que no son bloqueo entonces añado un mensaje generico.
        IF lv_errors_po = abap_true.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_error
                                              i_id         = zif_apd_data=>cv_msg_id
                                              i_number     = '018'
                                              i_langu      = mv_langu ) INTO TABLE et_return.
        ENDIF.
      ENDIF.

      " Lanzamos coche escoba para las notificaciones
      send_massive_notif( ).
    ELSE.
      " La excepcion varia si viene uno o varios documentos
      IF lines( it_numdoc ) > 1.
        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid = zcx_apd=>no_orders_found.
      ELSE.
        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid   = zcx_apd=>order_not_exist
            mv_msgv1 = |{ it_numdoc[ 1 ] }|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_apd_app~get_apps_kpi.


    CLEAR: rv_kpi.

    " Leo los datos de la ekko para saber lo que tiene pendiente el usuario
    read_ekko( IMPORTING et_ekko = DATA(lt_ekko) ).


    rv_kpi = lines( lt_ekko ).

  ENDMETHOD.


  METHOD zif_apd_app~get_app_info.
    CLEAR: et_info.

    " Primero busco las estrategias de liberación del propio usuario y de la gente que le ha
    " asignado como backups
    get_release_codes( IMPORTING et_release_codes = DATA(lt_release_codes) ).

    " Si se han encontrado codigos de liberación es que el usuario puede aprobar por lo tanto
    " busco la info de la aplicación
    IF lt_release_codes IS NOT INITIAL.

      read_app_info( EXPORTING it_r_app = VALUE #( ( sign = 'I' option = 'EQ' low = cv_id_app ) )
                     IMPORTING et_info = et_info ).

    ENDIF.
  ENDMETHOD.


  METHOD zif_apd_app~get_backups.

    CLEAR: er_backups.

    DATA(lv_usuario)  = COND syuname( WHEN iv_user IS NOT INITIAL THEN iv_user ELSE mv_user ).

    " Recuperamos el usuario del cual es sustituto
    SELECT user_po
      FROM zapd_t005
      INTO TABLE @DATA(lt_backups)
      WHERE replace_user  EQ @lv_usuario
        AND date_ini LE @sy-datum
        AND date_fin GE @sy-datum.

    er_backups      = VALUE #( FOR <ls_backups> IN lt_backups ( sign = 'I' option = 'EQ' low = <ls_backups>-user_po  ) )   .

  ENDMETHOD.


  METHOD zif_apd_app~get_detail.

    " Obtenemos la información de cabecera del pedido. Que se necesita para devolver parte de dicha información
    get_purchase_header_info( EXPORTING it_r_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = |{ iv_numdoc ALPHA = IN }| ) )
                              IMPORTING et_data = DATA(lt_header_data) ).

    IF lt_header_data IS NOT INITIAL.

      " Se obtiene la información del detalle del pedido
      get_purchase_detail_info( EXPORTING is_header_data = lt_header_data[ 1 ]
                                IMPORTING et_data        = DATA(lt_data) ).

      " Obtengo el tipo de documento para las distintas llamadas que se hace
      DATA(lv_doc_type) = COND #( WHEN is_po_inversion( lt_data[ 1 ]-knttp )
                                  THEN zif_apd_data=>cs_app-po_order-doc_type-inversion
                                  ELSE COND #( WHEN is_po_service( lt_header_data[ 1 ]-bsart )
                                               THEN zif_apd_data=>cs_app-po_order-doc_type-services
                                               ELSE zif_apd_data=>cs_app-po_order-doc_type-material ) ).

      " Obtenemos el catalogo de campo dependiendo del tipo de pedido
      get_view_fcat( EXPORTING iv_level    = zif_apd_data=>cs_app-level-detail
                                iv_doc_type = lv_doc_type
                      IMPORTING et_fcat     = DATA(lt_fcat) ).

      " Convierte el catalogo de campos al formato de salida
      convert_fcat_2_output( EXPORTING it_fcat = lt_fcat
                             IMPORTING et_fcat_output = es_list-catalog ).

      " Los datos de que se devuelven vienen de dos fuentes separadas. 1) Cabecera del pedido 2) Posición del pedido
      " Cabecera del pedido. Los datos de cabecera solo quedamos de los campos que están marcados que se quieren en la cabecera
      convert_itab_2_row_data( EXPORTING it_data = lt_header_data
                                         it_fcat = VALUE #( FOR <wa> IN lt_fcat WHERE ( show_header = abap_true ) ( <wa> ) )
                               IMPORTING et_row_data = DATA(lt_row_header) ).
      " Solo habrá un registro, porque solo una pedido, que pondremos el row a 0 para que identificar
      " que es de cabecera
      lt_row_header[ 1 ]-row = 0.
      INSERT LINES OF lt_row_header INTO TABLE es_list-data.

      " Ahora los datos de las posición
      convert_itab_2_row_data( EXPORTING it_data = lt_data
                                         it_fcat = VALUE #( FOR <wa> IN lt_fcat WHERE ( show_header = abap_false ) ( <wa> ) )
                                IMPORTING et_row_data = DATA(lt_row_pos) ).
      INSERT LINES OF lt_row_pos INTO TABLE es_list-data.


      " Acciones
      get_actions( EXPORTING iv_level = zif_apd_data=>cs_app-level-detail
                             iv_doc_type = lv_doc_type
                   IMPORTING et_actions = es_list-actions ).

    ELSE.
      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid   = zcx_apd=>order_not_exist
          mv_msgv1 = |{ iv_numdoc }|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_apd_app~get_list.

    CLEAR: es_list.

    " Datos de la aplicación
    read_app_info( EXPORTING it_r_app = VALUE #( ( sign = 'I' option = 'EQ' low = cv_id_app ) )
                     IMPORTING et_info = DATA(lt_info) ).

    " Primero vamos a obtener la información de los pedidos
    get_purchase_header_info( IMPORTING et_data = DATA(lt_header_data) ).

    READ TABLE lt_info ASSIGNING FIELD-SYMBOL(<ls_info>) INDEX 1.
    es_list-title = <ls_info>-title.

    " Se obtiene el catalogo de campos
    get_view_fcat( EXPORTING iv_level    = zif_apd_data=>cs_app-level-header
                             iv_doc_type = space
                   IMPORTING et_fcat     = DATA(lt_fcat) ).

    " Se convierten los datos al formato de salida
    convert_itab_2_row_data( EXPORTING it_data = lt_header_data
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
    CLEAR: er_backups.

    DATA(lv_usuario)  = COND syuname( WHEN iv_user IS NOT INITIAL THEN iv_user ELSE mv_user ).

    " Recuperamos los usuarios que reemplazan al usuario pasado por parámemtro
    SELECT replace_user
      FROM zapd_t005
      INTO TABLE @DATA(lt_backups)
      WHERE user_po  EQ @lv_usuario
        AND date_ini LE @sy-datum
        AND date_fin GE @sy-datum.

    er_backups      = VALUE #( FOR <ls_backups> IN lt_backups ( sign = 'I' option = 'EQ' low = <ls_backups>-replace_user  ) )   .
  ENDMETHOD.


  METHOD zif_apd_app~get_pdf.
    DATA lt_r_kschl TYPE RANGE OF nast-kschl.
    DATA lv_content TYPE xstring.
    DATA lv_content_len TYPE i.
    DATA lv_mimetype TYPE string.

    read_ekko( EXPORTING it_r_ebeln = VALUE #( ( sign = 'I' option = 'EQ' low = |{ iv_numdoc ALPHA = IN }| ) )
                              IMPORTING et_ekko = DATA(lt_ekko) ).

    IF lt_ekko IS NOT INITIAL.
      ASSIGN lt_ekko[ 1 ] TO FIELD-SYMBOL(<ls_ekko>).

      " Me guardo si es de servicio que lo tendré que usar en vario sitios.
      DATA(lv_is_po_service) = is_po_service( <ls_ekko>-bsart ).

      " Ahora hay saber si el pedido es de servicio o material para saber a que clases de mensaje hay que procesar.
      IF lv_is_po_service = abap_true.
        zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'KSCHL_PO_SERVICIOS_%'
                                                         CHANGING t_ranges = lt_r_kschl ).
      ELSE.
        zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'KSCHL_PO_MATERIALES_%'
                                                         CHANGING t_ranges = lt_r_kschl ).
      ENDIF.

      " Se mira si para el pedido hay alguna clase de mensaje generada
      SELECT kschl INTO @DATA(lv_kschl) UP TO 1 ROWS
             FROM nast
             WHERE objky = @<ls_ekko>-ebeln
                   AND kschl IN @lt_r_kschl
                   ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.

        " La previsualización indico por defecto que se previsualice para pedidos de material. El motivo es que dentro de las rutinas de impresion
        " llama a la funcion estándar de obtención de datos para el smartform, que da un error si el pedido no esta liberado y queremos previsualizar.
        " Por ello, hay que indicarle por defecto que queremos previsulizar y dentro de la rutina de impresion se cambia luego a que devuelva el otf si
        " proviene de la aplicacion de aprobación.
        " Para pedidos de servicios no es necesario porque usa sapscript y no hay tanto follon.
        CALL FUNCTION 'ZAPD_PO_ORDER_FORM'
          EXPORTING
            iv_kschl      = lv_kschl
            iv_ebeln      = <ls_ekko>-ebeln
            iv_langu      = mv_langu
            iv_ent_screen = COND #( WHEN lv_is_po_service = abap_true THEN abap_false ELSE abap_true )
          IMPORTING
            ev_content    = lv_content
            ev_numbytes   = lv_content_len.

        IF lv_content IS NOT INITIAL.

          zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'PDF_MIMETYPE'
                                                IMPORTING c_valor     = lv_mimetype ).

          " Se obtiene la URL a partir del binario.
          get_url_from_binary_content( EXPORTING iv_extension = 'pdf'
                                                 iv_mimetype =  lv_mimetype
                                                 iv_content = lv_content
                                                 iv_numbytes = lv_content_len
                                                 iv_numdoc = iv_numdoc
                                       IMPORTING ev_url = ev_url ).

        ELSE.
          RAISE EXCEPTION TYPE zcx_apd
            EXPORTING
              textid   = zcx_apd=>generate_po_pdf_not_possible
              mv_msgv1 = |{ iv_numdoc }|.
        ENDIF.

      ELSE.
        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid   = zcx_apd=>generate_po_pdf_not_possible
            mv_msgv1 = |{ iv_numdoc }|.
      ENDIF.

    ELSE.
      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid   = zcx_apd=>order_not_exist
          mv_msgv1 = |{ iv_numdoc }|.
    ENDIF.

  ENDMETHOD.


  METHOD zif_apd_app~reject_document.
    DATA lt_r_bsart TYPE RANGE OF ekko-bsart.
    DATA lv_procstat TYPE ekko-procstat.
    DATA lv_reset TYPE sap_bool.

    read_ekko( EXPORTING it_r_ebeln = VALUE #( FOR <wa> IN it_numdoc ( sign = 'I' option = 'EQ' low = |{ <wa> ALPHA = IN }| ) )
               IMPORTING et_ekko = DATA(lt_ekko) ).

    IF lt_ekko IS NOT INITIAL.

      DATA(lv_num_po) = lines( lt_ekko ).
      DATA(lv_ok_po) = 0.
      DATA(lv_errors_po) = abap_false.

      LOOP AT lt_ekko ASSIGNING FIELD-SYMBOL(<ls_ekko>).
        DATA(lv_pedido_rechazado) = abap_false.

        " Nota: El método de rechazo se hace lo mismo que esta en el programa ZMML059, lamentablemente no se puede llamar
        " a sus procedimientos para poderlo hacer.

        " La forma de rechazar depende del tipo de documento.
        zcl_apd_constants=>obtener_constantes_en_ranges( EXPORTING i_patron = 'BSART_REJECT_UPDATE_%'
                                                         CHANGING t_ranges = lt_r_bsart ).
        IF <ls_ekko>-bsart IN lt_r_bsart AND lt_r_bsart IS NOT INITIAL.
          zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'REJECT_PROCSTAT'
                                                IMPORTING c_valor     = lv_procstat ).

          UPDATE ekko SET procstat = lv_procstat WHERE ebeln EQ <ls_ekko>-ebeln.

          " Se envia el mail del rechazo. El método no devuelve errores ni nada. Porque solo podría fallar si el usuario
          " al que se envia el mail no existe. Pero eso no debe impedir que pueda rechazar.
          send_mail_reject( EXPORTING is_ekko = <ls_ekko>
                                      iv_reason = iv_reason
                            IMPORTING et_return = DATA(lt_return) ).


          lv_pedido_rechazado = abap_true.

          COMMIT WORK AND WAIT.


        ELSE.
          " El resto de clases de documento
          DATA(lo_order) = NEW cl_po_header_handle_mm( im_po_number = <ls_ekko>-ebeln ).

          TRY.
              lo_order->po_initialize( ).
              lo_order->po_read( EXPORTING im_tcode     = 'ME29N'
                                           im_trtyp     = 'V'
                                           im_aktyp     = 'V'
                                           im_po_number = <ls_ekko>-ebeln
                                 IMPORTING ex_result = DATA(lv_result) ).


              DATA(lo_release_order) = CAST if_releasable_mm( lo_order ).


              IF lo_release_order IS BOUND.

                " Se envia el mail del rechazo. El método no devuelve errores ni nada. Porque solo podría fallar si el usuario
                " al que se envia el mail no existe. Pero eso no debe impedir que pueda rechazar.
                send_mail_reject( EXPORTING is_ekko = <ls_ekko>
                                            iv_reason = iv_reason ).

                CALL METHOD lo_release_order->reject
                  EXPORTING
                    im_reset = lv_reset
                  EXCEPTIONS
                    failed   = 1.

                IF sy-subrc NE 0. " Si falla devuelvo el mensaje de error
                  " Se quita porque los mensajes son grupales.
                  INSERT zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_error
                                                         i_id         = sy-msgid
                                                         i_number     = sy-msgno
                                                         i_message_v1 = sy-msgv1
                                                         i_message_v2 = sy-msgv2
                                                         i_message_v3 = sy-msgv3
                                                         i_message_v4 = sy-msgv4
                                                         i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING FIELD-SYMBOL(<ls_return_out>).
                  <ls_return_out>-parameter = <ls_ekko>-ebeln.

                ELSEIF lv_reset = abap_false.
                  " Se graba el pedido
                  lo_order->po_post( EXCEPTIONS failure = 1 ).
                  IF sy-subrc = 0.

                    lv_pedido_rechazado = abap_true.
                    COMMIT WORK AND WAIT.

                  ELSE.

                    INSERT zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_error
                                                           i_id         = sy-msgid
                                                           i_number     = sy-msgno
                                                           i_message_v1 = sy-msgv1
                                                           i_message_v2 = sy-msgv2
                                                           i_message_v3 = sy-msgv3
                                                           i_message_v4 = sy-msgv4
                                                           i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                    <ls_return_out>-parameter = <ls_ekko>-ebeln.

                    ROLLBACK WORK.
                  ENDIF.
                ELSE.
                  lv_pedido_rechazado = abap_true.
*                  " Si se resetea se indico el mensaje que sale en el programa en vez del generico que tengo preparado
                  INSERT zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_success
                                                          i_id         = 'MEPO'
                                                          i_number     = '818'
                                                          i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                  <ls_return_out>-parameter = <ls_ekko>-ebeln.
                ENDIF.


              ELSE.
                RAISE EXCEPTION TYPE zcx_apd
                  EXPORTING
                    textid   = zcx_apd=>error_reject_po_order
                    mv_msgv1 = |{ <ls_ekko>-ebeln }|.
              ENDIF.

              " Si el pedido se rechaza incremento el número de pedidos ok
              IF lv_pedido_rechazado = abap_true.
                lv_ok_po = lv_ok_po + 1.
                INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                       i_id         = zif_apd_data=>cv_msg_id
                                       i_number     = '011'
                                       i_message_v1 = |{ <ls_ekko>-ebeln ALPHA = OUT }|
                                       i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                <ls_return_out>-parameter = <ls_ekko>-ebeln.
              ELSE.
                " Si no se ha rechazado es que ha habido algun error.
                lv_errors_po = abap_true.
              ENDIF.

            CATCH cx_root.
              IF sy-msgid = 'ME' AND sy-msgno = '006'.
                INSERT zcl_ca_utilidades=>fill_return( i_type = zif_apd_data=>cv_msg_type_error
                                                            i_id         = sy-msgid
                                                            i_number     = sy-msgno
                                                            i_message_v1 = sy-msgv1
                                                            i_message_v2 = sy-msgv2
                                                            i_message_v3 = sy-msgv3
                                                            i_message_v4 = sy-msgv4
                                                            i_langu      = mv_langu ) INTO TABLE et_return ASSIGNING <ls_return_out>.
                <ls_return_out>-parameter = <ls_ekko>-ebeln.
              ENDIF.
          ENDTRY.
        ENDIF.

      ENDLOOP.


      " Lanzamos coche escoba para las notificaciones
      send_massive_notif( ).

    ELSE.
      " La excepcion varia si viene uno o varios documentos
      IF lines( it_numdoc ) > 1.
        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid = zcx_apd=>no_orders_found.
      ELSE.
        RAISE EXCEPTION TYPE zcx_apd
          EXPORTING
            textid   = zcx_apd=>order_not_exist
            mv_msgv1 = |{ it_numdoc[ 1 ] }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD zif_apd_app~send_notification.

    DATA lt_notif TYPE tt_user_notifications.

    " Monto una nueva tabla con los backups
    LOOP AT it_users ASSIGNING FIELD-SYMBOL(<ls_users>) WHERE username IS NOT INITIAL.

      NEW zcl_apd_user( iv_user = <ls_users>-username )->get_user_details( IMPORTING ev_user_langu = DATA(lv_langu) ).
      " Buscamos los backups en ambos sentidos.
      " Si aprueba un suplente obtengo quien el aprobador principal
      DATA(lt_r_backups) = zif_apd_app~get_my_backups( iv_user = <ls_users>-username ).
      " Si aprueba el titular me devuelve sus suplentes.
      DATA(lt_r_backups2) = zif_apd_app~get_backups( iv_user = <ls_users>-username ).
      INSERT LINES OF lt_r_backups2 INTO TABLE lt_r_backups.
      SORT lt_r_backups.
      DELETE ADJACENT DUPLICATES FROM lt_r_backups COMPARING ALL FIELDS.


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
                      badge = REDUCE #( INIT x = 0 FOR <wa> IN lt_kpis  NEXT x = x + <wa>-kpi )
                      message = lv_message ) INTO TABLE lt_notif ASSIGNING FIELD-SYMBOL(<ls_notif>).

      " NOTA IRB: Es un guarrada pero es lo más simple. Realmente hay uno menos porque el pedido que estoy aprobando
      " esta dentrol numero total, pero como todavía no se ha realizado el commit saldrá.
      IF <ls_users>-approved = abap_true.
        <ls_notif>-badge = <ls_notif>-badge - 1.
      ELSEIF <ls_users>-pending = abap_true.
        <ls_notif>-badge = <ls_notif>-badge + 1.
      ENDIF.



      " Añado los backups a las notificaciones
      LOOP AT lt_r_backups ASSIGNING FIELD-SYMBOL(<ls_r_backups>).
        " Si el subsituto ya esta en las notificaciones no lo añado
        READ TABLE lt_notif TRANSPORTING NO FIELDS WITH KEY username = <ls_r_backups>-low.
        IF sy-subrc NE 0.

          NEW zcl_apd_user( iv_user = <ls_r_backups>-low )->get_user_details( IMPORTING ev_user_langu = lv_langu ).

          lv_message = zcl_ca_utilidades=>fill_return( i_type       = zif_apd_data=>cv_msg_type_success
                                                       i_id         = zif_apd_data=>cv_msg_id
                                                       i_number     = '021'
                                                       i_langu      = lv_langu )-message.

          lt_kpis = zcl_apd_apps=>get_kpi_all_apps( iv_user = <ls_r_backups>-low ).
          INSERT VALUE #( username = <ls_r_backups>-low
                          badge = REDUCE #( INIT x = 0 FOR <wa> IN lt_kpis NEXT x = x + <wa>-kpi )
                          message = lv_message ) INTO TABLE lt_notif ASSIGNING <ls_notif>.

          " NOTA IRB: Es un guarrada pero es lo más simple. Realmente hay uno menos porque el pedido que estoy aprobando
          " esta dentrol numero total, pero como todavía no se ha realizado el commit saldrá.
          IF <ls_users>-approved = abap_true.
            <ls_notif>-badge = <ls_notif>-badge - 1.
          ELSEIF <ls_users>-pending = abap_true.
            <ls_notif>-badge = <ls_notif>-badge + 1.
          ENDIF.

        ENDIF.
      ENDLOOP.


    ENDLOOP.

    " Envio de la notificación.
    send_notification( EXPORTING it_notificacion = lt_notif
                       IMPORTING et_return = et_return ).

  ENDMETHOD.
ENDCLASS.
