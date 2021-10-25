CLASS zcl_apd_apps DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES zif_apd_app .

    TYPES:
      BEGIN OF ts_apps,
        app   TYPE zapd_e_app,
        title TYPE zapd_e_desc_app,
        icon  TYPE zapd_e_icono,
        orden TYPE zapd_e_orden,
      END OF ts_apps .
    TYPES:
      tt_apps TYPE STANDARD TABLE OF ts_apps .
    TYPES:
      BEGIN OF ts_user_notifications,
        username TYPE syuname,
        message  TYPE string,
        badge    TYPE i,
      END OF ts_user_notifications .
    TYPES:
      tt_user_notifications TYPE STANDARD TABLE OF ts_user_notifications WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ts_kpi_all_apps,
        app TYPE zapd_e_app,
        kpi TYPE numc5,
      END OF ts_kpi_all_apps .
    TYPES:
      tt_kpi_all_aps TYPE STANDARD TABLE OF ts_kpi_all_apps WITH DEFAULT KEY .
    TYPES:
      tt_users_excluded TYPE STANDARD TABLE OF syuname WITH EMPTY KEY .

    METHODS constructor
      IMPORTING
        !iv_langu TYPE sy-langu DEFAULT sy-langu
        !iv_app   TYPE zapd_e_app OPTIONAL
        !iv_user  TYPE syuname DEFAULT sy-uname .
    CLASS-METHODS get_instance
      IMPORTING
        !iv_app   TYPE zapd_e_app
        !iv_langu TYPE sylangu
        !iv_user  TYPE syuname
      EXPORTING
        !eo_app   TYPE REF TO zif_apd_app
      RAISING
        zcx_apd .
    CLASS-METHODS get_class_controler
      IMPORTING
        !iv_app             TYPE zapd_e_app
      RETURNING
        VALUE(rv_classname) TYPE zapd_e_clase_control
      RAISING
        zcx_apd .
    CLASS-METHODS get_apps
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu
        !iv_user  TYPE syuname DEFAULT sy-uname
      EXPORTING
        !et_apps  TYPE tt_apps
      RAISING
        zcx_apd .
    "! <p class="shorttext synchronized">Devuelve si se llama desde ZAPD</p>
    "! Se usa para en la generación de formularios saber si se llama desde ZAPD
    "! para poderse guardar el OTF y devolvelo como URL
    "! @parameter rv_is | <p class="shorttext synchronized">Viene de ZAPD</p>
    CLASS-METHODS call_from_zapd
      RETURNING
        VALUE(rv_is) TYPE sap_bool .
    "! <p class="shorttext synchronized">Devuelve los KPIs de todas las apps</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter rt_kpis | <p class="shorttext synchronized">KPIs de las apps</p>
    CLASS-METHODS get_kpi_all_apps
      IMPORTING
        !iv_user       TYPE syuname DEFAULT sy-uname
        !iv_langu      TYPE sylangu DEFAULT sy-langu
      RETURNING
        VALUE(rt_kpis) TYPE tt_kpi_all_aps .
    "! <p class="shorttext synchronized">Envio masivo de notificaciones</p>
    "! @parameter iv_user_process | <p class="shorttext synchronized">Usuarios del proceso</p>
    "! @parameter IV_INMEDIATLY | <p class="shorttext synchronized">Envio inmediato?</p>
    CLASS-METHODS send_massive_notif
      IMPORTING
        !iv_user_process TYPE syuname DEFAULT sy-uname
        !iv_inmediatly   TYPE sap_bool DEFAULT abap_true.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ts_view_fcat.
        INCLUDE TYPE zcl_ca_fieldcatalog=>ts_view_fcat.
        INCLUDE TYPE zapd_s001.
    TYPES:
       END OF ts_view_fcat .
    TYPES:
      tt_view_fcat TYPE STANDARD TABLE OF ts_view_fcat WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_actions,
        action      TYPE zapd_t004-accion,
        label       TYPE zapd_t004t-descripcion,
        icon        TYPE zapd_t004-icon,
        position    TYPE zapd_t004-pos,
        action_type TYPE zapd_t004-act_type,
        is_massive  TYPE zapd_t004-massive,
      END OF ts_actions .
    TYPES:
      tt_actions TYPE STANDARD TABLE OF ts_actions WITH EMPTY KEY .

    DATA mv_langu TYPE sy-langu .
    DATA mv_app TYPE zapd_e_app .
    DATA mv_user TYPE syuname .
    DATA mo_fcat TYPE REF TO zcl_ca_fieldcatalog .

    "! <p class="shorttext synchronized">Lee los datos de las aplicación por parámetros</p>
    "! @parameter it_r_app | <p class="shorttext synchronized">Aplicaciones</p>
    "! @parameter et_info | <p class="shorttext synchronized">Información de la aplicación</p>
    METHODS read_app_info
      IMPORTING
        !it_r_app TYPE zif_apd_data=>tr_app_aprov
      EXPORTING
        !et_info  TYPE zif_apd_app=>tt_app_info .
    "! <p class="shorttext synchronized">Devuelve la vista de campo</p>
    "! @parameter iv_level | <p class="shorttext synchronized">Nivel</p>
    "! @parameter iv_doc_type | <p class="shorttext synchronized">Tipo de documento</p>
    "! @parameter rv_view | <p class="shorttext synchronized">ID de vista</p>
    METHODS get_id_view_fields
      IMPORTING
        !iv_level      TYPE zapd_e_level
        !iv_doc_type   TYPE zapd_e_doc_type
      RETURNING
        VALUE(rv_view) TYPE zca_e_id_view_fields .
    "! <p class="shorttext synchronized">Devuelve el catalogo de campos</p>
    "! @parameter iv_level | <p class="shorttext synchronized">Nivel</p>
    "! @parameter iv_doc_type | <p class="shorttext synchronized">Tipo de documento</p>
    "! @parameter et_fcat | <p class="shorttext synchronized">Catalogo de campos</p>
    METHODS get_view_fcat
      IMPORTING
        !iv_level    TYPE zapd_e_level
        !iv_doc_type TYPE zapd_e_doc_type
      EXPORTING
        !et_fcat     TYPE tt_view_fcat .
    "! <p class="shorttext synchronized">Devuelve las acciones</p>
    "! @parameter iv_level | <p class="shorttext synchronized">Nivel</p>
    "! @parameter iv_doc_type | <p class="shorttext synchronized">Tipo de documento</p>
    "! @parameter et_actions | <p class="shorttext synchronized">Acciones</p>
    METHODS get_actions
      IMPORTING
        !iv_level    TYPE zapd_e_level
        !iv_doc_type TYPE zapd_e_doc_type
      EXPORTING
        !et_actions  TYPE tt_actions .
    "! <p class="shorttext synchronized">Convierte datos tabla interna a datos en fila/columna</p>
    "! @parameter it_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter it_fcat | <p class="shorttext synchronized">Catalogo de campos</p>
    "! @parameter et_row_data | <p class="shorttext synchronized">Fila de datos</p>
    METHODS convert_itab_2_row_data
      IMPORTING
        !it_data     TYPE STANDARD TABLE
        !it_fcat     TYPE tt_view_fcat
      EXPORTING
        !et_row_data TYPE zif_apd_app=>tt_value_row .
    "! <p class="shorttext synchronized">Convierte el catalogo de campos al formato de salida</p>
    "! @parameter it_fcat | <p class="shorttext synchronized">Catalogo de campos</p>
    "! @parameter et_fcat_output | <p class="shorttext synchronized">Catalogo de campos formato salida</p>
    METHODS convert_fcat_2_output
      IMPORTING
        !it_fcat        TYPE tt_view_fcat
      EXPORTING
        !et_fcat_output TYPE zif_apd_app=>tt_fieldcatalog .
    "! <p class="shorttext synchronized">Obtiene la URL a partir del contenido en binario</p>
    "! @parameter iv_content | <p class="shorttext synchronized">Contenido</p>
    "! @parameter iv_numbytes | <p class="shorttext synchronized">Tamaño</p>
    "! @parameter iv_numdoc | <p class="shorttext synchronized">Numero de documento</p>
    "! @parameter ev_url | <p class="shorttext synchronized">URL</p>
    METHODS get_url_from_binary_content
      IMPORTING
        !iv_extension TYPE sopcklsti1-doc_type
        !iv_mimetype  TYPE string
        !iv_content   TYPE xstring
        !iv_numbytes  TYPE i
        !iv_numdoc    TYPE string
      EXPORTING
        !ev_url       TYPE string
      RAISING
        zcx_apd .
    "! <p class="shorttext synchronized">Cambia la URL de local a publica</p>
    "! @parameter cv_url | <p class="shorttext synchronized">URL</p>
    METHODS change_url_internal_2_cloud
      CHANGING
        !cv_url TYPE string .
    "! <p class="shorttext synchronized">Convierte la estructura a datos en fila/columna</p>
    "! @parameter is_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter it_fcat | <p class="shorttext synchronized">Catalogo de campos</p>
    "! @parameter es_row_data | <p class="shorttext synchronized">Fila de datos</p>
    METHODS convert_struc_2_row_data
      IMPORTING
        !is_data     TYPE any
        !it_fcat     TYPE tt_view_fcat
      EXPORTING
        !es_row_data TYPE zif_apd_app=>ts_value_row .
    "! <p class="shorttext synchronized">Envio las notificaciones</p>
    "! @parameter it_notificacion | <p class="shorttext synchronized">Notificaciones</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS send_notification
      IMPORTING
        !it_notificacion TYPE tt_user_notifications
      EXPORTING
        !et_return       TYPE bapiret2_t.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_apd_apps IMPLEMENTATION.


  METHOD call_from_zapd.
    DATA lt_callstack TYPE abap_callstack.
    DATA lt_r_function TYPE RANGE OF string.

    rv_is = abap_false.

* Se recuperan las funciones de formularios para indiciar si viene de APD
    zcl_apd_constants=>obtener_constantes_en_ranges(  EXPORTING i_patron = 'FUNCTIONS_FORMS_%'
                                                       CHANGING t_ranges = lt_r_function ).


* Si no hay datos no continuamos
    IF lt_r_function IS NOT INITIAL.

      CALL FUNCTION 'SYSTEM_CALLSTACK'
        IMPORTING
          callstack = lt_callstack.

      LOOP AT lt_callstack TRANSPORTING NO FIELDS
                              WHERE blocktype = 'FUNCTION'
                                    AND  blockname IN lt_r_function.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        rv_is = abap_true.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD change_url_internal_2_cloud.
    DATA lv_url_cloud TYPE string.

    zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'URL_CLOUD_R3'
                                          IMPORTING c_valor     = lv_url_cloud ).

    " Obtenemos la URL local
    DATA(lv_localhost) = zcl_ca_http=>get_host( ).
    IF lv_localhost IS NOT INITIAL.
      lv_localhost = |{ lv_localhost }/|.
    ENDIF.

    IF cv_url CS lv_localhost AND
       lv_url_cloud IS NOT INITIAL. "
      REPLACE FIRST OCCURRENCE OF lv_localhost IN cv_url WITH lv_url_cloud.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.
    mv_langu = iv_langu.
    mv_app = iv_app.
    mv_user = iv_user.

    " Clase que recupera cross que contiene la info de los campos para sacar la información
    mo_fcat = NEW zcl_ca_fieldcatalog( iv_appl = zif_apd_data=>cs_app-fcat_app
                                       iv_langu = mv_langu ).

  ENDMETHOD.


  METHOD convert_fcat_2_output.

    CLEAR et_fcat_output.

    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE sendui5 = abap_true.
      INSERT CORRESPONDING #( <ls_fcat> ) INTO TABLE et_fcat_output ASSIGNING FIELD-SYMBOL(<ls_fcat_output>).
      <ls_fcat_output>-desc = <ls_fcat>-col_text.
      <ls_fcat_output>-order = <ls_fcat>-pos.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_itab_2_row_data.


    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      DATA(lv_tabix) = sy-tabix.
      INSERT VALUE #( row = lv_tabix ) INTO TABLE et_row_data ASSIGNING FIELD-SYMBOL(<ls_row_data>).

      <ls_row_data>-row = lv_tabix.

      convert_struc_2_row_data( EXPORTING is_data = <ls_data>
                                          it_fcat = it_fcat
                                IMPORTING es_row_data = DATA(ls_row_data) ).

      <ls_row_data>-data = ls_row_data-data.

    ENDLOOP.

  ENDMETHOD.


  METHOD convert_struc_2_row_data.
    DATA lv_char20 TYPE c LENGTH 20.
    DATA lv_amount TYPE p LENGTH 16 DECIMALS 2.

    CLEAR: es_row_data.

    " Solo queremos los que se tienen que enviar a UI5
    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>) WHERE sendui5 = abap_true.
      CLEAR: lv_char20.

      ASSIGN COMPONENT <ls_fcat>-field_int OF STRUCTURE is_data TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.

        IF <field> IS NOT INITIAL. " Si hay valor se rellena, en caso contrario no se pasa para no añadir información demás.

          INSERT VALUE #( field = <ls_fcat>-field value = <field> ) INTO TABLE es_row_data-data ASSIGNING FIELD-SYMBOL(<ls_values>).

          " Si tiene conversion se le aplica al valor
          IF <ls_fcat>-conv_exit IS NOT INITIAL.
            mo_fcat->apply_convexit( EXPORTING iv_convexit  = <ls_fcat>-conv_exit
                                     CHANGING cv_value     = <ls_values>-value ).
          ELSEIF <ls_fcat>-field_ref IS NOT INITIAL. " Campos moneda cantidad
            " Busco el nombre interno del campo de referencia
            READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat_ref>) WITH KEY field = <ls_fcat>-field_ref.
            IF sy-subrc = 0.
              ASSIGN COMPONENT <ls_fcat_ref>-field_int OF STRUCTURE is_data TO FIELD-SYMBOL(<field_ref>).
              IF sy-subrc = 0.

                " Ahora añado el valor de referencia a los datos para que el frontend pueda leerlo
                INSERT VALUE #( field = <ls_fcat_ref>-field value = <field_ref> ) INTO TABLE es_row_data-data ASSIGNING FIELD-SYMBOL(<ls_value_ref>).

                " Si el campo de referncia tiene conversion exit le aplico el formato.
                IF <ls_fcat_ref>-conv_exit IS NOT INITIAL.
                  mo_fcat->apply_convexit( EXPORTING iv_convexit  = <ls_fcat_ref>-conv_exit
                                    CHANGING cv_value     = <ls_value_ref>-value ).
                ENDIF.

                " Si hay valor hago los formateos segun si es moneda o cantidad. Si no hay el valor del campo lo formateo sin unidad de medida.
                IF <field_ref> IS NOT INITIAL.
                  " El formateo depende si es importe o cantidad.
                  IF <ls_fcat>-datatype = zcl_ca_fieldcatalog=>cs_datatypes-curr.
                    " Paso el importe a un campo con formato moneda para transformarlo. Es curioso porque
                    " el campo <field> es importe pero no deja compilar porque no es capaz de determinar el tipo.
                    lv_amount = <field>.
                    lv_char20 = |{ lv_amount CURRENCY = <field_ref> SIGN = LEFT NUMBER = USER }|.
                  ELSE.
                    " Para el campo cantidad no existe la opción UNIT por lo tanto uso las sentencias antiguas.
                    WRITE <field> TO lv_char20 UNIT <field_ref> LEFT-JUSTIFIED.
                    " Para el signo lo hago a la antigua usanza. Solo si lo tiene.
                    IF lv_char20 CS '-'.
                      SHIFT lv_char20 UP TO '-' LEFT CIRCULAR.
                      CONDENSE lv_char20 NO-GAPS.
                    ENDIF.
                  ENDIF.
                  <ls_values>-value = lv_char20.

                ELSE.
                  WRITE <field> TO lv_char20 LEFT-JUSTIFIED.
                ENDIF.
                <ls_values>-value = lv_char20.
              ENDIF.
            ENDIF.
          ELSE.
            " Se formatea el valor que viene según el tipo de campo
            CASE <ls_fcat>-datatype.
              WHEN zcl_ca_fieldcatalog=>cs_datatypes-dats OR zcl_ca_fieldcatalog=>cs_datatypes-tims. " Fecha
                WRITE <field> TO lv_char20 LEFT-JUSTIFIED.
                <ls_values>-value = lv_char20.
              WHEN zcl_ca_fieldcatalog=>cs_datatypes-dec.
                WRITE <field> TO lv_char20 LEFT-JUSTIFIED DECIMALS <ls_fcat>-decimals.
                <ls_values>-value = lv_char20.
            ENDCASE.
          ENDIF.


          " Si el campo tiene asociado una descripción se le concatena al valor ya almacenado
          IF <ls_fcat>-field_desc IS NOT INITIAL.
            " Busco el nombre interno del campo de referencia
            READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat_desc>) WITH KEY field = <ls_fcat>-field_desc.
            IF sy-subrc = 0.
              ASSIGN COMPONENT <ls_fcat_desc>-field_int OF STRUCTURE is_data TO FIELD-SYMBOL(<field_desc>).
              IF sy-subrc = 0.
                IF <field_desc> IS NOT INITIAL.
                  <ls_values>-value = |{ <ls_values>-value } - { <field_desc> }|.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          " Si no hay valor hay que tener en cuenta ciertas excepciones
        ELSE.
          " Si tiene descripción asociada, se asociada directamente la descripción
          IF <ls_fcat>-field_desc IS NOT INITIAL.
            " En este caso se informa directamente la descripción
            READ TABLE it_fcat ASSIGNING <ls_fcat_desc> WITH KEY field = <ls_fcat>-field_desc.
            IF sy-subrc = 0.
              ASSIGN COMPONENT <ls_fcat_desc>-field_int OF STRUCTURE is_data TO <field_desc>.
              IF sy-subrc = 0.
                IF <field_desc> IS NOT INITIAL.
                  INSERT VALUE #( field = <ls_fcat>-field value = <field_desc> ) INTO TABLE es_row_data-data ASSIGNING <ls_values>.
                ENDIF.
              ENDIF.
            ENDIF.
            " Si se tiene que enviar aunque este en blanco se hace.
          ELSEIF <ls_fcat>-empty_field = abap_true.
            INSERT VALUE #( field = <ls_fcat>-field value = space ) INTO TABLE es_row_data-data ASSIGNING <ls_values>.
            " Hay que añadir el valor de su campo de referencia para garantizar la integridad de los datos
            IF <ls_fcat>-field_ref IS NOT INITIAL.
              " Busco el nombre interno del campo de referencia
              READ TABLE it_fcat ASSIGNING <ls_fcat_ref> WITH KEY field = <ls_fcat>-field_ref.
              IF sy-subrc = 0.
                ASSIGN COMPONENT <ls_fcat_ref>-field_int OF STRUCTURE is_data TO <field_ref>.
                IF sy-subrc = 0.
                  INSERT VALUE #( field = <ls_fcat_ref>-field value = <field_ref> ) INTO TABLE es_row_data-data ASSIGNING <ls_value_ref>.

                  " Si el campo de referncia tiene conversion exit le aplico el formato.
                  IF <ls_fcat_ref>-conv_exit IS NOT INITIAL.
                    mo_fcat->apply_convexit( EXPORTING iv_convexit  = <ls_fcat_ref>-conv_exit
                                      CHANGING cv_value     = <ls_value_ref>-value ).
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.


  METHOD get_actions.
    CLEAR: et_actions.

    SELECT a~accion AS action b~descripcion AS label a~icon a~pos AS position a~act_type AS action_type
           a~massive AS is_massive
           INTO TABLE et_actions
           FROM zapd_t004 AS a LEFT OUTER JOIN zapd_t004t AS b ON
                b~app = a~app
               AND b~applevel = a~applevel
               AND b~doc_type = a~doc_type
               AND b~accion = a~accion
               AND b~spras = mv_langu
           WHERE a~app = mv_app
                 AND a~applevel = iv_level
                 AND a~doc_type = iv_doc_type.
  ENDMETHOD.


  METHOD get_apps.
    DATA lo_app TYPE REF TO zif_apd_app.
    DATA lv_langu TYPE sylangu.

    CLEAR et_apps.

    IF iv_langu IS INITIAL.
      lv_langu = sy-langu.
    ELSE.
      lv_langu = iv_langu.
    ENDIF.

    SELECT clsname FROM vseoextend INTO TABLE @DATA(lt_classlist)
      WHERE refclsname = 'ZCL_APD_APPS'
      AND version = '1'.
    LOOP AT lt_classlist ASSIGNING FIELD-SYMBOL(<ls_classlist>).

      TRY.
          CREATE OBJECT lo_app TYPE (<ls_classlist>-clsname)
          EXPORTING
                iv_langu = lv_langu
                iv_user = iv_user.

          lo_app->get_app_info( IMPORTING et_info = DATA(lt_info) ).

          INSERT LINES OF lt_info INTO TABLE et_apps.
        CATCH cx_root.
      ENDTRY.
      CLEAR lt_info.
    ENDLOOP.

    IF et_apps IS INITIAL.
      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid = zcx_apd=>no_apps.
    ELSE.
      SORT et_apps BY orden.
    ENDIF.
  ENDMETHOD.


  METHOD get_class_controler.

    CLEAR rv_classname.

* Recuperamos la clase controladora de la pantalla
    SELECT SINGLE clase FROM zapd_t001 INTO rv_classname
     WHERE app = iv_app
      AND  activo = abap_true.

    IF ( sy-subrc NE 0 OR rv_classname IS INITIAL ).

*   Solo para la referencia de utilización del mensaje
      IF 1 EQ 2. MESSAGE i002(zapd) WITH iv_app. ENDIF.

*   La pantalla &1 no tiene clase controladora informada en la configuración
      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid   = zcx_apd=>app_without_handler_class
          mv_msgv1 = |{ iv_app }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_id_view_fields.

    CLEAR rv_view.

    SELECT id_view INTO rv_view UP TO 1 ROWS
           FROM zapd_t002
           WHERE app = mv_app
                 AND applevel = iv_level
                 AND doc_type = iv_doc_type
           ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDMETHOD.


  METHOD get_instance.


    DATA(lv_classname) = get_class_controler( iv_app ).

    CREATE OBJECT eo_app TYPE (lv_classname)
      EXPORTING
        iv_langu  = iv_langu
        iv_app    = iv_app
        iv_user   = iv_user.

  ENDMETHOD.


  METHOD get_kpi_all_apps.

    CLEAR rt_kpis.

    " Sacamos el listado de todas las aplicaciones activas
    TRY.
        get_apps(
          EXPORTING
            iv_langu = iv_langu
            iv_user  = iv_user
        IMPORTING
          et_apps  = DATA(lt_apps)  ).

        LOOP AT lt_apps ASSIGNING FIELD-SYMBOL(<ls_apps>).

          TRY.

              zcl_apd_apps=>get_instance(
                EXPORTING
                  iv_app   = <ls_apps>-app
                  iv_langu = iv_langu
                  iv_user  = iv_user
                IMPORTING
                  eo_app   = DATA(lo_app) ).

              DATA(lv_number) = lo_app->get_apps_kpi( ).
              INSERT VALUE #( app = <ls_apps>-app kpi = lo_app->get_apps_kpi( ) ) INTO TABLE rt_kpis.

            CATCH zcx_apd.
          ENDTRY.

        ENDLOOP.

      CATCH zcx_apd. " APD - Excepciones
    ENDTRY.
  ENDMETHOD.


  METHOD get_url_from_binary_content.
    DATA lv_service TYPE string.
    DATA lv_timestamp TYPE timestampl.
    DATA lv_mimetype TYPE string.


    TRY.
        lv_service = |{ iv_numdoc }{ cl_system_uuid=>if_system_uuid_static~create_uuid_c32( ) }|.
      CATCH cx_uuid_error .
        GET TIME STAMP FIELD lv_timestamp.
        lv_service = lv_timestamp.
    ENDTRY.

    zcl_ca_http=>generate_tmp_url(
      EXPORTING
        iv_content       = iv_content
        iv_mimetype      = iv_mimetype
        iv_service       = lv_service
        iv_doc_name      = |{ iv_numdoc }.{ iv_extension }|
        iv_langu         = mv_langu    " Clave de idioma del entorno de texto actual
      IMPORTING
        ev_url           = ev_url
         es_return   = DATA(ls_return) ).

    IF ls_return-type NE zif_apd_data=>cv_msg_type_error.

      " Cambiamos la URL de local a public
      change_url_internal_2_cloud( CHANGING cv_url = ev_url ).

    ELSE.

      RAISE EXCEPTION TYPE zcx_apd
        EXPORTING
          textid = zcx_apd=>get_document_not_possible.
    ENDIF.


  ENDMETHOD.


  METHOD get_view_fcat.

    CLEAR: et_fcat.

    " Obtenemos la vista donde hay que sacar los campos
    DATA(lv_view) = get_id_view_fields( iv_level = iv_level
                                        iv_doc_type = iv_doc_type ).

    IF lv_view IS NOT INITIAL. " Si hay vista se continua

      " Los campos de la vista
      mo_fcat->build_view_fcat( EXPORTING iv_view = lv_view
                                IMPORTING et_fcat = DATA(lt_fcat) ).

      " Ahora los campos propios asociados al campo maestro
      SELECT * INTO TABLE @DATA(lt_custom_data)
             FROM zapd_t003
             WHERE appl = @zif_apd_data=>cs_app-fcat_app
                  AND id_view = @lv_view.

      " Ahora consolidamos los datos
      LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        APPEND CORRESPONDING #( <ls_fcat> ) TO et_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat_all>).

        READ TABLE lt_custom_data ASSIGNING FIELD-SYMBOL(<ls_custom_data>) WITH KEY fldname = <ls_fcat>-field.
        IF sy-subrc = 0.
          <ls_fcat_all> = CORRESPONDING #( BASE ( <ls_fcat_all> ) <ls_custom_data>  ).
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD read_app_info.
    CLEAR et_info.

    SELECT a~app AS app b~descripcion AS title a~icono AS icon a~orden AS orden
          FROM zapd_t001 AS a INNER JOIN zapd_t001t AS b ON a~app EQ b~app
          INTO TABLE et_info
          WHERE b~spras EQ mv_langu
          AND a~app IN it_r_app
          AND   a~activo EQ abap_true.
  ENDMETHOD.


  METHOD send_massive_notif.
    DATA lv_jobcount   TYPE tbtcjob-jobcount.
    DATA lv_job_name   TYPE tbtcjob-jobname.
    DATA lv_ret        TYPE i.
    DATA lt_param_sl TYPE rsparams_tt.
    DATA lv_hora       TYPE t.
    DATA lv_fecha      TYPE d.
    DATA lv_tiempo_add TYPE t.
    DATA lv_job_delay  TYPE numc06.

    lt_param_sl = VALUE #( ( selname = 'P_APPL' kind = 'P' low = zif_apd_data=>cs_push-appl )
                            ( selname = 'P_TEST' kind = 'P' low = abap_false ) ).


    zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'JOBNAME_SEND_PUSH_MASSIVE'
                                          IMPORTING c_valor = lv_job_name  ).

    zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'JOBDELAY_SEND_PUSH_MASSIVE'
                                          IMPORTING c_valor = lv_job_delay  ).

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_job_name
        jobclass         = 'B'
      IMPORTING
        jobcount         = lv_jobcount
      CHANGING
        ret              = lv_ret
      EXCEPTIONS
        cant_create_job  = 1                " Job cannot be created, see system log
        invalid_job_data = 2                " Job Contains Invalid Job Data, See SYSLOG
        jobname_missing  = 3                " Job Name Not Specified
        OTHERS           = 4.

    IF sy-subrc = 0 AND lv_ret IS INITIAL.
      SUBMIT zapd_r_update_badge_push  WITH SELECTION-TABLE lt_param_sl
                                       VIA JOB lv_job_name NUMBER lv_jobcount
                                       USER 'EXP'
                                       AND RETURN.

      IF iv_inmediatly = abap_false.

        IF lv_job_delay IS INITIAL.
          lv_tiempo_add = '000100'.
        ELSE.
          lv_job_delay = |{ lv_job_delay }|.
          lv_tiempo_add = |{ lv_job_delay ALPHA = IN }|.
        ENDIF.

        CALL FUNCTION 'EWU_ADD_TIME'
          EXPORTING
            i_starttime = sy-uzeit
            i_startdate = sy-datum
            i_addtime   = lv_tiempo_add
          IMPORTING
            e_endtime   = lv_hora
            e_enddate   = lv_fecha.

        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_jobcount
            jobname              = lv_job_name
            sdlstrtdt            = lv_fecha
            sdlstrttm            = lv_hora
          CHANGING
            ret                  = lv_ret
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.
      ELSE.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = lv_jobcount
            jobname              = lv_job_name
            strtimmed            = abap_true
          CHANGING
            ret                  = lv_ret
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            invalid_target       = 8
            OTHERS               = 9.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD send_notification.

    TRY.
        DATA(lo_push) = NEW zcl_push_main( iv_langu = mv_langu
                                           iv_appl = zif_apd_data=>cs_push-appl ).

        " Aunque el sistema de nofificacion puede enviar más de una notificacion a multiples usuarios lo hago uno a uno.
        " El motivo es que el mensaje puede variar por usuario y sobretodo el badge de cada notificación, que cada usuario tendrá uno distinto.
        " El badge es el numero de KPIs.
        LOOP AT it_notificacion ASSIGNING FIELD-SYMBOL(<ls_notif>).

          lo_push->send_push(
            EXPORTING
              it_users        = VALUE #( ( <ls_notif>-username ) )
              it_notification = VALUE #( ( message = <ls_notif>-message
                                           badge = <ls_notif>-badge
                                           sound = COND #( WHEN <ls_notif>-message IS INITIAL THEN space ELSE zif_apd_data=>cs_push-default_sound ) ) )
            IMPORTING
              et_return       = DATA(lt_return) ).

        ENDLOOP.

      CATCH zcx_push.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
