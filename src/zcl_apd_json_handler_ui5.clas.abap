CLASS zcl_apd_json_handler_ui5 DEFINITION
  PUBLIC
  INHERITING FROM zcl_json_handler_base_ui5
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_APD_JSON_HANDLER_UI5
*"* do not include other source files here!!!
  PROTECTED SECTION.

    METHODS token_operation
      RAISING
        zcx_integra .
    METHODS pre_operations_services .

    METHODS pre_execute_settings
        REDEFINITION .
    METHODS mapping_nvp_2_params_object REDEFINITION.
  PRIVATE SECTION.
*"* private components of class ZCL_APD_JSON_HANDLER_UI5
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_apd_json_handler_ui5 IMPLEMENTATION.


  METHOD pre_execute_settings.

    CALL METHOD super->pre_execute_settings.

* Operaciones con el token
    token_operation( ).

* Operacion previas a la llamada de los servicios
    pre_operations_services( ).

  ENDMETHOD.


  METHOD pre_operations_services.


  ENDMETHOD.


  METHOD token_operation.

    FIELD-SYMBOLS <ls_nvp> TYPE LINE OF tihttpnvp.
    DATA lv_user TYPE syuname.
    DATA lv_token TYPE zca_e_token.
    DATA: ld_subrc TYPE subrc.

* Se busca el parámetro token
    READ TABLE mt_nvp ASSIGNING <ls_nvp> WITH KEY name = 'TOKEN'.
    IF sy-subrc NE 0.
      READ TABLE mt_nvp ASSIGNING <ls_nvp> WITH KEY name = 'token'.
    ENDIF.
    IF <ls_nvp> IS ASSIGNED.
      lv_token = <ls_nvp>-value.

* Se busca el usuario
      READ TABLE mt_nvp ASSIGNING <ls_nvp> WITH KEY name = 'USER'.
      IF sy-subrc NE 0.
        READ TABLE mt_nvp ASSIGNING <ls_nvp> WITH KEY name = 'user'.
      ENDIF.
      IF <ls_nvp> IS ASSIGNED.
        lv_user = <ls_nvp>-value.

        IF zcl_ca_token=>validar( i_token = lv_token i_app = zif_apd_data=>cv_app_ui5 i_name = lv_user ) = abap_true.

** Lo mapeo contra el parámetro usuario que tenga el objeto
*        CALL METHOD map_param_2_params_object
*          EXPORTING
*            iv_param = 'USER'
*            iv_value = lv_user.

        ELSE.
          RAISE EXCEPTION TYPE zcx_apd
            EXPORTING
              textid = zcx_apd=>token_not_valid.
        ENDIF.
      ENDIF.
    ENDIF.
    IF <ls_nvp> IS NOT ASSIGNED.
* Si no hay token y el servicio es el inicial no se lanzará excepcion
      CASE mv_type_object.
        WHEN 'FUNC'.
          IF mv_funcname NS 'USER_INFO'.
            RAISE EXCEPTION TYPE zcx_apd
              EXPORTING
                textid = zcx_apd=>token_not_valid.
          ENDIF.
        WHEN 'CLAS'.
          IF mv_method NS 'USER_INFO'.
            RAISE EXCEPTION TYPE zcx_apd
              EXPORTING
                textid = zcx_apd=>token_not_valid.
          ENDIF.
      ENDCASE.

    ENDIF.


  ENDMETHOD.
  METHOD mapping_nvp_2_params_object.

    " En el campo idioma llega ES-ES en vez de ES por tema de movilidad. Lo ajusto para recoger
    " los dos primer carácteres.
    READ TABLE mt_nvp ASSIGNING FIELD-SYMBOL(<ls_nvp>) WITH KEY name = 'LANGU'.
    IF sy-subrc NE 0.
      READ TABLE mt_nvp ASSIGNING <ls_nvp> WITH KEY name = 'langu'.
    ENDIF.
    IF <ls_nvp> IS ASSIGNED.
      <ls_nvp>-value = <ls_nvp>-value(2).
    ENDIF.

    super->mapping_nvp_2_params_object(  ).
  ENDMETHOD.

ENDCLASS.
