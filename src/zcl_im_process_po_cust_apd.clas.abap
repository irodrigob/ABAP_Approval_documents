CLASS zcl_im_process_po_cust_apd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_me_process_po_cust .
  PROTECTED SECTION.
    DATA mo_app_po TYPE REF TO zcl_apd_app_po.
    DATA mt_release_steps_open TYPE zcl_apd_app_po=>tt_rel_code_steps.
    DATA ms_header_data_open TYPE mepoheader.


  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_IM_PROCESS_PO_CUST_APD IMPLEMENTATION.


  METHOD if_ex_me_process_po_cust~check.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~close.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_header.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_header_refkeys.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_item.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~fieldselection_item_refkeys.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~initialize.
    " Instanciamos la propia clase para usar la funcionalidad interna
    mo_app_po = NEW #( iv_app = zcl_apd_app_po=>cv_id_app ).
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~open.

    " Al leer el documento me guardo los pasos de aprobación que hay en el momento de hacer la aprobacion y la cabecera
    mo_app_po->get_release_info_from_handler( EXPORTING io_handler = CAST #( im_header )
                                              IMPORTING es_header_data = ms_header_data_open
                                                        et_steps = mt_release_steps_open ).


  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~post.
*    DATA lv_procstat_reject TYPE ekko-procstat.
*    DATA lt_users_notif_process TYPE zif_apd_app=>tt_user_notif_process.
*
*    TRY.
*
*        DATA(lo_handler) = CAST cl_po_header_handle_mm( im_header ).
*
*        " Obtengo los datos de liberación del pedido y los datos de cabecra
*        mo_app_po->get_release_info_from_handler( EXPORTING io_handler = lo_handler
*                                                IMPORTING es_header_data = DATA(ls_header_data_badi)
*                                                          et_steps = DATA(lt_relase_steps_post)
*                                                          et_release_codes = DATA(lt_release_codes) ).
*
*        " El proceso solo se realizá si el pedido tiene estrategia de liberacion y pasos configurados
*        IF lt_relase_steps_post IS NOT INITIAL AND ls_header_data_badi-frgsx IS NOT INITIAL AND ls_header_data_badi-frggr IS NOT INITIAL.
*
*          DATA(lv_same_approvers) = abap_false.
*
*          " Sacamos el numero de pasos aprobados de ambos sitios
*          DATA(lt_steps_approved_post) = VALUE zcl_apd_app_po=>tt_rel_code_steps( FOR <wa1> IN lt_relase_steps_post WHERE ( approved = abap_true ) ( <wa1> ) ).
*          DATA(lv_num_approved_post) = lines( lt_steps_approved_post ).
*
*          DATA(lv_num_approved_open) = REDUCE #( INIT x = 0 FOR <wa> IN mt_release_steps_open WHERE ( approved = abap_true ) NEXT x = x + 1 ).
*
*          " Si el número de registros es igual, esta claro que no se ha aprobado nada. Si son distintos es que o bien se ha aprobado o se ha vuelto atras.
*          lv_same_approvers = COND #( WHEN lv_num_approved_post = lv_num_approved_open THEN abap_true ELSE abap_false ).
*
*          " Si ha habido aprobacion o hay cambios en el status de aprobación: que este rechazado y ahora no lo este, o antes estaba rechazado y ahora no lo esta.
*          " Es cuando se iniciara el proceso de notificación
*          IF lv_same_approvers = abap_false OR ms_header_data_open-procstat NE ls_header_data_badi-procstat.
*
*            " Recuperamos el valor del campo procstat cuando el pedido se rechaza
*            zcl_apd_constants=>obtener_constante( EXPORTING i_constante = 'REJECT_PROCSTAT'
*                                                            IMPORTING c_valor     = lv_procstat_reject ).
*            " Determinamos si el pedido ha sido rechazado
*            DATA(lv_po_rejected) = COND #( WHEN ls_header_data_badi-procstat = lv_procstat_reject THEN abap_true ELSE abap_false ).
*            " Determinados si originalmente ha sido rechazado
*            DATA(lv_po_prev_rejected) = COND #( WHEN ms_header_data_open-procstat = lv_procstat_reject THEN abap_true ELSE abap_false ).
*
*            " Montamos la tabla para el proceso de notificaciones
*
*            " Primer paso a los que han aprobado.
*
*            " Si el pedido no esta rechazado hay que sacar el último codigo de liberación aprobado. Es decir, el paso que acaba de ser aprobado o echado para atrás. Aunque el caso de echar para atras
*            " habrá que ir viendolo porque esos usuarios porque a lo mejor no tienen que recibirlas. Pero es imposible detectar el rechazo salvo que se comparé con los datos
*            " originales. De momento no se complica el proceso.
*            " Si no hay pasos aprobados es que estamos en el proceso inicial o se ha echado para atrás todos los pasos.
*            IF lv_num_approved_post NE 0.
*              IF lv_po_rejected = abap_false.
*
*                DATA(lv_rel_code_approved) = lt_steps_approved_post[ lv_num_approved_post ]-rel_code.
*
*                " Si previamente estaba rechazado no añadiré a los usuario porque su paso ya lo aprobarón, y no se les enviará la notificación que lo han aprobado cuando no es así.
*                IF lv_po_prev_rejected = abap_false.
*                  " Guardamos los usuarios aprobadores que van recibir la notificación que el pedido ha sido aprobado
*                  lt_users_notif_process = VALUE #( FOR <wa> IN lt_steps_approved_post WHERE ( rel_code = lv_rel_code_approved  ) ( username = <wa>-username approved = abap_true  ) ).
*                ENDIF.
*
*              ELSE.
*                " En el rechazo se enviará a todos los que hayan aprobado
*                lt_users_notif_process = VALUE #( FOR <wa> IN lt_steps_approved_post WHERE ( approved = abap_true  ) ( username = <wa>-username approved = abap_true  ) ).
*              ENDIF.
*            ENDIF.
*
*            " Ahora si hay codigo de liberación aprobado miro el siguiente para saber a los usuarios que tienen pendiente aprobar.
*            " Si no hay codigo de liberación, será cuando todavía no hay nada aprobado, se toma el primero.
*            DATA(lv_rel_code_pending) = VALUE frgco(  ).
*
*
*            IF lv_rel_code_approved IS NOT INITIAL.
*              READ TABLE lt_release_codes TRANSPORTING NO FIELDS WITH KEY rel_code = lv_rel_code_approved.
*              IF sy-subrc = 0.
*                DATA(lv_tabix) = sy-tabix + 1.
*                " Controlo que no lea un indice que no exista.
*                IF lv_tabix <= lines( lt_release_codes  ).
*                  lv_rel_code_pending = lt_release_codes[ lv_tabix ]-rel_code.
*                ENDIF.
*
*              ENDIF.
*            ELSE.
*              lv_rel_code_pending = lt_release_codes[ 1 ]-rel_code.
*            ENDIF.
*
*            " Si hay codigo de liberación pendiente saco sus usuarios para enviar la notificación
*            IF lv_rel_code_pending IS NOT INITIAL.
*
*              " Si hay aprobadores en pasos previos lo añadiré, pero hay que tener en cuenta la excepción.
*              IF lt_steps_approved_post IS NOT INITIAL.
*                lt_users_notif_process = VALUE #( BASE lt_users_notif_process
*                                                  FOR <wa> IN lt_steps_approved_post WHERE ( rel_code = lv_rel_code_pending  ) ( username = <wa>-username pending = abap_true  ) ).
*              ELSEIF lv_po_rejected = abap_true.
*                " Si esta vacio y se ha rechazado significa que el primer nivel lo ha actualizado en ese caso voy a optar por una solución salomonica.
*
*                " Añado todos los usuarios pero con la indicación que lo han aprobado, esto hace, que se envie una alerta sin mensaje pero con el badge actualizado
*                lt_users_notif_process = VALUE #( BASE lt_users_notif_process
*                                                   FOR <wa> IN lt_relase_steps_post ( username = <wa>-username approved = abap_true  ) ).
*              ELSEIF ls_header_data_badi-ebeln IS INITIAL.
*                " Si no hay pedido es que se esta creando de nuevo, en ese caso se envia al primer aprobador
*                lt_users_notif_process = VALUE #( BASE lt_users_notif_process
*                                                    FOR <wa> IN lt_relase_steps_post WHERE ( rel_code = lv_rel_code_pending  ) ( username = <wa>-username pending = abap_true  ) ).
*              ENDIF.
*            ENDIF.
*
*            " Si tengo a quien enviar la notificación, lanzo el proceso de envio
*            IF lt_users_notif_process IS NOT INITIAL.
*
*              mo_app_po->zif_apd_app~send_notification(
*                EXPORTING
*                  iv_num_doc      = CONV #( ls_header_data_badi-ebeln )
*                  iv_doc_rejected = lv_po_rejected
*                  it_users        = lt_users_notif_process
*                IMPORTING
*                  et_return       = DATA(lt_return) ).
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.

    " Hay ciertas casuísticas que no es posible controlar en la BADI o que va a complicar el código de tal manera que va hacer
    " dificil el mantenimiento. Ejemplos:
    " 1) Que cambien datos del pedido que haga que los aprobadores ya no sean los mismos. Haciendo
    " casi imposible saber quien tenia la aprobación y quien no.
    " 2) Borrado de todas las posiciones haciendo que no sea necesario la aprobación
    " En esos casos lo que se va hacer es lanzar una actualización masiva de badge para todos los usuarios
    " excepto lo que eviará notificación.
    " EXPORTING it_exclude_user = VALUE #( FOR <wa2> IN lt_users_notif_process ( <wa2>-username ) )
*    zcl_apd_apps=>send_massive_notif(  ).

*      CATCH cx_root.
*    ENDTRY..


  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_account.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_header.

  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_item.
  ENDMETHOD.


  METHOD if_ex_me_process_po_cust~process_schedule.
  ENDMETHOD.
ENDCLASS.
