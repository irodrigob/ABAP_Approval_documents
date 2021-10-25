*&---------------------------------------------------------------------*
*& Report ZAPD_R_PO_RELEASE_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zapd_r_po_release_data.

PARAMETERS p_po TYPE ekko-ebeln.
PARAMETERS p_trtyp TYPE t160-trtyp DEFAULT 'A'.

START-OF-SELECTION.

  DATA(lo_order) = NEW cl_po_header_handle_mm( im_po_number = p_po ).
  DATA(lo_show) = cl_demo_output=>new( ).

  lo_order->po_initialize( ).
  lo_order->po_read( EXPORTING im_tcode     = 'ME29N'
                               im_trtyp     = p_trtyp
                               im_aktyp     = p_trtyp
                               im_po_number = p_po ).


  lo_order->get_data( IMPORTING ex_data = DATA(ls_header_data) ).

  lo_order->if_releasable_mm~get_data( IMPORTING ex_strategy = DATA(lo_strategy)
                                                 ex_state = DATA(lo_state) ).

  lo_strategy->get_info(
    IMPORTING
      ex_frgot            = DATA(lv_frgot)
      ex_group            = DATA(lv_group)
      ex_group_desc       = DATA(lv_group_desc)
      ex_strategy         = DATA(lv_strategy)
      ex_strategy_desc    = DATA(lv_strategy_desc)
      ex_codes            = DATA(lt_codes)
      ex_prerequisites    = DATA(lt_prerequisites)
      ex_final            = DATA(lt_final)
      ex_consistent       = DATA(lv_consistent)
      ex_overall_release  = DATA(lv_overall_release)
      ex_external_release = DATA(lv_external_release) ).

  DATA(lt_states_codes) = lo_state->get_state( ).

  lo_state->get_indicator(
    IMPORTING
      ex_indicator      = DATA(lv_indicator)
      ex_indicator_desc = DATA(lv_indicator_desc)
      ex_flref          = DATA(lv_flref)
      ex_kzfae          = DATA(lv_kzfae)
      ex_tlfae          = DATA(lv_tlfae)
      ex_fixbp          = DATA(lv_fixbp)
      ex_frang          = DATA(lv_frang)
      ex_frbst          = DATA(lv_frbst) ).

  lo_show->write_data( value = lv_frgot ).
  lo_show->write_data( value = lv_group ).
  lo_show->write_data( value = lv_group_desc ).
  lo_show->write_data( value = lv_strategy ).
  lo_show->write_data( value = lv_strategy_desc ).
  lo_show->write_data( value = lt_codes ).
  lo_show->write_data( value = lt_prerequisites ).
  lo_show->write_data( value = lt_final ).
  lo_show->write_data( value = lv_consistent ).
  lo_show->write_data( value = lv_overall_release ).
  lo_show->write_data( value = lv_external_release ).

  lo_show->display( ).
