class ZCX_APD definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of APP_WITHOUT_HANDLER_CLASS,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of APP_WITHOUT_HANDLER_CLASS .
  constants:
    begin of TOKEN_NOT_VALID,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TOKEN_NOT_VALID .
  constants:
    begin of NO_APPS,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_APPS .
  constants:
    begin of ORDER_NOT_EXIST,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ORDER_NOT_EXIST .
  constants:
    begin of GENERATE_PO_PDF_NOT_POSSIBLE,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GENERATE_PO_PDF_NOT_POSSIBLE .
  constants:
    begin of GET_DOCUMENT_NOT_POSSIBLE,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of GET_DOCUMENT_NOT_POSSIBLE .
  constants:
    begin of NO_ORDERS_FOUND,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_ORDERS_FOUND .
  constants:
    begin of ERROR_REJECT_PO_ORDER,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_REJECT_PO_ORDER .
  constants:
    begin of INVOICE_NOT_EXIST,
      msgid type symsgid value 'ZAPD',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of INVOICE_NOT_EXIST .
  data MV_MSGV1 type SYMSGV .
  data MV_MSGV2 type SYMSGV .
  data MV_MSGV3 type SYMSGV .
  data MV_MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional .
  class-methods RAISE_EXCEPTION
    importing
      !IV_ID_EXCEPTION type ANY
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_APD IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method RAISE_EXCEPTION.
  endmethod.
ENDCLASS.
