CLASS zcl_sales_bom_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.


    TYPES : BEGIN OF st_2 ,
              plant              TYPE string,
              salesdocument      TYPE i_salesdocumentitem-salesdocument,
              salesdocumentitem  TYPE i_salesdocumentitem-salesdocumentitem,
*              material           TYPE i_product-product,
              material           TYPE C LENGTH 18 ,
              productdescription TYPE string,
              soqty              TYPE string,
              bom                TYPE i_billofmaterialheaderdex_2-billofmaterial,
              routing            TYPE string,
              version            TYPE string,
            END OF st_2 .

    DATA : it_hed TYPE  st_2 .

    "Material": "FDWOSRKX2010",
    "Description": "Kab-17590 85%Visc 12%Poly 3%Lycra",
    "OperationsGroup": "50000000",
    "GroupCounter": "3"

    TYPES : BEGIN OF st_item ,
              material          TYPE c lENGTH 18,
              component         TYPE i_product-Product ,
              productionversion TYPE i_productionversiontp-productionversion,
              description       TYPE string,
              bom               TYPE c LENGTH 2,  " ALTERNATIVE BOM
              minlotsizeqty     TYPE string,
              maxlotsizeqty     TYPE string,
              operationsgroup   TYPE i_mfgbillofoperationschgst-billofoperationsgroup,
              groupcounter      TYPE i_mfgbillofoperationschgst-boointernalversioncounter,
              qty               TYPE p LENGTH 16 DECIMALS 3,
              uom               TYPE string,
              fixedquantity     TYPE c LENGTH 1,  " ALTERNATIVE BOM
              pt                TYPE c LENGTH 40,
              dosingtime        TYPE c LENGTH 10,
              temprature        TYPE c LENGTH 10,
              injectdosing      TYPE c LENGTH 10,
              holdtime          TYPE c LENGTH 10,
*              percentage  TYPE p LENGTH 16 DECIMALS 3,

            END OF st_item .

    DATA : it_item TYPE TABLE OF st_item .

    TYPES : BEGIN OF st_hed ,
              tabledataarray1 LIKE it_item,
              headerdata      LIKE it_hed,
            END OF st_hed .

    DATA : respdata TYPE  st_hed .


    """""""""" ALL METHODS
    CLASS-METHODS : bomcreate
      IMPORTING
                VALUE(respodata) LIKE respdata

      RETURNING VALUE(bom_res)   TYPE string
      RAISING   cx_static_check .

    CLASS-METHODS : routingassign
      IMPORTING
                VALUE(respodata)   LIKE respdata

      RETURNING VALUE(routing_res) TYPE string
      RAISING   cx_static_check .

    CLASS-METHODS : pvcreate
      IMPORTING
                VALUE(respodata) LIKE respdata

      RETURNING VALUE(pv_res)    TYPE string
      RAISING   cx_static_check .



    INTERFACES if_http_service_extension .
    INTERFACES if_oo_adt_classrun.
    INTERFACES if_t100_message.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SALES_BOM_HTTP IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA(req) = request->get_form_fields(  ).
    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).


    DATA(body) = request->get_text( ).
    xco_cp_json=>data->from_string( body )->write_to( REF #( respdata ) ).

    DATA(type) = VALUE #( req[ name = 'type' ]-value OPTIONAL ) .


    """ CREATE BOM

    IF type = 'BOM' .
      DATA(ms_res) = me->bomcreate( respodata = respdata ) .

    ELSEIF type = 'Routing' .
      ms_res = me->routingassign( respodata = respdata ) .
    ELSEIF type = 'PV' .
      ms_res = me->pvcreate( respodata = respdata ) .
    ENDIF .


    response->set_text( ms_res ) .

  ENDMETHOD.


  METHOD bomcreate .

    respodata-headerdata-salesdocument = |{ respodata-headerdata-salesdocument ALPHA = IN }| .
    respodata-headerdata-material = |{ respodata-headerdata-material ALPHA = IN }| .

    DATA : ms_respo TYPE string .

    MODIFY ENTITIES OF i_salesorderbillofmaterialtp_2
    ENTITY salesbillofmaterial
    CREATE
    AUTO FILL CID
    SET FIELDS WITH VALUE #( (
    plant = respodata-headerdata-plant
    salesorder = respodata-headerdata-salesdocument
    salesorderitem = respodata-headerdata-salesdocumentitem
    material = respodata-headerdata-material



    billofmaterialvariantusage = '1'
    billofmaterialcategory = 'K'
    billofmaterialvariant = '01'
    engineeringchangedocument = ''
    ) )
    MAPPED DATA(ls_mapped1)
    FAILED DATA(ls_failed1)
    REPORTED DATA(ls_reported1).

    COMMIT ENTITIES BEGIN
        RESPONSE OF i_salesorderbillofmaterialtp_2
        FAILED   DATA(ls_save_failed_hed)
        REPORTED DATA(ls_save_reported_hed).
    COMMIT ENTITIES END.


    IF ls_mapped1-salesbillofmaterial IS NOT INITIAL .

*          DATA(lv_add) = ls_failed1-salesbillofmaterial[ 1 ]-%fail->cause( ).
      DATA(ms_bom) = ls_mapped1-salesbillofmaterial[ 1 ]-billofmaterial.

      ms_respo = |{ ms_respo } SUCCESS:- BOM Created { ms_bom } Successfully.!! | .
    ELSE .

      IF ls_reported1-salesbillofmaterial IS NOT INITIAL .
        LOOP AT ls_reported1-salesbillofmaterial ASSIGNING FIELD-SYMBOL(<ms_error11>).
*          DATA(msh1) = ls_reported1-salesbillofmaterial[ 1 ]-%msg->if_message~get_longtext( ).
*          DATA(msh2) = ls_reported1-salesbillofmaterial[ 1 ]-%msg->if_message~get_text( ).
          DATA(msh1) = <ms_error11>-%msg->if_message~get_longtext( ).
          DATA(msh2) = <ms_error11>-%msg->if_message~get_text( ).
        ENDLOOP.
      ENDIF.

      IF ls_reported1-salesbillofmaterialitem IS NOT INITIAL .
        LOOP AT ls_reported1-salesbillofmaterialitem ASSIGNING FIELD-SYMBOL(<ms_error12>).
          DATA(msh11) = <ms_error12>-%msg->if_message~get_longtext( ).
          DATA(msh12) = <ms_error12>-%msg->if_message~get_text( ).
        ENDLOOP.
      ENDIF.

      ms_respo = |{ ms_respo } ERROR_H:- { msh2 }.!! | .

      CLEAR : msh1,msh2,msh11,msh12 .
    ENDIF.


    """""""""" FOR ITEM
    IF ms_bom IS NOT INITIAL .

    data : mbom tYPE c lENGTH 18 .
*    mbom = respodata-tabledataarray1-component .

      SELECT FROM i_product AS a
      FIELDS
      a~product,
      a~producttype

      FOR ALL ENTRIES IN @respodata-tabledataarray1
      WHERE a~product = @respodata-tabledataarray1-component
      INTO TABLE @DATA(it_mat).


      DATA : ptype TYPE string .
      LOOP AT respodata-tabledataarray1 INTO DATA(wa_item) .

        ptype  = VALUE #( it_mat[ product = wa_item-component ]-producttype OPTIONAL ) .
        IF ptype = 'ZCHA' .
          wa_item-fixedquantity = 'X' .
        ENDIF.


        mbom = |{ wa_item-component ALPHA = IN }| .

        IF respodata-headerdata-material = mbom .
        DATA(RecursiveBOM) = 'X' .
        ELSE .
        RecursiveBOM = ' ' .
        ENDIF.


        MODIFY ENTITIES OF i_salesorderbillofmaterialtp_2
        ENTITY salesbillofmaterial
        CREATE BY \_billofmaterialitem
        AUTO FILL CID
        SET FIELDS WITH VALUE
        #( (

        billofmaterial = ms_bom
        material = respodata-headerdata-material
        plant = respodata-headerdata-plant
*      billofmaterial = '00000032'
*      material = '0000000156'
*      plant = '1100'
        billofmaterialcategory = 'K'
        billofmaterialvariant = '01'
        engineeringchangedocument = ''

        %target = VALUE #(

        (
*        billofmaterialcomponent = wa_item-component
        billofmaterialcomponent = mbom
        billofmaterialitemquantity = wa_item-qty
        billofmaterialitemunit = wa_item-uom
        billofmaterialitemcategory = 'L'
        bomitemhasfixedquantity = wa_item-fixedquantity
        bomitemdescription = wa_item-pt
        bomitemtext2  = wa_item-temprature
        bomitemsorter = wa_item-holdtime
         leadtimeoffset = wa_item-dosingtime
         IsBOMRecursiveAllowed = RecursiveBOM
*         BOMIsRecursive = RecursiveBOM




        ) )
        ) )

        MAPPED DATA(ls_mapped2)
        FAILED DATA(ls_failed2)
        REPORTED DATA(ls_reported2).


        COMMIT ENTITIES BEGIN
            RESPONSE OF i_salesorderbillofmaterialtp_2
            FAILED   DATA(ls_save_failed_item)
            REPORTED DATA(ls_save_reported_item).
        COMMIT ENTITIES END.

        IF ls_mapped2 IS NOT INITIAL .

*          ms_respo = |{ ms_respo } BOM Created { ms_bom2 } Successfully !! | .
        ELSE .
          IF ls_reported2-salesbillofmaterial IS NOT INITIAL .
            LOOP AT ls_reported2-salesbillofmaterial ASSIGNING FIELD-SYMBOL(<ms_error21>).
              DATA(ms_i1) = <ms_error21>-%msg->if_message~get_longtext( ).
              DATA(ms_i2) = <ms_error21>-%msg->if_message~get_text( ).
            ENDLOOP.
          ENDIF.

          IF ls_reported2-salesbillofmaterialitem IS NOT INITIAL .
            LOOP AT ls_reported2-salesbillofmaterialitem ASSIGNING FIELD-SYMBOL(<ms_error22>).
              DATA(ms_i21) = <ms_error22>-%msg->if_message~get_longtext( ).
              DATA(ms_i22) = <ms_error22>-%msg->if_message~get_text( ).
            ENDLOOP.
          ENDIF.
          IF ms_i2 IS INITIAL .
            ms_i2 = ms_i22 .
          ENDIF.

          ms_respo = |{ ms_respo } ERROR_I :- { wa_item-component } { ms_i2 }. | .
        ENDIF.


        CLEAR : wa_item,ms_i2,ms_i1,ms_i21,ms_i22,mbom .
      ENDLOOP.

    ENDIF.


    bom_res = ms_respo .

  ENDMETHOD.


  METHOD routingassign .

    DATA :  prodrouting TYPE c LENGTH 2 .

    READ TABLE respodata-tabledataarray1 INTO DATA(wa_item) INDEX 1 .

    prodrouting = |{ wa_item-groupcounter ALPHA = IN  }| .

    wa_item-operationsgroup = |{ wa_item-operationsgroup ALPHA = IN  }| .
    wa_item-groupcounter = |{ wa_item-groupcounter ALPHA = IN  }| .
    respodata-headerdata-material = |{ respodata-headerdata-material ALPHA = IN  }| .
    respodata-headerdata-salesdocument = |{ respodata-headerdata-salesdocument ALPHA = IN  }| .




    MODIFY ENTITIES OF i_productionroutingtp_2
       ENTITY productionroutingheader
       CREATE BY \_matlassgmt FROM
       VALUE #( (
*       %key = VALUE #( productionroutinggroup = wa_item-operationsgroup productionrouting = '01' productionroutinginternalvers = wa_item-groupcounter )
       %key = VALUE #( productionroutinggroup = wa_item-operationsgroup  productionrouting = prodrouting productionroutinginternalvers = wa_item-groupcounter )
       %target = VALUE #( (
       %cid = 'CID_MAT_001'
       plant = respodata-headerdata-plant
       product = respodata-headerdata-material
       salesorder = respodata-headerdata-salesdocument
       salesorderitem = respodata-headerdata-salesdocumentitem

       %control-plant = cl_abap_behv=>flag_changed
       %control-product = cl_abap_behv=>flag_changed
       %control-salesorder = cl_abap_behv=>flag_changed
       %control-salesorderitem = cl_abap_behv=>flag_changed
       ) )
       ) )
      MAPPED DATA(ls_mapped)
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).

    COMMIT ENTITIES BEGIN
         RESPONSE OF i_productionroutingtp_2
         FAILED   DATA(ls_save_failed_item)
         REPORTED DATA(ls_save_reported_item).
    COMMIT ENTITIES END.

    IF ls_save_reported_item-productionroutingheader IS NOT INITIAL .
      LOOP AT ls_save_reported_item-productionroutingheader ASSIGNING FIELD-SYMBOL(<ms_error11>).
*          DATA(msh1) = ls_save_failed_item-productionroutingheader[ 1 ]-%fail->cause->if_message~get_longtext( ).
*          DATA(msh2) = ls_reported1-salesbillofmaterial[ 1 ]-%msg->if_message~get_text( ).
        DATA(msh1) = <ms_error11>-%msg->if_message~get_longtext( ).
        DATA(msh2) = <ms_error11>-%msg->if_message~get_text( ).

      ENDLOOP.

      msh2 = ls_save_reported_item-productionroutingheader[ 1 ]-%msg->if_message~get_text( ).
      msh2 = |ERROR:- { msh2 }| .


    ELSE.

      respodata-headerdata-material = |{ respodata-headerdata-material ALPHA = OUT  }| .
      respodata-headerdata-salesdocument = |{ respodata-headerdata-salesdocument ALPHA = OUT  }| .
      respodata-headerdata-salesdocumentitem = |{ respodata-headerdata-salesdocumentitem ALPHA = OUT  }| .

      msh2 = |SUCCESS:- Routing Updated For Material-{ respodata-headerdata-material } & SO-{ respodata-headerdata-salesdocument }/{ respodata-headerdata-salesdocumentitem }  !!|.

    ENDIF.



    routing_res = msh2 .

  ENDMETHOD.


  METHOD pvcreate .

    DATA: lt_config_data TYPE TABLE FOR CREATE i_productionversiontp.


    DATA :  prodrouting TYPE c LENGTH 2 .

    READ TABLE respodata-tabledataarray1 INTO DATA(wa_item) INDEX 1 .

*    prodrouting = |{ wa_item-groupcounter ALPHA = IN  }| .

    wa_item-operationsgroup = |{ wa_item-operationsgroup ALPHA = IN  }| .
    wa_item-groupcounter = |{ wa_item-groupcounter ALPHA = IN  }| .
    respodata-headerdata-material = |{ respodata-headerdata-material ALPHA = IN  }| .
    wa_item-bom = |{ wa_item-bom ALPHA = IN  }| .
    respodata-headerdata-salesdocument = |{ respodata-headerdata-salesdocument ALPHA = IN  }| .


    lt_config_data = VALUE #( (
    %cid = 'W1'

    material = respodata-headerdata-material
    plant = respodata-headerdata-plant
    productionversion = wa_item-productionversion
    productionversiontext = wa_item-description
    billofoperationstype = 'N'
    billofoperationsgroup = wa_item-operationsgroup " Routing
    billofoperationsvariant = '01'     " Counter-Routing
    billofmaterialvariantusage = '1'   "
    billofmaterialvariant = wa_item-bom       " Alternative BOM
    materialminlotsizequantity = wa_item-minlotsizeqty
    materialmaxlotsizequantity = wa_item-maxlotsizeqty

    %control = VALUE #(
    material = if_abap_behv=>mk-on
    plant = if_abap_behv=>mk-on
    productionversion = if_abap_behv=>mk-on
    productionversiontext = if_abap_behv=>mk-on
    billofoperationstype = if_abap_behv=>mk-on
    billofoperationsgroup = if_abap_behv=>mk-on
    billofoperationsvariant = if_abap_behv=>mk-on
    billofmaterialvariantusage = if_abap_behv=>mk-on
    billofmaterialvariant = if_abap_behv=>mk-on
    materialmaxlotsizequantity = if_abap_behv=>mk-on
    materialminlotsizequantity = if_abap_behv=>mk-on
    )
    ) ) .

    MODIFY ENTITIES OF i_productionversiontp
    ENTITY productionversion
    CREATE FROM lt_config_data
        MAPPED DATA(ls_mapped)
      FAILED DATA(ls_failed)
      REPORTED DATA(ls_reported).

    COMMIT ENTITIES BEGIN
    RESPONSE OF i_productionversiontp
         FAILED   DATA(ls_save_failed_item)
         REPORTED DATA(ls_save_reported_item).
    COMMIT ENTITIES END.



    IF ls_save_reported_item-productionversion IS NOT INITIAL .
      LOOP AT ls_save_reported_item-productionversion ASSIGNING FIELD-SYMBOL(<ms_error11>).
*          DATA(msh1) = ls_save_failed_item-productionroutingheader[ 1 ]-%fail->cause->if_message~get_longtext( ).
        DATA(msh1) = <ms_error11>-%msg->if_message~get_longtext( ).
        DATA(msh2) = <ms_error11>-%msg->if_message~get_text( ).

      ENDLOOP.


      msh2 = ls_save_reported_item-productionversion[ 1 ]-%msg->if_message~get_text( ).
      msh2 = |ERROR:- { msh2 }| .


    ELSE.

      IF ls_reported-productionversion IS NOT INITIAL .
        DATA(msh91) = ls_reported-productionversion[ 1 ]-%msg->if_message~get_text( ).

        msh2 = |ERROR:- { msh91 }| .
      ELSE .

        respodata-headerdata-material = |{ respodata-headerdata-material ALPHA = OUT  }| .

        msh2 = |SUCCESS:- Production Version Created:- { wa_item-productionversion } For Material { respodata-headerdata-material } & Plant { respodata-headerdata-plant } !!|.

      ENDIF.
    ENDIF.



    pv_res = msh2 .

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main .

*    MODIFY ENTITIES OF i_productionversiontp
*      ENTITY productionversion
*      EXECUTE checkprodnversconstcy
*      FROM VALUE #(
*        ( %key-material          = 'FDWOSRKX2010'
*          %key-plant             = '1100'
*          %key-productionversion = 'PV01' )
*      )
*      REPORTED DATA(reported)
*      FAILED   DATA(failed)
*      MAPPED   DATA(mapped).
*
*    IF failed IS INITIAL.
**      LOOP AT reported INTO DATA(ls_reported).
**        " Here, check ls_reported-%msg for messages
**      ENDLOOP.
*    ENDIF.


    """"""""""""""""""""""""""" FOR Consistency  Check """"""""""""""""""""""'

    MODIFY ENTITIES OF i_productionversiontp
    ENTITY productionversion
    EXECUTE checkprodnversconstcy FROM VALUE #( (  %key-material = 'FDWOSRKX2010' %key-plant = '1100' %key-productionversion ='PV01'
    )  )
    REPORTED DATA(reported)
    FAILED DATA(failed)
    MAPPED DATA(mapped).

    IF failed IS INITIAL.
      COMMIT ENTITIES BEGIN
      RESPONSE OF i_productionversiontp
      FAILED DATA(save_failed)
      REPORTED DATA(save_reported).
      COMMIT ENTITIES END .
    ENDIF.


    LOOP AT reported-productionversion ASSIGNING FIELD-SYMBOL(<ms_error11>).
*          DATA(msh1) = ls_save_failed_item-productionroutingheader[ 1 ]-%fail->cause->if_message~get_longtext( ).
*          DATA(msh2) = ls_reported1-salesbillofmaterial[ 1 ]-%msg->if_message~get_text( ).
      DATA(msh1) = <ms_error11>-%msg->if_message~get_text( ).
*        DATA(msh2) = <ms_error11>-%msg->if_message~get_text( ).
    ENDLOOP.


    """"""""""""""""""" 1


*    DATA: lt_consistency TYPE TABLE FOR ACTION IMPORT i_productionversiontp~checkprodnversconstcy.
*
*    lt_consistency = VALUE #( (
**     %cid_ref = 'W1'
*     material = 'FDWOSRKX2010' plant = '1100' productionversion ='0001'
*    ) ) .
*
*    MODIFY ENTITIES OF i_productionversiontp
*  ENTITY productionversion
*  EXECUTE checkprodnversconstcy FROM lt_consistency
*
*     MAPPED DATA(ls_mapped)
*    FAILED DATA(ls_failed)
*    REPORTED DATA(ls_reported).
*
*    COMMIT ENTITIES BEGIN
*    RESPONSE OF i_productionversiontp
*         FAILED   DATA(ls_save_failed_item)
*         REPORTED DATA(ls_save_reported_item).
*    COMMIT ENTITIES END.



    "" chat

*    DATA: lt_check  TYPE TABLE FOR ACTION IMPORT i_productionversiontp~checkprodnversconstcy,
*          lt_result TYPE TABLE FOR ACTION RESULT i_productionversiontp~checkprodnversconstcy.
*
*    lt_check = VALUE #( ( %cid_ref = 'W'
*                           material = 'FDWOSRKX2010' plant = '1100' productionversion ='0001'  ) ).
*
*    MODIFY ENTITIES OF I_ProductionVersionTP
*      ACTION checkprodnversconstcy
*      FROM lt_check
*      RESULT DATA(lt_result).
*
*    IF lt_result IS NOT INITIAL.
*      LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<fs_result>).
*        " Check <fs_result>-%msg for consistency messages
*      ENDLOOP.
*    ENDIF.


  ENDMETHOD.
ENDCLASS.
