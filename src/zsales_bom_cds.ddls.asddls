@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales BOM'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSALES_BOM_CDS
  as select from    I_SalesDocumentItem    as a
    left outer join I_SalesDocument        as b    on b.SalesDocument = a.SalesDocument
    left outer join I_SalesOrderBOMLink    as C    on  C.SalesOrder     = a.SalesDocument
                                                   and C.SalesOrderItem = a.SalesDocumentItem
    left outer join I_ProductDescription_2 as d    on  d.Product  = a.Material
                                                   and d.Language = 'E'
    left outer join ZROUTING_CHECK         as Rout on  Rout.SalesOrder     = a.SalesDocument
                                                   and Rout.SalesOrderItem = a.SalesDocumentItem
                                                   and Rout.Product        = a.Material
                                                   and Rout.Plant          = a.Plant
    left outer join ZPV_CHECK              as PV   on  PV.SalesOrder     = a.SalesDocument
                                                   and PV.SalesOrderItem = a.SalesDocumentItem
                                                   and PV.Material       = a.Material
                                                   and PV.Plant          = a.Plant
    left outer join ZLOT_DATA              as lot  on  lot.salesdoc = a.SalesDocument
                                                   and lot.soitem   = a.SalesDocumentItem
    //                                                   and lot.material = a.Material
                                                   and lot.plant    = a.Plant
    left outer join ZSO_DATA               as so   on  so.SalesOrder     = a.SalesDocument
                                                   and so.SalesOrderItem = a.SalesDocumentItem
                                                   and so.Material       = a.Material
                                                   and so.Plant          = a.Plant
    left outer join ZPROD_ORDRE_DATA       as odr  on  odr.SalesOrder     = a.SalesDocument
                                                   and odr.SalesOrderItem = a.SalesDocumentItem
    left outer join I_DivisionText         as e   on e.Division = a.Division and e.Language = 'E'                                                
                                                   
                                                   
{
  key a.SalesDocument,
  key a.SalesDocumentItem,
      a.Material,
      a.Plant,
      a.YY1_MerchantFinalAppr1_SDI    as    YY1_MerchantFinalAppro_SDH ,
      a.Division ,
      a.DistributionChannel,
      e.DivisionName,
      a.SDDocumentReason ,
      b.CreationDate,
      b.SalesDocApprovalStatus                        as SOStatus,
      a.OrderQuantityUnit                             as UOM,
      //      C.BillOfMaterial                            as BOM,
      cast(a.OrderQuantity as abap.dec( 16, 3 ) )     as SOQty,
      d.ProductDescription,

      case when C.BillOfMaterial is not null
      then cast ( 'Created'  as abap.char( 20 ) ) end as BOM,

      case when PV.Plant is not null
      then cast ( 'Created'  as abap.char( 20 ) ) end as ProductionVersion,

      case when Rout.SalesOrder is not null
      then cast ( 'Created'  as abap.char( 20 ) ) end as Routing,

      case when lot.plant is not null
      then cast ( 'Created'  as abap.char( 20 ) ) end as LotStatus,

      lot.TotQty                                      as IssuedLotQty,

      so.Qty                                          as FoldingQty,

      odr.Qty                                         as OrderQty,
      case when odr.Qty > 1
      then cast ( 'Created'  as abap.char( 20 ) ) end as OrderStatus,
      
      
      cast( ( a.OrderQuantity + ( a.OrderQuantity * a.YY1_ProcessLoss_SDI )  / 100 )
       as abap.dec( 16, 3 ) ) - coalesce( odr.Qty, 0 ) as BalanceQty,


      a.OrderQuantityUnit,
      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      a.OrderQuantity,
      a.YY1_ProcessLoss_SDI   as ProcessLoss,
      a.YY1_CutWaste_SDI      as CutWaste,

//A2/(100-B2-C2)*100 "" changed by MS/AS 02/01/2026 (Requirement by Gunjan ji)

//      cast( ( a.OrderQuantity / &1 * 100 ) as abap.dec( 16, 3 ) )   as ReqGreigeFabric,

     cast( 100  - coalesce( a.YY1_ProcessLoss_SDI , 0 ) - coalesce(a.YY1_CutWaste_SDI, 0 ) as abap.dec( 16, 3 ) ) as qt11 , 
       
      cast( ( a.OrderQuantity + ( a.OrderQuantity * a.YY1_ProcessLoss_SDI )  / 100 )
       as abap.dec( 16, 3 ) )                         as ReqGreigeFabric



}
where
  b.SalesDocumentType  <> 'CBAR'
  and e.DivisionName <> 'SAMPLE'
  and a.SDDocumentReason  is initial
  
