@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZSALES_BOM_CUBE'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSALES_BOM_CUBE
  as select distinct from ZSALES_BOM_CDS as A
{
  key A.SalesDocument,
  key A.SalesDocumentItem,
  key A.Material,
  key A.ProductDescription,
  key A.Plant,
  key A.CreationDate,
  key A.UOM,
  key A.BOM             as BOM,
  key A.Routing         as Routing,
  key ProductionVersion as Version,
  key A.SOQty,
  key A.ProcessLoss as  ProcessLoss,
  key A.CutWaste as  CutWaste,
  key A.qt11 ,
  key A.DivisionName,
      
      case when A.qt11 > 0 
      then
      cast( ( A.SOQty / A.qt11 * 100 ) as abap.dec( 16, 3 ) ) 
      else A.SOQty end  as ReqGreigeFabric,
      A.ReqGreigeFabric as ReqGreigeFabric_old,
      
      A.LotStatus,
      
      A.OrderQty,
      A.OrderStatus,
      A.BalanceQty, 
      
      
      A.IssuedLotQty,
      A.FoldingQty,
      A.YY1_MerchantFinalAppro_SDH as MerchantApprovalDate
}
 where SOStatus <> 'A'
