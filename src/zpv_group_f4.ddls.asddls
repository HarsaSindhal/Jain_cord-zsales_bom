@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZPV_GROUP_F4'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZPV_GROUP_F4
  as select from I_MfgBOOMaterialAssgmtChgSt as A
  //    left outer join I_SalesOrderBOMLink         as BOM on  BOM.Material       = A.Product
  //                                                       and BOM.Plant          = A.Plant
  //                                                       and BOM.SalesOrder     = A.SalesOrder
  //                                                       and BOM.SalesOrderItem = A.SalesOrderItem

{
  key A.Plant,
  key A.Product,
  key A.SalesOrder,
  key A.SalesOrderItem,
  key A.BillOfOperationsGroup   as OperationsGroup,
  key A.BillOfOperationsVariant as Counter


}
group by
  A.Plant,
  A.Product,
  A.SalesOrder,
  A.SalesOrderItem,
  A.BillOfOperationsGroup,
  A.BillOfOperationsVariant
