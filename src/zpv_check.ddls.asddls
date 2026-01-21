@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZPV_CHECK'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZPV_CHECK
  as select from    I_ProductionVersion         as A
    left outer join I_MfgBOOMaterialAssgmtChgSt as Routing on  Routing.BillOfOperationsGroup = A.BillOfOperationsGroup
                                                           and Routing.Plant                 = A.Plant
                                                           and Routing.Product                 = A.Material
{
  key A.Plant,
  key A.ProductionVersionGroup,
  key Routing.SalesOrder,
  key Routing.SalesOrderItem,
  key A.Material
}
group by
  A.Plant,
  A.ProductionVersionGroup,
  Routing.SalesOrder,
  Routing.SalesOrderItem,
  A.Material
