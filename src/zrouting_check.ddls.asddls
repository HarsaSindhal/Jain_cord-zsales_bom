@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZROUTING_CHECK'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZROUTING_CHECK
  as select from I_MfgBOOMaterialAssgmtChgSt as A
{
  key A.Plant,
  key A.Product,
  key A.SalesOrder,
  key A.SalesOrderItem
}
where
  A.IsDeleted <> 'X'
