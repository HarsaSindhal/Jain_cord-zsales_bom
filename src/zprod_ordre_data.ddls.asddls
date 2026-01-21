@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZPROD_ORDRE_DATA'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZPROD_ORDRE_DATA
  as select from I_ManufacturingOrder as A
{
  key A.SalesOrder,
  key A.SalesOrderItem,
//     a.
      cast(sum(A.MfgOrderPlannedTotalQty ) as abap.dec( 16, 3 )) as Qty

} where A.IsMarkedForDeletion <> 'X'
 group by 
 A.SalesOrder,
  A.SalesOrderItem
