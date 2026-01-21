@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZSO_DATA'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSO_DATA
  as select from    I_MaterialDocumentItem_2 as A
    left outer join I_ManufacturingOrder     as B on(
      B.ManufacturingOrder = A.ManufacturingOrder
    )
{
  key A.Material,
  key A.Plant,
      B.SalesOrder,
      B.SalesOrderItem,

      A.MaterialBaseUnit,
      cast( sum( A.QuantityInBaseUnit ) as abap.dec( 16, 3 )) as Qty



}
where
      A.GoodsMovementIsCancelled <> 'X'
  and A.GoodsMovementType        = '101'


group by
  A.Material,
  A.Plant,
  B.SalesOrder,
  B.SalesOrderItem,

  A.MaterialBaseUnit
