@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZSO_GREY_STOCK'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZSO_GREY_STOCK
  as select from    I_MaterialStock_2        as A
    inner join      I_Product                as b on b.Product       = A.Material
                                                  and(
                                                    b.ProductType    = 'ZGR1'
                                                    or b.ProductType = 'ZGR2'
                                                    or b.ProductType = 'ZGRS'
                                                  )
    left outer join I_MRPAreaStorageLocation as c on(
      A.StorageLocation = c.MRPAreaStorageLocation
    )
{
  key A.SDDocument,
  key A.SDDocumentItem,
  key A.Material                           as Product,
      b.ProductType                        as ProductType,
      b._Text.ProductName                  as ProductDescription,

      A.MaterialBaseUnit                   as UOM,
      @Semantics.quantity.unitOfMeasure: 'UOM'
      sum(A.MatlWrhsStkQtyInMatlBaseUnit ) as StockQty
}

where
       A.MatlWrhsStkQtyInMatlBaseUnit > 0
  and  A.InventoryStockType           = '01'
  and  A.SDDocument                   is not initial
  and(
       c.MRPArea                      = '1100/C1'
    or c.MRPArea                      = '1100/C2'
    or c.MRPArea                      = '1100/E29'
  )
group by

  A.SDDocument,
  A.SDDocumentItem,
  A.MaterialBaseUnit,
  b.ProductType,
  b._Text.ProductName,
  A.Material
