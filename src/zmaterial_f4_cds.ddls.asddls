@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Material F4'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zmaterial_f4_cds
  as select from    I_Product              as a
    left outer join I_ProductDescription_2 as b on(
      b.Product      = a.Product
      and b.Language = 'E'
    )
{
@Search.defaultSearchElement: true

  key  a.Product,
       a.BaseUnit as UOM,
       a.ProductType,
       b.ProductDescription
}


where
     a.ProductType = 'ZGR1'
  or a.ProductType = 'ZGR2'
  or a.ProductType = 'ZCHA'
 or a.ProductType = 'ZGRS'
 or a.ProductType = 'ZFG1'
or a.ProductType = 'ZFG2'
or a.ProductType = 'ZSFG'
