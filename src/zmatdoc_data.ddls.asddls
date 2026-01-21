@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZMATDOC_DATA'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZMATDOC_DATA
  as select from I_MaterialDocumentItem_2 as A
{
  key A.MaterialDocument,
  key A.MaterialDocumentYear
}
where
      A.GoodsMovementIsCancelled <> 'X'
  and A.ReversedMaterialDocument =  ''

group by
  A.MaterialDocument,
  A.MaterialDocumentYear
