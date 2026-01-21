@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZLAST_PV_F4'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZLAST_PV_F4
  as select from I_ProductionVersion as A
{
  key A.Material,
      A.Plant,
      max(A.ProductionVersion )  as PV

}
group by
  A.Material,
  A.Plant
