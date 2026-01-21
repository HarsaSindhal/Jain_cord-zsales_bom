@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZLOT_DATA'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZLOT_DATA
  as select from    zbeam_creat_tab as A
    inner join ZMATDOC_DATA    as b on  b.MaterialDocument     = A.mat_doc311
                                         and b.MaterialDocumentYear = A.mat_doc_year
{
      //  key A.material,
  key A.plant,
  key A.salesdoc,
  key A.soitem,
      //      sum(A.beamlength ) as TotQty
      //      sum(A.totalqty ) as TotQty
      sum(A.quantity ) as TotQty
}

group by
//  A.material,
  A.plant,
  A.salesdoc,
  A.soitem
