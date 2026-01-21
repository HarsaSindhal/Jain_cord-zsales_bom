@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZROUTING_GROUP_DATA'
@Metadata.ignorePropagatedAnnotations: true
define root view entity Zrouting_Group_Data
  as select from I_MfgBillOfOperationsChgSt as a
{

  key a.BillOfOperationsGroup     as OperationsGroup,
  key a.BOOInternalVersionCounter as GroupCounter,
  key a.BillOfOperationsDesc      as Description
}
where
  a.IsDeleted <> 'X'

group by
  a.BillOfOperationsGroup,
  a.BOOInternalVersionCounter,
  a.BillOfOperationsDesc
