unit FMX.OleConst;

interface

resourcestring
  SBadPropValue = '''%s'' is not a valid property value';
  SCannotActivate = 'OLE control activation failed';
  SNoWindowHandle = 'Could not obtain OLE control window handle';
  SOleError = 'OLE error %.8x';
  SVarNotObject = 'Variant does not reference an OLE object';
  SVarNotAutoObject = 'Variant does not reference an automation object';
  SNoMethod = 'Method ''%s'' not supported by OLE object';
  SLinkProperties = 'Link Properties';
  SInvalidLinkSource = 'Cannot link to an invalid source.';
  SCannotBreakLink = 'Break link operation is not supported.';
  SLinkedObject = 'Linked %s';
  SEmptyContainer = 'Operation not allowed on an empty OLE container';
  SInvalidVerb = 'Invalid object verb';
  SPropDlgCaption = '%s Properties';
  SInvalidStreamFormat = 'Invalid stream format';
  SInvalidLicense = 'License information for %s is invalid';
  SNotLicensed = 'License information for %s not found. You cannot use this control in design mode';
  sNoRunningObject = 'Unable to retrieve a pointer to a running object registered with OLE for %s/%s';

implementation

end.
