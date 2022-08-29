unit ACBrOpenDeliveryEvents;

interface

uses
  ACBrOpenDeliverySchemaClasses;

type
  TOnEventStatus = procedure(AEvent: TACBrOpenDeliverySchemaEvent;
    var Ack: Boolean) of object;

  TOnEventOrder = procedure(AEvent: TACBrOpenDeliverySchemaEvent;
    AOrder: TACBrOpenDeliverySchemaOrder; var Ack: Boolean) of object;

  TOnPollingEnd = procedure(AEndPolling: TDateTime;
    AEvents: TACBrOpenDeliverySchemaEventCollection) of object;

  TOnTokenGet = procedure(AClientId: string; var AToken: string; var AExpiresAt: TDateTime) of object;
  TOnTokenSave = procedure(AClientId, AToken: string; AExpiresAt: TDateTime) of object;

implementation

end.
