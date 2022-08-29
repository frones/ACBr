unit ACBrOpenDeliveryException;

interface

uses
  ACBrBase,
  Classes,
  SysUtils;

type
  EACBrOpenDeliveryException = class(EACBrException)
  public
    constructor Create(const AMsg: string);
    constructor CreateDef(const AMsg: string; ADummy: Integer = 0);
  end;

  EACBrOpenDeliveryHTTPException = class(EACBrOpenDeliveryException)
  private
    FStatus: Integer;
    FTitle: string;
  public
    constructor Create(const AStatus: Integer; const ATitle: string); reintroduce;

    property Status: Integer read FStatus;
    property Title: string read FTitle;
  end;

implementation

uses
  ACBrUtil.Strings;

{ EACBrOpenDeliveryException }

constructor EACBrOpenDeliveryException.Create(const AMsg: string);
begin
  inherited Create(ACBrStr(AMsg));
end;

constructor EACBrOpenDeliveryException.CreateDef(const AMsg: string; ADummy: Integer);
begin
  inherited Create(AMsg);
end;

{ EACBrOpenDeliveryHTTPException }

constructor EACBrOpenDeliveryHTTPException.Create(const AStatus: Integer; const ATitle: string);
var
  LMsg: string;
begin
  LMsg := Format('%d: %s', [AStatus, ATitle]);
  inherited Create(LMsg);
  FStatus := AStatus;
  FTitle := ATitle;
end;

end.

