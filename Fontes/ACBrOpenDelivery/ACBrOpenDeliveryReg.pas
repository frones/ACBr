{$I ACBr.inc}

unit ACBrOpenDeliveryReg;

interface

uses
  Classes,
  {$IFNDEF FPC}
  DesignIntf,
  DesignEditors,
  {$ENDIF}
  ACBrOpenDelivery;

{$IFNDEF FPC}
type
  TACBrOpenDeliveryEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ACBrOpenDelivery', [TACBrOpenDelivery]);

  {$IFNDEF FPC}
  RegisterSelectionEditor(TACBrOpenDelivery, TACBrOpenDeliveryEditor);
  {$ENDIF}
end;

{$IFNDEF FPC}
{ TACBrOpenDeliveryEditor }

procedure TACBrOpenDeliveryEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('ACBrBase');
  Proc('ACBrOpenDelivery');
  Proc('ACBrOpenDeliverySchemaClasses');
  Proc('pcnConversaoOD');
end;
{$ENDIF}

end.
