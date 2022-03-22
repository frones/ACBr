unit ACBrOFXReg;

{$I ACBr.inc}

interface

  procedure Register;

implementation

uses
  Classes, ACBrOFX;

{$IFNDEF FPC}
 {$R ACBrOFX.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrOFX]);
end;


end.
