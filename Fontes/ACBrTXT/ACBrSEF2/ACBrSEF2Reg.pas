unit ACBrSEF2Reg;

{$I ACBr.inc}

interface

  procedure Register;

implementation

uses
  Classes, ACBrSEF2;

{$IFNDEF FPC}
 {$R ACBrSEF2.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSEF2]);
end;


end.
