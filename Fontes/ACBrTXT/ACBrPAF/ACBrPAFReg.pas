unit ACBrPAFReg;

{$I ACBr.inc}

interface

  procedure Register;

implementation

uses
  Classes, ACBrPAF;

{$IFNDEF FPC}
 {$R ACBrPAF.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrPAF]);
end;


end.
