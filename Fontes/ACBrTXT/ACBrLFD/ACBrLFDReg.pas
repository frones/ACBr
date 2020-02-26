unit ACBrLFDReg;

{$I ACBr.inc}

interface

uses
  Classes;

procedure Register;

implementation

uses
  ACBrLFD;

{$IFNDEF FPC}
 {$R ACBrLFD.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrLFD]);
end;


end.
