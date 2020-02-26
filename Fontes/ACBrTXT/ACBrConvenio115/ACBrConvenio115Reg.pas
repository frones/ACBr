unit ACBrConvenio115Reg;

{$I ACBr.inc}

interface

uses
  Classes, ACBrConvenio115;

{$IFNDEF FPC}
  {$R ACBrConvenio115.dcr}
{$EndIf}
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrConvenio115]);
end;


end.
