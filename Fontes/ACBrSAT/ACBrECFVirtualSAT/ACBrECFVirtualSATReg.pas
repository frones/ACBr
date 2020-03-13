unit ACBrECFVirtualSATReg;

interface

  procedure Register;

implementation

Uses
  Classes, ACBrECFVirtualSAT;

procedure Register;
begin
  RegisterComponents('ACBrSAT', [TACBrECFVirtualSAT]);
end;

end.
