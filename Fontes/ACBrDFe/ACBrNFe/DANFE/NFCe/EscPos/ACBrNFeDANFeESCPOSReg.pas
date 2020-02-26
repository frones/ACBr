unit ACBrNFeDANFeESCPOSReg;

{$I ACBr.inc}

interface

  procedure Register;

implementation

uses
  Classes, ACBrNFeDANFeESCPOS;

procedure Register;
begin
  RegisterComponents('ACBrNFe', [TACBrNFeDANFeESCPOS]);
end;

end.
