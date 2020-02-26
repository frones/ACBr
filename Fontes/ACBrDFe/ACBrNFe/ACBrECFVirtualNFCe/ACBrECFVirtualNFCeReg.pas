unit ACBrECFVirtualNFCeReg;

{$I ACBr.inc}

interface

procedure Register;

implementation

uses
  Classes, ACBrECFVirtualNFCe;

procedure Register;
begin
  RegisterComponents('ACBrNFe', [TACBrECFVirtualNFCe]);
end;

end.
