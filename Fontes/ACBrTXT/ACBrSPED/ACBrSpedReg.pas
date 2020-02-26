unit ACBrSpedReg;

interface

  procedure Register;

implementation

uses
  Classes, ACBrSpedContabil, ACBrSpedECF, ACBrSpedFiscal, ACBrSpedPisCofins;

{$IFNDEF FPC}
 {$R ACBr_SPEDContabil.dcr}
 {$R ACBr_SPEDECF.dcr}
 {$R ACBr_SPEDFiscal.dcr}
 {$R ACBr_SPEDPisCofins.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSPEDContabil, TACBrSPEDECF, TACBrSPEDFiscal, TACBrSPEDPisCofins]);
end;


end.
