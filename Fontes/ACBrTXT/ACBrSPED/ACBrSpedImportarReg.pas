unit ACBrSpedImportarReg;

{$I ACBr.inc}

interface

procedure Register;

implementation

Uses
  Classes, ACBrEPCImportar, ACBrEFDImportar;

{$IFNDEF FPC}
 {$R ACBrSPEDPisCofinsImportar.dcr}
 {$R ACBrSPEDFiscalImportar.dcr}
{$ENDIF}

procedure Register;
begin
  RegisterComponents('ACBrTXT', [TACBrSpedPCImportar, TACBrSpedFiscalImportar]);
end;


end.
