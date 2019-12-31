{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_EDI;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrEDI, ACBrEDIClass, ACBrEDICobranca, ACBrEDIConhectos, ACBrEDINotaFiscal, 
  ACBrEDIOcorrencia, pediConversao, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrEDI', @ACBrEDI.Register);
end;

initialization
  RegisterPackage('ACBr_EDI', @Register);
end.
