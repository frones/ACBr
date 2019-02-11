{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_Boleto;

interface

uses
  ACBrBoleto, ACBrBancoBradesco, ACBrBancoBrasil, ACBrBancoItau, 
  ACBrBancoSicredi, ACBrBancoMercantil, ACBrBancoBanrisul, ACBrBancoSantander, 
  ACBrBancoBancoob, ACBrBancoHSBC, ACBrBancoNordeste, ACBrBancoBRB, 
  ACBrBancoBic, ACBrBancoBanestes, ACBrBancoCecred, ACBrBancoCaixa, 
  ACBrBancoCaixaSICOB, ACBrBancoBrasilSicoob, ACBrBancoCitiBank, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBoleto', @ACBrBoleto.Register);
end;

initialization
  RegisterPackage('ACBr_Boleto', @Register);
end.
