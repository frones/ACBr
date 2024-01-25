{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PagFor;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPagForReg, ACBrPagForArquivo, ACBrPagForArquivoClass, 
  PagFor.BancoCECRED.GravarTxtRemessa, PagFor.BancoCECRED.LerTxtRetorno, 
  PagFor.BancoCECRED.Provider, PagFor.BancodoBrasil.GravarTxtRemessa, 
  PagFor.BancodoBrasil.LerTxtRetorno, PagFor.BancodoBrasil.Provider, 
  PagFor.Bradesco.GravarTxtRemessa, PagFor.Bradesco.LerTxtRetorno, 
  PagFor.Bradesco.Provider, PagFor.HSBC.GravarTxtRemessa, 
  PagFor.HSBC.LerTxtRetorno, PagFor.HSBC.Provider, 
  PagFor.Itau.GravarTxtRemessa, PagFor.Itau.LerTxtRetorno, 
  PagFor.Itau.Provider, PagFor.Safra.GravarTxtRemessa, 
  PagFor.Safra.LerTxtRetorno, PagFor.Safra.Provider, 
  PagFor.Santander.GravarTxtRemessa, PagFor.Santander.LerTxtRetorno, 
  PagFor.Santander.Provider, PagFor.Sicredi.GravarTxtRemessa, 
  PagFor.Sicredi.LerTxtRetorno, PagFor.Sicredi.Provider, ACBrPagForClass, 
  ACBrPagForConversao, ACBrPagForParametros, ACBrPagForGravarTxt, 
  ACBrPagForInterface, ACBrPagForLerTxt, ACBrPagForProviderBase, 
  ACBrPagForProviderManager, CNAB240.GravarTxtRemessa, CNAB240.LerTxtRetorno, 
  CNAB240.Provider, PagFor.Modelo.GravarTxtRemessa, 
  PagFor.Modelo.LerTxtRetorno, PagFor.Modelo.Provider, 
  PagFor.Banrisul.GravarTxtRemessa, PagFor.Banrisul.LerTxtRetorno, 
  PagFor.Banrisul.Provider, PagFor.UnicredCooperativa.GravarTxtRemessa, 
  PagFor.UnicredCooperativa.LerTxtRetorno, PagFor.UnicredCooperativa.Provider, 
  PagFor.Inter.GravarTxtRemessa, PagFor.Inter.LerTxtRetorno, PagFor.Inter.Provider, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPagForReg', @ACBrPagForReg.Register);
end;

initialization
  RegisterPackage('ACBr_PagFor', @Register);
end.
