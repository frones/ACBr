{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PagFor;


interface

uses
  ACBrPagForReg, ACBrPagFor, ACBrPagForArquivo, ACBrPagForArquivoClass, 
  ACBrPagForClass, ACBrPagForConfiguracoes, ACBrPagForConversao, 
  ACBrPagForGravarTxt, ACBrPagForLerTxt, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPagForReg', @ACBrPagForReg.Register);
end;

initialization
  RegisterPackage('ACBr_PagFor', @Register);
end.
