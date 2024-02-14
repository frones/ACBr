{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_NF3e;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrNF3e, ACBrNF3eReg, ACBrNF3eNotasFiscais, ACBrNF3eConfiguracoes, 
  ACBrNF3eWebServices, ACBrNF3eDANF3eClass, ACBrNF3eConsSit, 
  ACBrNF3eEnvEvento, ACBrNF3eEventoClass, ACBrNF3eClass, ACBrNF3eXmlReader, 
  ACBrNF3eXmlWriter, ACBrNF3eRetConsSit, ACBrNF3eRetEnvEvento, 
  ACBrNF3eConversao, ACBrNF3eConsts, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrNF3eReg', @ACBrNF3eReg.Register);
end;

initialization
  RegisterPackage('ACBr_NF3e', @Register);
end.
