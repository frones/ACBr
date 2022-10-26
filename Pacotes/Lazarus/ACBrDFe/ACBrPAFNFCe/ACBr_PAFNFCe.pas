{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_PAFNFCe;

{$warn 5023 off : no warning about unused units}
interface

uses
  ACBrPAFNFCe, ACBrPAFNFCe_Comum, ACBrPAFNFCe_Reg, ACBrPAFNFCe_MenuFiscal, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrPAFNFCe_Reg', @ACBrPAFNFCe_Reg.Register);
end;

initialization
  RegisterPackage('ACBr_PAFNFCe', @Register);
end.
