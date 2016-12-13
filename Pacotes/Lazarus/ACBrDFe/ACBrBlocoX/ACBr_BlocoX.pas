{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBr_BlocoX;

interface

uses
  ACBrBlocoX, ACBrBlocoX_Reg, ACBrBlocoX_Comum, ACBrBlocoX_Estoque, 
  ACBrBlocoX_ReducaoZ, pcnRetEnvBlocoX, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrBlocoX_Reg', @ACBrBlocoX_Reg.Register);
end;

initialization
  RegisterPackage('ACBr_BlocoX', @Register);
end.
