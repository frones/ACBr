{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ACBR_DeSTDA;

interface

uses
  ACBrDeSTDA, ACBrDeSTDA4715, ACBrDeSTDABloco_0, ACBrDeSTDABloco_0_Class, 
  ACBrDeSTDABloco_9, ACBrDeSTDABloco_9_Class, ACBrDeSTDABloco_G, 
  ACBrDeSTDABloco_G_Class, ACBrDeSTDABlocos, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ACBrDeSTDA', @ACBrDeSTDA.Register);
end;

initialization
  RegisterPackage('ACBR_DeSTDA', @Register);
end.
