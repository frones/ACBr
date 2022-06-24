program ExemploACBrOFX;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uExemploACBrOFX in 'uExemploACBrOFX.pas';

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFExemploACBrOFX, FExemploACBrOFX);
  Application.Run;
end.
