program ExemploACBrOFX;

uses
  Forms,
  uExemploACBrOFX in 'uExemploACBrOFX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFExemploACBrOFX, FExemploACBrOFX);
  Application.Run;
end.
