program ExemploEsocial;

uses
  Forms,
  uExemploEsocial in 'uExemploEsocial.pas' {FExemploEsocial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFExemploEsocial, FExemploEsocial);
  Application.Run;
end.
