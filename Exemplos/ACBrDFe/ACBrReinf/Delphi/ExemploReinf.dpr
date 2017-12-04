program ExemploReinf;

uses
  Forms,
  uExemploReinf in 'uExemploReinf.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
