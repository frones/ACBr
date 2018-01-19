program ExemploReinf;

uses
  Forms,
  uExemploReinf in 'uExemploReinf.pas' {Form2},
  Unit2 in 'unit2.pas' {frSelecionarCertificado};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
