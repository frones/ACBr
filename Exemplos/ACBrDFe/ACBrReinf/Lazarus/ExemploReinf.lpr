program ExemploReinf;
{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  uExemploReinf ;

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
