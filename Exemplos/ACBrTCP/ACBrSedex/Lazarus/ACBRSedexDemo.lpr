program ACBRSedexDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  frmCalculoSedex in 'frmCalculoSedex.pas' {Form1};

{.$R *.res}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
