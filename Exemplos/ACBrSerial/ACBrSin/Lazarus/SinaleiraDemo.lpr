program SinaleiraDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  FSinaleiraDemo in 'FSinaleiraDemo.pas' {FormSinaleiraDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSinaleiraDemo, FormSinaleiraDemo);
  Application.Run;
end.
