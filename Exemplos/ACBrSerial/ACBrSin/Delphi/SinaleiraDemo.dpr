program SinaleiraDemo;

uses
  Forms,
  FSinaleiraDemo in 'FSinaleiraDemo.pas' {FormSinaleiraDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSinaleiraDemo, FormSinaleiraDemo);
  Application.Run;
end.
