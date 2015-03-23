program LCBECFTeste;

uses
  Forms,
  LCBECFTeste1 in 'LCBECFTeste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
