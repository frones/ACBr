program Project1;

uses
 FMX.Forms,
  CEPTeste1 in 'CEPTeste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
