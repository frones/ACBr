program DemoFeriados;

uses
  Forms,
  Frm_Feriados in 'Frm_Feriados.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
