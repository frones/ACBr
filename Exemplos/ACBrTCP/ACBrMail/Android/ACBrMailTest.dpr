program ACBrMailTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  ACBrMailTestFr in 'ACBrMailTestFr.pas' {ACBrMailTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TACBrMailTestForm, ACBrMailTestForm);
  Application.Run;
end.
