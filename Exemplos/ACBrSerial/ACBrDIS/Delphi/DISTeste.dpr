program DISTeste;

uses
  Forms,
  DISTeste1 in 'DISTeste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
