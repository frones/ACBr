program DemoPrinter;

uses
  System.StartUpCopy,
  FMX.Forms,
  uDemoPrinter in 'uDemoPrinter.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
