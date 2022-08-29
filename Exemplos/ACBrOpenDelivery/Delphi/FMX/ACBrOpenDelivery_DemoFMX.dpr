program ACBrOpenDelivery_DemoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {FMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.
