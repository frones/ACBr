program Demo_ACBrBPe;

uses
  Forms,
  Frm_Demo_ACBrBPe in 'Frm_Demo_ACBrBPe.pas' {frm_DemoACBrBPe},
  ConfiguraSerial in 'configuraserial.pas' {frConfiguraSerial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_DemoACBrBPe, frm_DemoACBrBPe);
  Application.CreateForm(TfrConfiguraSerial, frConfiguraSerial);
  Application.Run;
end.
