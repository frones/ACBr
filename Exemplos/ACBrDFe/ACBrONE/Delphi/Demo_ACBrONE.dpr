program Demo_ACBrONE;

uses
  Forms,
  Frm_Demo_ACBrONE in 'Frm_Demo_ACBrONE.pas' {frm_DemoACBrONE},
  ConfiguraSerial in 'configuraserial.pas' {frConfiguraSerial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tfrm_DemoACBrONE, frm_DemoACBrONE);
  Application.CreateForm(TfrConfiguraSerial, frConfiguraSerial);
  Application.Run;
end.
