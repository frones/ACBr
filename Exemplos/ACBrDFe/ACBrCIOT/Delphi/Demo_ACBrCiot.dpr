program Demo_ACBrCiot;

uses
  Forms,
  ufrmStatus in 'ufrmStatus.pas' {frmStatus},
  Frm_Demo_ACBrCiot in 'Frm_Demo_ACBrCiot.pas' {frmDemo_ACBrCIOT};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrCIOT, frmDemo_ACBrCIOT);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
