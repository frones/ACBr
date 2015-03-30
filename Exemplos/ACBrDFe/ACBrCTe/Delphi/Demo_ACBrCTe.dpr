program Demo_ACBrCTe;

uses
  Forms,
  Frm_Demo_ACBrCTe in 'Frm_Demo_ACBrCTe.pas' {frmDemo_ACBrCTe};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrCTe, frmDemo_ACBrCTe);
  Application.Run;
end.
