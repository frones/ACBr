program Demo_ACBrCTe;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_Demo_ACBrCTe in 'Frm_Demo_ACBrCTe.pas' {frmDemo_ACBrCTe};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrCTe, frmDemo_ACBrCTe);
  Application.Run;
end.
