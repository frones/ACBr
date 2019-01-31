program Demo_ACBrANe;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_Demo_ACBrANe in 'Frm_Demo_ACBrANe.pas' {frmDemo_ACBrANe},
  ufrmStatus in 'ufrmStatus.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrANe, frmDemo_ACBrANe);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
