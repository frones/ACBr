program Demo_ACBrMDFe;

uses
  Forms,
  Frm_Demo_ACBrMDFe in 'Frm_Demo_ACBrMDFe.pas' {frmDemo_ACBrMDFe},
  ufrmStatus in 'ufrmStatus.pas' {frmStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrMDFe, frmDemo_ACBrMDFe);
  Application.CreateForm(TfrmStatus, frmStatus);
  Application.Run;
end.
