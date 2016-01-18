program Demo_ACBrGNRE;

uses
  Forms,
  ufrmStatus in 'ufrmStatus.pas' {frmStatus},
  Frm_Demo_ACBrGNRE in 'Frm_Demo_ACBrGNRE.pas' {frmDemo_ACBrGNRE};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrGNRE, frmDemo_ACBrGNRE);
  Application.Run;
end.
