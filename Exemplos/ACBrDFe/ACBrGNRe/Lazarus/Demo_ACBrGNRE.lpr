program Demo_ACBrGNRE;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ufrmStatus in 'ufrmStatus.pas' {frmStatus},
  Frm_Demo_ACBrGNRE in 'Frm_Demo_ACBrGNRE.pas' {frmDemo_ACBrGNRE};

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo_ACBrGNRE, frmDemo_ACBrGNRE);
  Application.Run;
end.
