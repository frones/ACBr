program ACBrEDIExemplo;

uses
  Forms,
  Frm_ACBrEDI in 'Frm_ACBrEDI.pas' {frmACBrEDI};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmACBrEDI, frmACBrEDI);
  Application.Run;
end.
