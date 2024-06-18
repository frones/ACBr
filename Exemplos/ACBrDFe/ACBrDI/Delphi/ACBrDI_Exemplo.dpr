program ACBrDI_Exemplo;

uses
  Forms,
  Frm_ACBrDI in 'Frm_ACBrDI.pas' {frmACBrDI};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmACBrDI, frmACBrDI);
  Application.Run;
end.
