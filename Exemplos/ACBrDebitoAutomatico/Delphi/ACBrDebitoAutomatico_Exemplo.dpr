program ACBrDebitoAutomatico_Exemplo;

uses
  Forms,
  Frm_ACBrDebitoAutomatico_Exemplo in 'Frm_ACBrDebitoAutomatico_Exemplo.pas' {frmACBrDebitoAutomatico_Exemplo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrDebitoAutomatico_Exemplo, frmACBrDebitoAutomatico_Exemplo);
  Application.Run;
end.
