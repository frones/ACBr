program ACBrDebitoAutomatico_Exemplo;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  Frm_ACBrDebitoAutomatico_Exemplo in 'Frm_ACBrDebitoAutomatico_Exemplo.pas' {frmACBrDebitoAutomatico_Exemplo};

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrDebitoAutomatico_Exemplo, frmACBrDebitoAutomatico_Exemplo);
  Application.Run;
end.
