program ACBrSMSDemo;

uses
  Forms,
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uListaMensagem in 'uListaMensagem.pas' {frmListaMensagem},
  uTrocarBandeja in 'uTrocarBandeja.pas' {frmTrocarBandeja},
  uEnviarMensagem in 'uEnviarMensagem.pas' {frmEnviarMensagem},
  uEnvioLote in 'uEnvioLote.pas' {frmEnvioLote};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Demo ACBrSMS';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
