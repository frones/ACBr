program SPEDECF;

uses
  Forms,
  Frm_SPEDECF in 'Frm_SPEDECF.pas' {FrmSPEDECF};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmSPEDECF, FrmSPEDECF);
  Application.Run;
end.
