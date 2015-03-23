program SPEDFCont;

uses
  Forms,
  Frm_SPEDFCont in 'Frm_SPEDFCont.pas' {FrmSPEDFCont};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmSPEDFCont, FrmSPEDFCont);
  Application.Run;
end.
