program ConsultaCNPJ;

uses
  System.StartUpCopy,
  FMX.Forms,
  UConsultaCNPJ in 'UConsultaCNPJ.pas' {F_ConsultaCNPJ};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TF_ConsultaCNPJ, F_ConsultaCNPJ);
  Application.Run;
end.
