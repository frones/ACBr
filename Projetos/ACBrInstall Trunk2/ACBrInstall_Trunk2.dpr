program ACBrInstall_Trunk2;

uses
  Forms,
  uPrincipal in 'uPrincipal.pas' {frmPrincipal},
  uFrameLista in 'uFrameLista.pas' {framePacotes: TFrame},
  ACBrInstallDelphiComponentes in 'ACBrInstallDelphiComponentes.pas',
  ACBrInstallUtils in 'ACBrInstallUtils.pas',
  ACBrPacotes in 'ACBrPacotes.pas',
  ACBrUtil.Strings in '..\..\Fontes\ACBrComum\ACBrUtil.Strings.pas',
  ACBrUtil.FilesIO in '..\..\Fontes\ACBrComum\ACBrUtil.FilesIO.pas',
  UACBrPlataformaInstalacaoAlvo in 'UACBrPlataformaInstalacaoAlvo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instalação do Projeto ACBr';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
