program ACBrInstall_Trunk2;

uses
  Forms,
  ACBr.Principal in 'ACBr.Principal.pas' {frmPrincipal},
  ACBr.FrameLista in 'ACBr.FrameLista.pas' {framePacotes: TFrame},
  ACBr.InstallDelphiComponentes in 'ACBr.InstallDelphiComponentes.pas',
  ACBr.InstallUtils in 'ACBr.InstallUtils.pas',
  ACBr.Pacotes in 'ACBr.Pacotes.pas',
  ACBrUtil.Strings in '..\..\Fontes\ACBrComum\ACBrUtil.Strings.pas',
  ACBrUtil.FilesIO in '..\..\Fontes\ACBrComum\ACBrUtil.FilesIO.pas',
  ACBr.PlataformaInstalacaoAlvo in 'ACBr.PlataformaInstalacaoAlvo.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

{$SETPEOSVERSION 5.0}
{$SETPESUBSYSVERSION 5.0}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instalação do Projeto ACBr';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
