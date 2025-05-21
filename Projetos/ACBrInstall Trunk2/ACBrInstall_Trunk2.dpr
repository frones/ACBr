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
  SysUtils,
  Dialogs,
  Windows;

{$R *.res}

{$SETPEOSVERSION 5.0}
{$SETPESUBSYSVERSION 5.0}

var
  hMutex: THandle;
  LastError: DWORD;

const
  MUTEX_NAME = 'ACBrInstallTrunkII';

begin
  hMutex := CreateMutex(nil, False, PChar(MUTEX_NAME));
  LastError := GetLastError;

  if (hMutex = 0) or (hMutex = INVALID_HANDLE_VALUE) then
  begin
    MessageDlg('Erro ao criar mutex. Código: ' + IntToStr(GetLastError), mtError, [mbOK], 0);
    Halt(1);
  end;

  if LastError = ERROR_ALREADY_EXISTS then
  begin
    MessageDlg('O ACBrInstall já está aberto. Verifique se ele está minimizado ou em segundo plano.', mtError, [mbOK], 0);
    CloseHandle(hMutex);
    Halt(0);
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Instalação do Projeto ACBr';
  Application.CreateForm(TfrmPrincipal, frmPrincipal);
  Application.Run;
end.
