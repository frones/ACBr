{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit uPrincipal;

interface

uses
  JclIDEUtils, JclCompilerUtils, ACBrUtil,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, pngimage, ShlObj,
  uFrameLista, IOUtils,
  Types, JvComponentBase, JvCreateProcess, JvExControls, JvAnimatedImage,
  JvGIFCtrl, JvWizard, JvWizardRouteMapNodes, CheckLst;

type
  TDestino = (tdSystem, tdDelphi, tdNone);

  TfrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizMapa: TJvWizardRouteMapNodes;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizPgObterFontes: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    wizPgFinalizar: TJvWizardInteriorPage;
    wizPgInicio: TJvWizardWelcomePage;
    Label4: TLabel;
    Label5: TLabel;
    edtDelphiVersion: TComboBox;
    edtPlatform: TComboBox;
    Label2: TLabel;
    edtDirDestino: TEdit;
    Label6: TLabel;
    Label1: TLabel;
    edtURL: TEdit;
    imgLogomarca: TImage;
    lblInfoObterFontes: TLabel;
    lstMsgInstalacao: TListBox;
    pnlTopo: TPanel;
    Label9: TLabel;
    btnSelecDirInstall: TSpeedButton;
    imgGifPropagandaACBrSAC: TJvGIFAnimator;
    Label3: TLabel;
    pgbInstalacao: TProgressBar;
    lblUrlACBrSac1: TLabel;
    lblUrlForum1: TLabel;
    lblUrlACBr1: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label7: TLabel;
    btnSVNCheckoutUpdate: TSpeedButton;
    btnInstalarACBr: TSpeedButton;
    ckbFecharTortoise: TCheckBox;
    btnVisualizarLogCompilacao: TSpeedButton;
    pnlInfoCompilador: TPanel;
    wizPgPacotes: TJvWizardInteriorPage;
    rdgDLL: TRadioGroup;
    ckbCopiarTodasDll: TCheckBox;
    ckbBCB: TCheckBox;
    lbInfo: TListBox;
    Label8: TLabel;
    chkDeixarSomenteLIB: TCheckBox;
    btnWCInfo: TButton;
    ckbRemoverArquivosAntigos: TCheckBox;
    JvCreateProcess1: TJvCreateProcess;
    Label22: TLabel;
    clbDelphiVersion: TCheckListBox;
    Label23: TLabel;
    ckbRemoveOpenSSL: TCheckBox;
    ckbRemoveCapicom: TCheckBox;
    ckbCargaDllTardia: TCheckBox;
    ckbRemoverCastWarnings: TCheckBox;
    ckbUsarArquivoConfig: TCheckBox;
    framePacotes1: TframePacotes;
    ckbRemoverXMLSec: TCheckBox;
    procedure imgPropaganda1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtDelphiVersionChange(Sender: TObject);
    procedure wizPgInicioNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure URLClick(Sender: TObject);
    procedure btnSelecDirInstallClick(Sender: TObject);
    procedure wizPrincipalCancelButtonClick(Sender: TObject);
    procedure wizPrincipalFinishButtonClick(Sender: TObject);
    procedure wizPgConfiguracaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnSVNCheckoutUpdateClick(Sender: TObject);
    procedure wizPgObterFontesEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure btnInstalarACBrClick(Sender: TObject);
    procedure wizPgObterFontesNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure wizPgInstalacaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnVisualizarLogCompilacaoClick(Sender: TObject);
    procedure wizPgInstalacaoEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure rdgDLLClick(Sender: TObject);
    procedure btnWCInfoClick(Sender: TObject);
    procedure clbDelphiVersionClick(Sender: TObject);
  private
    FCountErros: Integer;
    oACBr: TJclBorRADToolInstallations;
    iVersion: Integer;
    tPlatform: TJclBDSPlatform;
    sDirRoot: string;
    sDirLibrary: string;
    sDirPackage: string;
    sDestino   : TDestino;
    sPathBin   : String;
    FPacoteAtual: TFileName;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure AddLibrarySearchPath;
    procedure OutputCallLine(const Text: string);
    procedure SetPlatformSelected;
    function IsCheckOutJaFeito(const ADiretorio: String): Boolean;
    procedure CreateDirectoryLibrarysNotExist;
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog: String;
    procedure InstalarCapicom;
    procedure InstalarOpenSSL;
    procedure InstalarXMLSec;
    procedure InstalarDiversos;
    function PathSystem: String;
    function RegistrarActiveXServer(const AServerLocation: string;
      const ARegister: Boolean): Boolean;
    procedure CopiarArquivoTo(ADestino : TDestino; const ANomeArquivo: String);
    procedure ExtrairDiretorioPacote(NomePacote: string);
    procedure AddLibraryPathToDelphiPath(const APath, AProcurarRemover: String);
    procedure FindDirs(ADirRoot: String; bAdicionar: Boolean = True);
    procedure DeixarSomenteLib;
    procedure RemoverArquivosAntigosDoDisco;
    procedure RemoverDiretoriosEPacotesAntigos;
    function RunAsAdminAndWaitForCompletion(hWnd: HWND; filename: string): Boolean;
    procedure GetDriveLetters(AList: TStrings);
    procedure MostraDadosVersao;
    function GetPathACBrInc: TFileName;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  SVN_Class, FileCtrl, ShellApi, IniFiles, StrUtils, Math, Registry;

{$R *.dfm}

function TfrmPrincipal.RunAsAdminAndWaitForCompletion(hWnd: HWND; filename: string): Boolean;
{
    See Step 3: Redesign for UAC Compatibility (UAC)
    http://msdn.microsoft.com/en-us/library/bb756922.aspx
}
var
  sei: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  ZeroMemory(@sei, SizeOf(sei));
  sei.cbSize       := SizeOf(TShellExecuteInfo);
  sei.Wnd          := hwnd;
  sei.fMask        := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb       := PWideChar('runas');
  sei.lpFile       := PWideChar(Filename);
  sei.lpParameters := PWideChar('');
  sei.nShow        := SW_HIDE;

  if ShellExecuteEx(@sei) then
  begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(sei.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or  Application.Terminated;
  end;
end;

procedure TfrmPrincipal.ExtrairDiretorioPacote(NomePacote: string);

  procedure FindDirPackage(sDir, sPacote: String);
  var
    oDirList: TSearchRec;
//    iRet: Integer;
//    sDirDpk: string;
  begin
    sDir := IncludeTrailingPathDelimiter(sDir);
    if not DirectoryExists(sDir) then
      Exit;

    if SysUtils.FindFirst(sDir + '*.*', faAnyFile, oDirList) = 0 then
    begin
      try
        repeat

          if (oDirList.Name = '.')  or (oDirList.Name = '..') or (oDirList.Name = '__history')
          or (oDirList.Name = '__recovery')then
            Continue;

          //if oDirList.Attr = faDirectory then
          if DirectoryExists(sDir + oDirList.Name) then
            FindDirPackage(sDir + oDirList.Name, sPacote)
          else
          begin
            if UpperCase(oDirList.Name) = UpperCase(sPacote) then
              sDirPackage := IncludeTrailingPathDelimiter(sDir);
          end;

        until SysUtils.FindNext(oDirList) <> 0;
      finally
        SysUtils.FindClose(oDirList);
      end;
    end;
  end;

begin
   sDirPackage := '';
   FindDirPackage(IncludeTrailingPathDelimiter(sDirRoot) + 'Pacotes\Delphi', NomePacote);
end;

// retornar o path do aplicativo
function TfrmPrincipal.PathApp: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

// retornar o caminho completo para o arquivo .ini de configurações
function TfrmPrincipal.PathArquivoIni: String;
var
  NomeApp: String;
begin
  NomeApp := ExtractFileName(ParamStr(0));
  Result := PathApp + ChangeFileExt(NomeApp, '.ini');
end;

// retornar o caminho completo para o arquivo de logs
function TfrmPrincipal.PathArquivoLog: String;
begin
  Result := PathApp + 'log_' + StringReplace(edtDelphiVersion.Text, ' ', '_', [rfReplaceAll]) + '.txt';
end;

// verificar se no caminho informado já existe o .svn indicando que o
// checkout já foi feito no diretorio
function TfrmPrincipal.IsCheckOutJaFeito(const ADiretorio: String): Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(ADiretorio) + '.svn')
end;

// retorna o diretório de sistema atual
function TfrmPrincipal.PathSystem: String;
var
  strTmp: array[0..MAX_PATH] of char;
  DirWindows: String;
const
  SYS_64 = 'SysWOW64';
  SYS_32 = 'System32';
begin
  Result := '';

  //SetLength(strTmp, MAX_PATH);
  if Windows.GetWindowsDirectory(strTmp, MAX_PATH) > 0 then
  begin
    DirWindows := Trim(StrPas(strTmp));
    DirWindows := IncludeTrailingPathDelimiter(DirWindows);

    if DirectoryExists(DirWindows + SYS_64) then
      Result := DirWindows + SYS_64
    else
    if DirectoryExists(DirWindows + SYS_32) then
      Result := DirWindows + SYS_32
    else
      raise EFileNotFoundException.Create('Diretório de sistema não encontrado.');
  end
  else
    raise EFileNotFoundException.Create('Ocorreu um erro ao tentar obter o diretório do windows.');
end;

procedure TfrmPrincipal.rdgDLLClick(Sender: TObject);
begin
  case rdgdll.ItemIndex of
    0 : sDestino := tdSystem;
    1 : sDestino := tdDelphi;
    2 : sDestino := tdNone;
  end;
end;

function TfrmPrincipal.RegistrarActiveXServer(const AServerLocation: string;
  const ARegister: Boolean): Boolean;
var
  ServerDllRegisterServer: function: HResult; stdcall;
  ServerDllUnregisterServer: function: HResult; stdcall;
  ServerHandle: THandle;

  procedure UnloadServerFunctions;
  begin
    @ServerDllRegisterServer := nil;
    @ServerDllUnregisterServer := nil;
    FreeLibrary(ServerHandle);
  end;

  function LoadServerFunctions: Boolean;
  begin
    Result := False;
    ServerHandle := SafeLoadLibrary(AServerLocation);

    if (ServerHandle <> 0) then
    begin
      @ServerDllRegisterServer := GetProcAddress(ServerHandle, 'DllRegisterServer');
      @ServerDllUnregisterServer := GetProcAddress(ServerHandle, 'DllUnregisterServer');

      if (@ServerDllRegisterServer = nil) or (@ServerDllUnregisterServer = nil) then
        UnloadServerFunctions
      else
        Result := True;
    end;
  end;
begin
  Result := False;
  try
    if LoadServerFunctions then
    try
      if ARegister then
        Result := ServerDllRegisterServer = S_OK
      else
        Result := ServerDllUnregisterServer = S_OK;
    finally
      UnloadServerFunctions;
    end;
  except
  end;
end;

procedure TfrmPrincipal.CopiarArquivoTo(ADestino : TDestino; const ANomeArquivo: String);
var
  PathOrigem: String;
  PathDestino: String;
  DirSystem: String;
  DirACBr: String;
begin
  case ADestino of
    tdSystem: DirSystem := Trim(PathSystem);
    tdDelphi: DirSystem := sPathBin;
  end;

  DirACBr := IncludeTrailingPathDelimiter(edtDirDestino.Text);

  if DirSystem <> EmptyStr then
    DirSystem := IncludeTrailingPathDelimiter(DirSystem)
  else
    raise EFileNotFoundException.Create('Diretório de sistema não encontrado.');

  PathOrigem  := DirACBr + 'DLLs\' + ANomeArquivo;
  PathDestino := DirSystem + ExtractFileName(ANomeArquivo);

  if FileExists(PathOrigem) and not(FileExists(PathDestino)) then
  begin
    if not CopyFile(PWideChar(PathOrigem), PWideChar(PathDestino), True) then
    begin
      raise EFilerError.CreateFmt(
        'Ocorreu o seguinte erro ao tentar copiar o arquivo "%s": %d - %s', [
        ANomeArquivo, GetLastError, SysErrorMessage(GetLastError)
      ]);
    end;
  end;
end;

// copia as dlls da pasta capcom para a pasta escolhida pelo usuario e registra a dll
procedure TfrmPrincipal.InstalarCapicom;
begin
  if sDestino <> tdNone then
  begin
    CopiarArquivoTo(sDestino,'Capicom\capicom.dll');
    CopiarArquivoTo(sDestino,'Capicom\msxml5.dll');
    CopiarArquivoTo(sDestino,'Capicom\msxml5r.dll');

    if sDestino = tdDelphi then
    begin
      RegistrarActiveXServer(sPathBin + 'capicom.dll', True);
      RegistrarActiveXServer(sPathBin + 'msxml5.dll', True);
    end
    else
    begin
      RegistrarActiveXServer('capicom.dll', True);
      RegistrarActiveXServer('msxml5.dll', True);
    end;
  end;
end;

//copia as dlls da pasta Diversoso para a pasta escolhida pelo usuario
procedure TfrmPrincipal.InstalarDiversos;
begin
  if sDestino <> tdNone then
  begin
    CopiarArquivoTo(sDestino,'Diversos\iconv.dll');
    CopiarArquivoTo(sDestino,'Diversos\inpout32.dll');
    CopiarArquivoTo(sDestino,'Diversos\msvcr71.dll');
  end;
end;

// copia as dlls da pasta openssl, estas dlls são utilizadas para assinar
// arquivos e outras coisas mais
procedure TfrmPrincipal.InstalarOpenSSL;
begin
  if sDestino <> tdNone then
  begin
    CopiarArquivoTo(sDestino,'OpenSSL\1.0.2.13\x86\libeay32.dll');
    CopiarArquivoTo(sDestino,'OpenSSL\1.0.2.13\x86\ssleay32.dll');
    CopiarArquivoTo(sDestino,'OpenSSL\1.0.2.13\x86\msvcr120.dll');
  end;
end;

//copia as dlls da pasta XMLSec para a pasta escolhida pelo usuario
procedure TfrmPrincipal.InstalarXMLSec;
begin
  if sDestino <> tdNone then
  begin
    CopiarArquivoTo(sDestino, 'XMLSec\iconv.dll');
    CopiarArquivoTo(sDestino, 'XMLSec\libxml2.dll');
    CopiarArquivoTo(sDestino, 'XMLSec\libxmlsec.dll');
    CopiarArquivoTo(sDestino, 'XMLSec\libxmlsec-openssl.dll');
    CopiarArquivoTo(sDestino, 'XMLSec\libxslt.dll');
    CopiarArquivoTo(sDestino, 'XMLSec\zlib1.dll');
  end;
end;

// ler o arquivo .ini de configurações e setar os campos com os valores lidos
procedure TfrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    edtDirDestino.Text          := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', ExtractFilePath(ParamStr(0)));
    edtPlatform.ItemIndex := 0;
    ckbFecharTortoise.Checked      := ArqIni.ReadBool('CONFIG', 'FecharTortoise', True);
    rdgDLL.ItemIndex               := ArqIni.ReadInteger('CONFIG','DestinoDLL',0);
    ckbCopiarTodasDll.Checked      := True;
    ckbBCB.Checked                 := ArqIni.ReadBool('CONFIG','C++Builder',False);
    chkDeixarSomenteLIB.Checked    := ArqIni.ReadBool('CONFIG','DexarSomenteLib',False);
    ckbRemoveOpenSSL.Checked       := ArqIni.ReadBool('CONFIG','RemoveOpenSSL',False);
    ckbRemoveCapicom.Checked       := ArqIni.ReadBool('CONFIG','RemoveCapicom',False);
    ckbCargaDllTardia.Checked      := ArqIni.ReadBool('CONFIG','CargaDllTardia',False);
    ckbRemoverCastWarnings.Checked := ArqIni.ReadBool('CONFIG','RemoverCastWarnings',False);
    ckbUsarArquivoConfig.Checked   := True;

    if Trim(edtDelphiVersion.Text) = '' then
      edtDelphiVersion.ItemIndex := 0;

    edtDelphiVersionChange(edtDelphiVersion);

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      framePacotes1.Pacotes[I].Checked := ArqIni.ReadBool('PACOTES', framePacotes1.Pacotes[I].Caption, False);
  finally
    ArqIni.Free;
  end;
end;

procedure TfrmPrincipal.MostraDadosVersao;
begin
  // mostrar ao usuário as informações de compilação
  lbInfo.Clear;
  with lbInfo.Items do
  begin
    Clear;
    Add(edtDelphiVersion.Text + ' ' + edtPlatform.Text);
    Add('Dir. Instalação  : ' + edtDirDestino.Text);
    Add('Dir. Bibliotecas : ' + sDirLibrary);
  end;
end;

// gravar as configurações efetuadas pelo usuário
procedure TfrmPrincipal.GravarConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteBool('CONFIG', 'FecharTortoise', ckbFecharTortoise.Checked);
    ArqIni.WriteInteger('CONFIG','DestinoDLL', rdgDLL.ItemIndex);
    ArqIni.WriteBool('CONFIG','C++Builder',ckbBCB.Checked);
    ArqIni.WriteBool('CONFIG','DexarSomenteLib', chkDeixarSomenteLIB.Checked);
    ArqIni.WriteBool('CONFIG','RemoveOpenSSL', ckbRemoveOpenSSL.Checked);
    ArqIni.WriteBool('CONFIG','RemoveCapicom', ckbRemoveCapicom.Checked);
    ArqIni.WriteBool('CONFIG','CargaDllTardia', ckbCargaDllTardia.Checked);
    ArqIni.WriteBool('CONFIG','RemoverCastWarnings', ckbRemoverCastWarnings.Checked);

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      ArqIni.WriteBool('PACOTES', framePacotes1.Pacotes[I].Caption, framePacotes1.Pacotes[I].Checked);
  finally
    ArqIni.Free;
  end;
end;

// criação dos diretórios necessários
procedure TfrmPrincipal.CreateDirectoryLibrarysNotExist;
begin
  // Checa se existe diretório da plataforma
  if not DirectoryExists(sDirLibrary) then
    ForceDirectories(sDirLibrary);
end;

procedure TfrmPrincipal.DeixarSomenteLib;
  procedure Copiar(const Extensao : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(IncludeTrailingPathDelimiter(sDirRoot) + 'Fontes', Extensao ,TSearchOption.soAllDirectories ) ;
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo := ExtractFileName(ListArquivos[i]);
      CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(sDirLibrary) + Arquivo), False);
    end;
  end;
begin
  // remover os path com o segundo parametro
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Fontes', False);

  Copiar('*.dcr');
  Copiar('*.res');
  Copiar('*.dfm');
  Copiar('*.ini');
  Copiar('*.inc');
end;

procedure TfrmPrincipal.AddLibraryPathToDelphiPath(const APath: String; const AProcurarRemover: String);
const
  cs: PChar = 'Environment Variables';
var
  lParam, wParam: Integer;
  aResult: Cardinal;
  ListaPaths: TStringList;
  I: Integer;
  PathsAtuais: String;
//  PathFonte: string;
begin
  with oACBr.Installations[iVersion] do
  begin
    // tentar ler o path configurado na ide do delphi, se não existir ler
    // a atual para complementar e fazer o override
    PathsAtuais := Trim(EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');

    // manipular as strings
    ListaPaths := TStringList.Create;
    try
      ListaPaths.Clear;
      ListaPaths.Delimiter       := ';';
      ListaPaths.StrictDelimiter := True;
      ListaPaths.DelimitedText   := PathsAtuais;

      // verificar se existe algo do ACBr e remover do environment variable PATH do delphi
      if Trim(AProcurarRemover) <> '' then
      begin
        for I := ListaPaths.Count - 1 downto 0 do
        begin
         if Pos(AnsiUpperCase(AProcurarRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
           ListaPaths.Delete(I);
        end;
      end;

      // adicionar o path
      ListaPaths.Add(APath);

      // escrever a variavel no override da ide
      ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);

      // enviar um broadcast de atualização para o windows
      wParam := 0;
      lParam := LongInt(cs);
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, aResult);
      if aResult <> 0 then
        raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(aResult));
    finally
      ListaPaths.Free;
    end;
  end;
end;

procedure TfrmPrincipal.FindDirs(ADirRoot: String; bAdicionar: Boolean = True);
var
  oDirList: TSearchRec;

  function EProibido(const ADir: String): Boolean;
  const
    LISTA_PROIBIDOS: ARRAY[0..5] OF STRING = (
      'quick', 'rave', 'laz', 'VerificarNecessidade', '__history', '__recovery'
    );
  var
    Str: String;
  begin
    Result := False;
    for str in LISTA_PROIBIDOS do
    begin
      Result := Pos(AnsiUpperCase(str), AnsiUpperCase(ADir)) > 0;
      if Result then
        Break;
    end;
  end;

begin
  ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

  if FindFirst(ADirRoot + '*.*', faDirectory, oDirList) = 0 then
  begin
     try
       repeat
          if ((oDirList.Attr and faDirectory) <> 0) and
              (oDirList.Name <> '.')                and
              (oDirList.Name <> '..')               and
              (not EProibido(oDirList.Name)) then
          begin
             with oACBr.Installations[iVersion] do
             begin
               if bAdicionar then
               begin
                  AddToLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
                  AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, tPlatform);
               end
               else
                  RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
             end;
             //-- Procura subpastas
             FindDirs(ADirRoot + oDirList.Name, bAdicionar);
          end;
       until FindNext(oDirList) <> 0;
     finally
       SysUtils.FindClose(oDirList)
     end;
  end;
end;

// adicionar o paths ao library path do delphi
procedure TfrmPrincipal.AddLibrarySearchPath;
begin
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Fontes');

  // --
  with oACBr.Installations[iVersion] do
  begin
    AddToLibraryBrowsingPath(sDirLibrary, tPlatform);
    AddToLibrarySearchPath(sDirLibrary, tPlatform);
    AddToDebugDCUPath(sDirLibrary, tPlatform);
  end;

  // -- adicionar a library path ao path do windows
  AddLibraryPathToDelphiPath(sDirLibrary, 'acbr');

  //-- ************ C++ Builder *************** //
  if ckbBCB.Checked then
  begin
     if oACBr.Installations[iVersion] is TJclBDSInstallation then
     begin
        with TJclBDSInstallation(oACBr.Installations[iVersion]) do
        begin
           AddToCppSearchPath(sDirLibrary, tPlatform);
           AddToCppLibraryPath(sDirLibrary, tPlatform);
           AddToCppBrowsingPath(sDirLibrary, tPlatform);
           AddToCppIncludePath(sDirLibrary, tPlatform);
        end;
     end;
  end;
end;

// setar a plataforma de compilação
procedure TfrmPrincipal.SetPlatformSelected;
var
  sVersao: String;
  sTipo: String;
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sVersao  := AnsiUpperCase(oACBr.Installations[iVersion].VersionNumberStr);
  sDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);

  sTipo := 'Lib\Delphi\';

  if edtPlatform.ItemIndex = 0 then // Win32
  begin
    tPlatform   := bpWin32;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao;
  end
  else
  if edtPlatform.ItemIndex = 1 then // Win64
  begin
    tPlatform   := bpWin64;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao + 'x64';
  end;
end;

// Evento disparado a cada ação do instalador
procedure TfrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
    WriteToTXT(PathArquivoLog, Text);
end;

// evento para setar os parâmetros do compilador antes de compilar
procedure TfrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
var
  LArquivoCfg: TFilename;
begin
  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if (oACBr.Installations[iVersion].SupportsNoConfig)and
     // -- Arquivo cfg agora opcional no caso de paths muito extensos
     (not ckbUsarArquivoConfig.Checked) then
    Sender.Options.Add('--no-config');

  // -B = Build all units
  Sender.Options.Add('-B');
  // O+ = Optimization
  Sender.Options.Add('-$O-');
  // W- = Generate stack frames
  Sender.Options.Add('-$W+');
  // Y+ = Symbol reference info
  Sender.Options.Add('-$Y-');
  // -M = Make modified units
  Sender.Options.Add('-M');
  // -Q = Quiet compile
  Sender.Options.Add('-Q');
  // não mostrar warnings
  Sender.Options.Add('-H-');
  // não mostrar hints
  Sender.Options.Add('-W-');
  // -D<syms> = Define conditionals
  Sender.Options.Add('-DRELEASE');
  // -U<paths> = Unit directories
  Sender.AddPathOption('U', oACBr.Installations[iVersion].LibFolderName[tPlatform]);
  Sender.AddPathOption('U', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  Sender.AddPathOption('U', sDirLibrary);
  // -I<paths> = Include directories
  Sender.AddPathOption('I', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -R<paths> = Resource directories
  Sender.AddPathOption('R', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -N0<path> = unit .dcu output directory
  Sender.AddPathOption('N0', sDirLibrary);
  Sender.AddPathOption('LE', sDirLibrary);
  Sender.AddPathOption('LN', sDirLibrary);

  // ************ C++ Builder *************** //
  if ckbBCB.Checked then
  begin
     // -JL compila c++ builder
     Sender.AddPathOption('JL', sDirLibrary);
     // -NO compila .dpi output directory c++ builder
     Sender.AddPathOption('NO', sDirLibrary);
     // -NB compila .lib output directory c++ builder
     Sender.AddPathOption('NB', sDirLibrary);
     // -NH compila .hpp output directory c++ builder
     Sender.AddPathOption('NH', sDirLibrary);
  end;
  //
  with oACBr.Installations[iVersion] do
  begin
     // -- Path para instalar os pacotes do Rave no D7, nas demais versões
     // -- o path existe.
     if VersionNumberStr = 'd7' then
        Sender.AddPathOption('U', oACBr.Installations[iVersion].RootDir + '\Rave5\Lib');

     // -- Na versão XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
     // -- os prefixos dos nomes, para identificar se será compilado para VCL ou FMX
     if VersionNumberStr = 'd16' then
        Sender.Options.Add('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

     if MatchText(VersionNumberStr, ['d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']) then
        Sender.Options.Add('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');

  end;
  
  if (ckbUsarArquivoConfig.Checked) then
  begin
    LArquivoCfg := ChangeFileExt(FPacoteAtual, '.cfg');
    Sender.Options.SaveToFile(LArquivoCfg);
    Sender.Options.Clear;
  end;
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  iFor: Integer;
begin
  iVersion    := -1;
  sDirRoot    := '';
  sDirLibrary := '';
  sDirPackage := '';

  oACBr := TJclBorRADToolInstallations.Create;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to oACBr.Count - 1 do
  begin
    if      oACBr.Installations[iFor].VersionNumberStr = 'd3' then
      edtDelphiVersion.Items.Add('Delphi 3')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd4' then
      edtDelphiVersion.Items.Add('Delphi 4')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd5' then
      edtDelphiVersion.Items.Add('Delphi 5')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd6' then
      edtDelphiVersion.Items.Add('Delphi 6')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd7' then
      edtDelphiVersion.Items.Add('Delphi 7')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd9' then
      edtDelphiVersion.Items.Add('Delphi 2005')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd10' then
      edtDelphiVersion.Items.Add('Delphi 2006')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd11' then
      edtDelphiVersion.Items.Add('Delphi 2007')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd12' then
      edtDelphiVersion.Items.Add('Delphi 2009')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd14' then
      edtDelphiVersion.Items.Add('Delphi 2010')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd15' then
      edtDelphiVersion.Items.Add('Delphi XE')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd16' then
      edtDelphiVersion.Items.Add('Delphi XE2')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd17' then
      edtDelphiVersion.Items.Add('Delphi XE3')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd18' then
      edtDelphiVersion.Items.Add('Delphi XE4')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd19' then
      edtDelphiVersion.Items.Add('Delphi XE5')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd20' then
      edtDelphiVersion.Items.Add('Delphi XE6')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd21' then
      edtDelphiVersion.Items.Add('Delphi XE7')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd22' then
      edtDelphiVersion.Items.Add('Delphi XE8')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd23' then
      edtDelphiVersion.Items.Add('Delphi 10 Seattle')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd24' then
      edtDelphiVersion.Items.Add('Delphi 10.1 Berlin')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd25' then
      edtDelphiVersion.Items.Add('Delphi 10.2 Tokyo')
    else if oACBr.Installations[iFor].VersionNumberStr = 'd26' then
      edtDelphiVersion.Items.Add('Delphi 10.3 Rio');

    // -- Evento disparado antes de iniciar a execução do processo.
    oACBr.Installations[iFor].DCC32.OnBeforeExecute := BeforeExecute;

    // -- Evento para saidas de mensagens.
    oACBr.Installations[iFor].OutputCallback := OutputCallLine;
  end;
  //
  clbDelphiVersion.Items.Text := edtDelphiVersion.Items.Text;

  if edtDelphiVersion.Items.Count > 0 then
  begin
    edtDelphiVersion.ItemIndex := 0;
    iVersion := 0;
  end;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oACBr.Free;
end;

procedure TfrmPrincipal.RemoverDiretoriosEPacotesAntigos;
var
  ListaPaths: TStringList;
  I: Integer;
begin
  ListaPaths := TStringList.Create;
  try
    ListaPaths.StrictDelimiter := True;
    ListaPaths.Delimiter := ';';
    with oACBr.Installations[iVersion] do
    begin
      // remover do search path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawLibrarySearchPath[tPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      RawLibrarySearchPath[tPlatform] := ListaPaths.DelimitedText;
      // remover do browse path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawLibraryBrowsingPath[tPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      RawLibraryBrowsingPath[tPlatform] := ListaPaths.DelimitedText;
      // remover do Debug DCU path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := RawDebugDCUPath[tPlatform];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      RawDebugDCUPath[tPlatform] := ListaPaths.DelimitedText;
      // remover pacotes antigos
      for I := IdePackages.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(IdePackages.PackageFileNames[I])) > 0 then
          IdePackages.RemovePackage(IdePackages.PackageFileNames[I]);
      end;
    end;
  finally
    ListaPaths.Free;
  end;
end;

procedure TfrmPrincipal.GetDriveLetters(AList: TStrings);
var
  vDrivesSize: Cardinal;
  vDrives: array[0..128] of Char;
  vDrive: PChar;
  vDriveType: Cardinal;
begin
  AList.BeginUpdate;
  try
    // clear the list from possible leftover from prior operations
    AList.Clear;
    vDrivesSize := GetLogicalDriveStrings(SizeOf(vDrives), vDrives);
    if vDrivesSize = 0 then
      Exit;

    vDrive := vDrives;
    while vDrive^ <> #0 do
    begin
      // adicionar somente drives fixos
      vDriveType := GetDriveType(vDrive);
      if vDriveType = DRIVE_FIXED then
        AList.Add(StrPas(vDrive));

      Inc(vDrive, SizeOf(vDrive));
    end;
  finally
	  AList.EndUpdate;
  end;
end;

procedure TfrmPrincipal.RemoverArquivosAntigosDoDisco;
var
  PathBat: String;
  DriverList: TStringList;
  ConteudoArquivo: String;
  I: Integer;
begin
  PathBat := ExtractFilePath(ParamStr(0)) + 'apagarACBr.bat';

  // listar driver para montar o ConteudoArquivo
  DriverList := TStringList.Create;
  try
    GetDriveLetters(DriverList);
    ConteudoArquivo := '@echo off' + sLineBreak;
    for I := 0 to DriverList.Count -1 do
    begin
      ConteudoArquivo := ConteudoArquivo + StringReplace(DriverList[I], '\', '', []) + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + 'cd\' + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + 'del ACBr*.bpl ACBr*.dcp ACBr*.dcu PCN*.bpl PCN*.dcp PCN*.dcu SYNA*.bpl SYNA*.dcp SYNA*.dcu pnfs*.dcu pcte*.bpl pcte*.dcp pcte*.dcu pmdfe*.bpl pmdfe*.dcp pmdfe*.dcu pgnre*.dcp pgnre*.dcu pces*.dcp pces*.dcu pca*.dcp pca*.dcu /s' + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + sLineBreak;
    end;

    WriteToTXT(PathBat, ConteudoArquivo, False);
  finally
    DriverList.Free;
  end;

  RunAsAdminAndWaitForCompletion(Handle, PathBat);
end;

function TfrmPrincipal.GetPathACBrInc: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(edtDirDestino.Text) + 'Fontes\ACBrComum\ACBr.inc';
end;

// botão de compilação e instalação dos pacotes selecionados no treeview
procedure TfrmPrincipal.btnInstalarACBrClick(Sender: TObject);
var
  iDpk: Integer;
  bRunOnly: Boolean;
  NomePacote: String;
  Cabecalho: String;
  iListaVer: Integer;

  procedure Logar(const AString: String);
  begin
    lstMsgInstalacao.Items.Add(AString);
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
    Application.ProcessMessages;

    WriteToTXT(PathArquivoLog, AString);
  end;

  procedure MostrarMensagemInstalado(const aMensagem: String; const aErro: String = '');
  var
    Msg: String;
  begin

    if Trim(aErro) = EmptyStr then
    begin
      case sDestino of
        tdSystem: Msg := Format(aMensagem + ' em "%s"', [PathSystem]);
        tdDelphi: Msg := Format(aMensagem + ' em "%s"', [sPathBin]);
        tdNone:   Msg := 'Tipo de destino "nenhum" não aceito!';
      end;
    end
    else
    begin
      Inc(FCountErros);

      case sDestino of
        tdSystem: Msg := Format(aMensagem + ' em "%s": "%s"', [PathSystem, aErro]);
        tdDelphi: Msg := Format(aMensagem + ' em "%s": "%s"', [sPathBin, aErro]);
        tdNone:   Msg := 'Tipo de destino "nenhum" não aceito!';
      end;
    end;

    WriteToTXT(PathArquivoLog, '');
    Logar(Msg);
  end;

  procedure IncrementaBarraProgresso;
  begin
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    Application.ProcessMessages;
  end;

  procedure DesligarDefineACBrInc(const ADefineName: String; const ADesligar: Boolean);
  var
    F: TStringList;
    I: Integer;
  begin
    F := TStringList.Create;
    try
      F.LoadFromFile(GetPathACBrInc);
      for I := 0 to F.Count - 1 do
      begin
        if Pos(ADefineName.ToUpper, F[I].ToUpper) > 0 then
        begin
          if ADesligar then
            F[I] := '{$DEFINE ' + ADefineName + '}'
          else
            F[I] := '{.$DEFINE ' + ADefineName + '}';

          Break;
        end;
      end;
      F.SaveToFile(GetPathACBrInc);
    finally
      F.Free;
    end;
  end;

begin
  DesligarDefineACBrInc('DFE_SEM_OPENSSL',  ckbRemoveOpenSSL.Checked);
  DesligarDefineACBrInc('DFE_SEM_CAPICOM',  ckbRemoveCapicom.Checked);
  DesligarDefineACBrInc('USE_DELAYED',      ckbCargaDllTardia.Checked);
  DesligarDefineACBrInc('REMOVE_CAST_WARN', ckbRemoverCastWarnings.Checked);
  DesligarDefineACBrInc('DFE_SEM_XMLSEC',   ckbRemoverXMLSec.Checked);


  for iListaVer := 0 to clbDelphiVersion.Count -1 do
  begin
    // só instala as versão marcadas para instalar.
    if clbDelphiVersion.Checked[iListaVer] then
    begin
      lstMsgInstalacao.Clear;
      pgbInstalacao.Position := 0;

      // seleciona a versão no combobox.
      edtDelphiVersion.ItemIndex := iListaVer;
      edtDelphiVersionChange(edtDelphiVersion);

      // define dados da plataforna selecionada
      SetPlatformSelected;

      // mostra dados da versão na tela a ser instaladas
      MostraDadosVersao();

      FCountErros := 0;

      btnInstalarACBr.Enabled := False;
      wizPgInstalacao.EnableButton(bkNext, False);
      wizPgInstalacao.EnableButton(bkBack, False);
      wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
      try
        Cabecalho := 'Caminho: ' + edtDirDestino.Text + sLineBreak +
                     'Versão do delphi: ' + edtDelphiVersion.Text + ' (' + IntToStr(iVersion)+ ')' + sLineBreak +
                     'Plataforma: ' + edtPlatform.Text + '(' + IntToStr(Integer(tPlatform)) + ')' + sLineBreak +
                     StringOfChar('=', 80);

        // limpar o log
        lstMsgInstalacao.Clear;
        WriteToTXT(PathArquivoLog, Cabecalho, False);

        // setar barra de progresso
        pgbInstalacao.Position := 0;
        pgbInstalacao.Max := (framePacotes1.Pacotes.Count * 2) + 6;

        // *************************************************************************
        // removendo arquivos antigos se configurado
        // *************************************************************************
        if ckbRemoverArquivosAntigos.Checked then
        begin
          if Application.MessageBox(
            'você optou por limpar arquivos antigos do ACBr do seu computador, essa ação pode demorar vários minutos, deseja realmente continuar com está ação?',
            'Limpar',
            MB_YESNO + MB_DEFBUTTON2
          ) = ID_YES then
          begin
            Logar('Removendo arquivos antigos do disco...');
            RemoverArquivosAntigosDoDisco;
            IncrementaBarraProgresso;
          end;
        end;

        // *************************************************************************
        // Cria diretório de biblioteca da versão do delphi selecionada,
        // só será criado se não existir
        // *************************************************************************
        Logar('Criando diretórios de bibliotecas...');
        CreateDirectoryLibrarysNotExist;
        IncrementaBarraProgresso;


        // *************************************************************************
        // remover paths do delphi
        // *************************************************************************
        Logar('Removendo diretorios e pacotes antigos instalados...');
        RemoverDiretoriosEPacotesAntigos;
        IncrementaBarraProgresso;


        // *************************************************************************
        // Adiciona os paths dos fontes na versão do delphi selecionada
        // *************************************************************************
        Logar('Adicionando library paths...');
        AddLibrarySearchPath;
        IncrementaBarraProgresso;


        // *************************************************************************
        // compilar os pacotes primeiramente
        // *************************************************************************
        Logar('');
        Logar('COMPILANDO OS PACOTES...');
        for iDpk := 0 to framePacotes1.Pacotes.Count - 1 do
        begin
          NomePacote := framePacotes1.Pacotes[iDpk].Caption;

          // Busca diretório do pacote
          ExtrairDiretorioPacote(NomePacote);

          if (IsDelphiPackage(NomePacote)) and (framePacotes1.Pacotes[iDpk].Checked) then
          begin
            WriteToTXT(PathArquivoLog, '');
            FPacoteAtual := sDirPackage + NomePacote;
            if oACBr.Installations[iVersion].CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
              Logar(Format('Pacote "%s" compilado com sucesso.', [NomePacote]))
            else
            begin
              Inc(FCountErros);
              Logar(Format('Erro ao compilar o pacote "%s".', [NomePacote]));

              // parar no primeiro erro para evitar de compilar outros pacotes que
              // precisam do pacote que deu erro
              Break
            end;
          end;

          IncrementaBarraProgresso;
        end;


        // *************************************************************************
        // instalar os pacotes somente se não ocorreu erro na compilação e plataforma for Win32
        // *************************************************************************
        if (edtPlatform.ItemIndex = 0) then
        begin
          if (FCountErros <= 0) then
          begin
            Logar('');
            Logar('INSTALANDO OS PACOTES...');

            for iDpk := 0 to framePacotes1.Pacotes.Count - 1 do
            begin
              NomePacote := framePacotes1.Pacotes[iDpk].Caption;

              // Busca diretório do pacote
              ExtrairDiretorioPacote(NomePacote);

              if IsDelphiPackage(NomePacote) then
              begin
                FPacoteAtual := sDirPackage + NomePacote;
                // instalar somente os pacotes de designtime
                GetDPKFileInfo(sDirPackage + NomePacote, bRunOnly);
                if not bRunOnly then
                begin
                  // se o pacote estiver marcado instalar, senão desinstalar
                  if framePacotes1.Pacotes[iDpk].Checked then
                  begin
                    WriteToTXT(PathArquivoLog, '');

                    if oACBr.Installations[iVersion].InstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                      Logar(Format('Pacote "%s" instalado com sucesso.', [NomePacote]))
                    else
                    begin
                      Inc(FCountErros);
                      Logar(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));

                      Break;
                    end;
                  end
                  else
                  begin
                    WriteToTXT(PathArquivoLog, '');

                    if oACBr.Installations[iVersion].UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                      Logar(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
                  end;
                end;
              end;

              IncrementaBarraProgresso;
            end;
          end
          else
          begin
            Logar('');
            Logar('Abortando... Ocorreram erros na compilação dos pacotes.');
          end;
        end
        else
        begin
          Logar('');
          Logar('Para a plataforma de 64 bits os pacotes são somente compilados.');
        end;

        if FCountErros > 0 then
        begin
          if Application.MessageBox(
            PWideChar(
              'Ocorreram erros durante o processo de instalação, '+sLineBreak+
              'para maiores informações verifique o arquivo de log gerado.'+sLineBreak+sLineBreak+
              'Deseja visualizar o arquivo de log gerado?'
            ),
            'Instalação',
            MB_ICONQUESTION + MB_YESNO
          ) = ID_YES then
          begin
            btnVisualizarLogCompilacao.Click;
            Break
          end;
        end;

        // *************************************************************************
        // não instalar outros requisitos se ocorreu erro anteriormente
        // *************************************************************************
        if FCountErros <= 0 then
        begin
          Logar('');
          Logar('INSTALANDO OUTROS REQUISITOS...');

          // *************************************************************************
          // deixar somente a pasta lib se for configurado assim
          // *************************************************************************
          if chkDeixarSomenteLIB.Checked then
          begin
            try
              DeixarSomenteLib;

              MostrarMensagemInstalado('Limpeza library path com sucesso');
              MostrarMensagemInstalado('Copia dos arquivos necessário.');
            except
              on E: Exception do
              begin
                MostrarMensagemInstalado('Ocorreu erro ao limpas os path e copiar arquivos' + sLineBreak +E.Message )
              end;
            end;
          end;
        end;
      finally
        btnInstalarACBr.Enabled := True;
        wizPgInstalacao.EnableButton(bkBack, True);
        wizPgInstalacao.EnableButton(bkNext, FCountErros = 0);
        wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
      end;
    end;
  end;

  if FCountErros = 0 then
  begin
    // *************************************************************************
    // instalar capicom
    // *************************************************************************
    try
      InstalarCapicom;
      MostrarMensagemInstalado('CAPICOM instalado com sucesso');
    except
      on E: Exception do
      begin
        MostrarMensagemInstalado('Ocorreu erro ao instalar a CAPICOM', E.Message);
      end;
    end;

    // *************************************************************************
    // instalar OpenSSL
    // *************************************************************************
    try
      InstalarOpenSSL;
      MostrarMensagemInstalado('OPENSSL instalado com sucesso');
    except
      on E: Exception do
      begin
        MostrarMensagemInstalado('Ocorreu erro ao instalar a OPENSSL', E.Message);
      end;
    end;

    // *************************************************************************
    //instalar todas as "OUTRAS" DLLs
    // *************************************************************************
    if ckbCopiarTodasDll.Checked then
    begin
      try
        InstalarXMLSec;
        InstalarDiversos;
        MostrarMensagemInstalado('Outras DLL´s instaladas com sucesso');
      except
        on E: Exception do
        begin
          MostrarMensagemInstalado('Ocorreu erro ao instalar Outras DLL´s', E.Message)
        end;
      end;
    end;

    Application.MessageBox(
      PWideChar(
        'Pacotes compilados e instalados com sucesso! '+sLineBreak+
        'Clique em "Próximo" para finalizar a instalação.'
      ),
      'Instalação',
      MB_ICONINFORMATION + MB_OK
    );
  end;

end;

// chama a caixa de dialogo para selecionar o diretório de instalação
// seria bom que a caixa fosse aquele que possui o botão de criar pasta
procedure TfrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir, [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

// quando trocar a versão verificar se libera ou não o combo
// da plataforma de compilação
procedure TfrmPrincipal.clbDelphiVersionClick(Sender: TObject);
begin
  if MatchText(oACBr.Installations[clbDelphiVersion.ItemIndex].VersionNumberStr, ['d3','d4','d5','d6']) then
  begin
    Application.MessageBox(
      'Versão do delphi não suportada pelo ACBr.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if MatchText(oACBr.Installations[clbDelphiVersion.ItemIndex].VersionNumberStr, ['d7','d9','d10','d11']) then
  begin
    Application.MessageBox(
      'Atenção: a partir de Agosto de 2016 o Projeto ACBr não suportará mais versões não Unicode do Delphi, atualize o quanto antes para versões mais recentes do Delphi.',
      'Erro.',
      MB_OK + MB_ICONWARNING
    );
  end;

  // C++ Builder a partir do D2006, versões anteriores tem IDE independentes.
  ckbBCB.Enabled := MatchText(oACBr.Installations[iVersion].VersionNumberStr, ['d10','d11','d12','d14','d15','d16','d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']);
  if not ckbBCB.Enabled then
     ckbBCB.Checked := False;
end;

procedure TfrmPrincipal.edtDelphiVersionChange(Sender: TObject);
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sPathBin := IncludeTrailingPathDelimiter(oACBr.Installations[iVersion].BinFolderName);
  // -- Plataforma só habilita para Delphi XE2
  // -- Desabilita para versão diferente de Delphi XE2
  //edtPlatform.Enabled := oACBr.Installations[iVersion].VersionNumber >= 9;
  //if oACBr.Installations[iVersion].VersionNumber < 9 then
  edtPlatform.ItemIndex := 0;
end;

// abrir o endereço do ACBrSAC quando clicar na propaganda
procedure TfrmPrincipal.imgPropaganda1Click(Sender: TObject);
begin
  // ir para o endereço do ACBrSAC
  ShellExecute(Handle, 'open', PWideChar(lblUrlACBrSac1.Caption), '', '', 1);
end;

// quando clicar em alguma das urls chamar o link mostrado no caption
procedure TfrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TfrmPrincipal.wizPgInicioNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // Verificar se o delphi está aberto
  {$IFNDEF DEBUG}
  if oACBr.AnyInstanceRunning then
  begin
    Stop := True;
    Application.MessageBox(
      'Feche a IDE do delphi antes de continuar.',
      PWideChar(Application.Title),
      MB_ICONERROR + MB_OK
    );
  end;
  {$ENDIF}

  // Verificar se o tortoise está instalado, se não estiver, não mostrar a aba de atualização
  // o usuário deve utilizar software proprio e fazer manualmente
  // pedido do forum
  wizPgObterFontes.Visible := TSVN_Class.SVNInstalled;
end;

procedure TfrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
var
  iFor: Integer;
begin
  // para 64 bit somente compilar
  if tPlatform = bpWin32 then // Win32
    btnInstalarACBr.Caption := 'Instalar'
  else // win64
    btnInstalarACBr.Caption := 'Compilar';

  lbInfo.Clear;
  for iFor := 0 to clbDelphiVersion.Count -1 do
  begin
     // Só pega os dados da 1a versão selecionada, para mostrar na tela qual vai iniciar
     if clbDelphiVersion.Checked[iFor] then
     begin
        lbInfo.Items.Add('Instalar : ' + clbDelphiVersion.Items[ifor] + ' ' + edtPlatform.Text);
     end;
  end;
end;

procedure TfrmPrincipal.wizPgInstalacaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if (lstMsgInstalacao.Count <= 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Clique no botão instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if (FCountErros > 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Ocorreram erros durante a compilação e instalação dos pacotes, verifique.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;
end;

procedure TfrmPrincipal.wizPgConfiguracaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
var
  iFor: Integer;
  bChk: Boolean;
begin
  bChk := False;
  for iFor := 0 to clbDelphiVersion.Count -1 do
  begin
     if clbDelphiVersion.Checked[iFor] then
        bChk := True;
  end;

  if not bChk then
  begin
    Stop := True;
    clbDelphiVersion.SetFocus;
    Application.MessageBox(
      'Para continuar escolha a versão do Delphi para a qual deseja instalar o ACBr.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // verificar se foi informado o diretório
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diretório de instalação não foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // prevenir plataforma em branco
  if Trim(edtPlatform.Text) = '' then
  begin
    Stop := True;
    edtPlatform.SetFocus;
    Application.MessageBox(
      'Plataforma de compilação não foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // Gravar as configurações em um .ini para utilizar depois
  GravarConfiguracoes;
end;

procedure TfrmPrincipal.wizPgObterFontesEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
  // verificar se o checkout já foi feito se sim, atualizar
  // se não fazer o checkout
  if IsCheckOutJaFeito(edtDirDestino.Text) then
  begin
    lblInfoObterFontes.Caption := 'Clique em "Atualizar" para efetuar a atualização do repositório ACBr.';
    btnSVNCheckoutUpdate.Caption := 'Atualizar...';
    btnSVNCheckoutUpdate.Tag := -1;
  end
  else
  begin
    lblInfoObterFontes.Caption := 'Clique em "Download" para efetuar o download do repositório ACBr.';
    btnSVNCheckoutUpdate.Caption := 'Download...';
    btnSVNCheckoutUpdate.Tag := 1;
  end;
end;

procedure TfrmPrincipal.btnSVNCheckoutUpdateClick(Sender: TObject);
begin
  // chamar o método de update ou checkout conforme a necessidade
  if TButton(Sender).Tag > 0 then
  begin
    // criar o diretório onde será baixado o repositório
    if not DirectoryExists(edtDirDestino.Text) then
    begin
      if not ForceDirectories(edtDirDestino.Text) then
      begin
        raise EDirectoryNotFoundException.Create(
          'Ocorreu o seguinte erro ao criar o diretório' + sLineBreak +
            SysErrorMessage(GetLastError));
      end;
    end;

    // checkout
    TSVN_Class.SVNTortoise_CheckOut(edtURL.Text, edtDirDestino.Text, ckbFecharTortoise.Checked );
  end
  else
  begin
    // update
    TSVN_Class.SVNTortoise_Update(edtDirDestino.Text, ckbFecharTortoise.Checked);
  end;
end;

procedure TfrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(PathArquivoLog), '', '', 1);
end;

procedure TfrmPrincipal.btnWCInfoClick(Sender: TObject);
begin
  // capturar informações da última revisão
  TSVN_Class.GetRevision(edtDirDestino.Text);
  ShowMessage(
    'Última Revisão: ' + TSVN_Class.WCInfo.Revision + sLineBreak +
    'Autor: ' + TSVN_Class.WCInfo.Author + sLineBreak +
    'Data: ' + TSVN_Class.WCInfo.Date
  );
end;

procedure TfrmPrincipal.wizPgObterFontesNextButtonClick(Sender: TObject;
  var Stop: Boolean);
var
  I: Integer;
  NomePacote: String;
begin
  GravarConfiguracoes;

  // verificar se os pacotes existem antes de seguir para o próximo paso
  for I := 0 to framePacotes1.Pacotes.Count - 1 do
  begin
    if framePacotes1.Pacotes[I].Checked then
    begin
      sDirRoot   := IncludeTrailingPathDelimiter(edtDirDestino.Text);
      NomePacote := framePacotes1.Pacotes[I].Caption;

      // Busca diretório do pacote
      ExtrairDiretorioPacote(NomePacote);
      if Trim(sDirPackage) = '' then
        raise Exception.Create('Não foi possível retornar o diretório do pacote : ' + NomePacote);

      if IsDelphiPackage(NomePacote) then
      begin
        if not FileExists(IncludeTrailingPathDelimiter(sDirPackage) + NomePacote) then
        begin
          Stop := True;
          Application.MessageBox(PWideChar(Format(
            'Pacote "%s" não encontrado, efetue novamente o download do repositório', [NomePacote])),
            'Erro.',
            MB_ICONERROR + MB_OK
          );
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmPrincipal.wizPrincipalCancelButtonClick(Sender: TObject);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instalação?',
    'Fechar',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_YES then
  begin
    Self.Close;
  end;
end;

procedure TfrmPrincipal.wizPrincipalFinishButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.
