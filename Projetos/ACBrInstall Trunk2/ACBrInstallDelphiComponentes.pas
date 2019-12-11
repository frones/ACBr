unit ACBrInstallDelphiComponentes;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms,
  JclIDEUtils;


  function PathSystem: String;

type
  TDestino = (tdSystem, tdDelphi, tdNone);

  TACBrInstallComponentes = class(TObject)
  private
    FApp: TApplication;

    function RunAsAdminAndWaitForCompletion(hWnd: HWND; const filename: string): Boolean;
    procedure GetDriveLetters(AList: TStrings);
    function RegistrarActiveXServer(const AServerLocation: string; const ARegister: Boolean): Boolean;
    procedure FindDirs(InstalacaoAtual: TJclBorRADToolInstallation; APlatform:
        TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);
  public
    constructor Create(app: TApplication);

    procedure CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String;
        const DirACBr: String; const APathBin: string);
    procedure InstalarCapicom(ADestino : TDestino; const DirACBr: String; const
        APathBin: string);
    procedure InstalarDiversos(ADestino: TDestino; const DirACBr: String; const
        APathBin: string);
    procedure InstalarLibXml2(ADestino: TDestino; const DirACBr: String; const
        APathBin: string);
    procedure InstalarOpenSSL(ADestino: TDestino; const DirACBr: String; const
        APathBin: string);
    procedure InstalarXMLSec(ADestino: TDestino; const DirACBr: string; const
        APathBin: string);

    procedure DesligarDefineACBrInc(const ArquivoACBrInc: TFileName; const ADefineName: String; const ADesligar: Boolean);
    procedure AdicionaLibraryPathNaDelphiVersaoEspecifica(UmaInstalacaoDelphiJcl: TJclBorRADToolInstallation;
      const APath: string; const AProcurarRemover: string);

    procedure AddLibrarySearchPath(InstalacaoAtual: TJclBorRADToolInstallation;
        InstalacaoCpp: Boolean; APlatform: TJclBDSPlatform; const ADirRoot: string;
        const ADirLibrary: string);
    procedure DeixarSomenteLib(InstalacaoAtual: TJclBorRADToolInstallation;
        APlatform: TJclBDSPlatform; const ADirRoot: string);
    procedure CopiarOutrosArquivos(const ADirRoot: string; const ADirLibrary: string);

    procedure RemoverArquivosAntigosDoDisco;
    procedure RemoverDiretoriosEPacotesAntigos(InstalacaoAtual: TJclBorRADToolInstallation;
      APlatform: TJclBDSPlatform);
  end;

implementation

uses
  ShellApi, Types, IOUtils,
  {IniFiles, StrUtils, Registry,
  JclCompilerUtils,
  Messages, FileCtrl, Variants, Graphics, Controls,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, pngimage, ShlObj,
  uFrameLista, UITypes,
  Types, JvComponentBase, JvCreateProcess, JvExControls, JvAnimatedImage,
  JvGIFCtrl, JvWizard, JvWizardRouteMapNodes, CheckLst
}
  ACBrUtil;



function PathSystem: String;
var
  strTmp: array[0..MAX_PATH] of char;
  DirWindows: String;
const
  SYS_64 = 'SysWOW64';
  SYS_32 = 'System32';
begin
// retorna o diretório de sistema atual
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


{ TACBrInstallComponentes }
constructor TACBrInstallComponentes.Create(app: TApplication);
begin
  inherited Create;
  FApp := app;
end;


procedure TACBrInstallComponentes.DeixarSomenteLib(InstalacaoAtual: TJclBorRADToolInstallation; APlatform: TJclBDSPlatform; const ADirRoot: string);
begin
  // remover os path com o segundo parametro
  FindDirs(InstalacaoAtual, APlatform, IncludeTrailingPathDelimiter(ADirRoot) + 'Fontes', False);
end;

procedure TACBrInstallComponentes.FindDirs(InstalacaoAtual: TJclBorRADToolInstallation;
    APlatform: TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);

  function EProibido(const ADir: String): Boolean;
  const
    LISTA_PROIBIDOS: ARRAY[0..5] OF STRING = (
      'quick', 'rave', 'laz', 'VerificarNecessidade', '__history', '__recovery'
    );
  var
    Str: String;
  begin
//    Result := False;
    for str in LISTA_PROIBIDOS do
    begin
      Result := Pos(AnsiUpperCase(str), AnsiUpperCase(ADir)) > 0;
      if Result then
        Break;
    end;
  end;

var
  oDirList: TSearchRec;
begin
  ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

  if FindFirst(ADirRoot + '*.*', faDirectory, oDirList) = 0 then
  begin
    try
//      InstalacaoAtual := oACBr.Installations[iVersion];
      repeat
        if ((oDirList.Attr and faDirectory) <> 0) and
            (oDirList.Name <> '.')                and
            (oDirList.Name <> '..')               and
            (not EProibido(oDirList.Name)) then
        begin
         if bAdicionar then
         begin
           InstalacaoAtual.AddToLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
           InstalacaoAtual.AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, APlatform);
         end
         else
           InstalacaoAtual.RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
         //-- Procura subpastas
         FindDirs(InstalacaoAtual, APlatform, ADirRoot + oDirList.Name, bAdicionar);
        end;
      until FindNext(oDirList) <> 0;
    finally
      SysUtils.FindClose(oDirList)
    end;
  end;
end;

procedure TACBrInstallComponentes.AddLibrarySearchPath(InstalacaoAtual: TJclBorRADToolInstallation;
   InstalacaoCpp: Boolean; APlatform: TJclBDSPlatform; const ADirRoot: string; const ADirLibrary: string );
var
  InstalacaoAtualCpp: TJclBDSInstallation;
begin
// adicionar o paths ao library path do delphi

  FindDirs(InstalacaoAtual, APlatform, IncludeTrailingPathDelimiter(ADirRoot) + 'Fontes');

  InstalacaoAtual.AddToLibraryBrowsingPath(ADirLibrary, APlatform);
  InstalacaoAtual.AddToLibrarySearchPath(ADirLibrary, APlatform);
  InstalacaoAtual.AddToDebugDCUPath(ADirLibrary, APlatform);

  // -- adicionar a library path ao path do windows

  AdicionaLibraryPathNaDelphiVersaoEspecifica(InstalacaoAtual, ADirLibrary, 'acbr');

  //-- ************ C++ Builder *************** //
  if InstalacaoCpp then
  begin
     if InstalacaoAtual is TJclBDSInstallation then
     begin
       InstalacaoAtualCpp := TJclBDSInstallation(InstalacaoAtual);
       InstalacaoAtualCpp.AddToCppSearchPath(ADirLibrary, APlatform);
       InstalacaoAtualCpp.AddToCppLibraryPath(ADirLibrary, APlatform);
       InstalacaoAtualCpp.AddToCppBrowsingPath(ADirLibrary, APlatform);
       InstalacaoAtualCpp.AddToCppIncludePath(ADirLibrary, APlatform);
     end;
  end;
end;

procedure TACBrInstallComponentes.RemoverDiretoriosEPacotesAntigos(InstalacaoAtual: TJclBorRADToolInstallation; APlatform: TJclBDSPlatform);
var
  ListaPaths: TStringList;
  I: Integer;
begin
  ListaPaths := TStringList.Create;
  try
    ListaPaths.StrictDelimiter := True;
    ListaPaths.Delimiter := ';';

    // remover do search path
    ListaPaths.Clear;
    ListaPaths.DelimitedText := InstalacaoAtual.RawLibrarySearchPath[APlatform];
    for I := ListaPaths.Count - 1 downto 0 do
    begin
      if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
        ListaPaths.Delete(I);
    end;
    InstalacaoAtual.RawLibrarySearchPath[APlatform] := ListaPaths.DelimitedText;
    // remover do browse path
    ListaPaths.Clear;
    ListaPaths.DelimitedText := InstalacaoAtual.RawLibraryBrowsingPath[APlatform];
    for I := ListaPaths.Count - 1 downto 0 do
    begin
      if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
        ListaPaths.Delete(I);
    end;
    InstalacaoAtual.RawLibraryBrowsingPath[APlatform] := ListaPaths.DelimitedText;
    // remover do Debug DCU path
    ListaPaths.Clear;
    ListaPaths.DelimitedText := InstalacaoAtual.RawDebugDCUPath[APlatform];
    for I := ListaPaths.Count - 1 downto 0 do
    begin
      if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
        ListaPaths.Delete(I);
    end;
    InstalacaoAtual.RawDebugDCUPath[APlatform] := ListaPaths.DelimitedText;
  finally
    ListaPaths.Free;
  end;

  // remover pacotes antigos
  for I := InstalacaoAtual.IdePackages.Count - 1 downto 0 do
  begin
    if Pos('ACBR', AnsiUpperCase(InstalacaoAtual.IdePackages.PackageFileNames[I])) > 0 then
      InstalacaoAtual.IdePackages.RemovePackage(InstalacaoAtual.IdePackages.PackageFileNames[I]);
  end;

end;

function TACBrInstallComponentes.RegistrarActiveXServer(const AServerLocation: string;
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

procedure TACBrInstallComponentes.CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String;
     const DirACBr: String; const APathBin: string);
var
  PathOrigem: String;
  PathDestino: String;
  DirSystem: String;

begin
  case ADestino of
    tdSystem: DirSystem := Trim(PathSystem);
    tdDelphi: DirSystem := APathBin;
  end;

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

procedure TACBrInstallComponentes.InstalarCapicom(ADestino : TDestino; const DirACBr: String; const APathBin: string);
var
  pathACBr: string;
begin
// copia as dlls da pasta capcom para a pasta escolhida pelo usuario e registra a dll
  if ADestino <> tdNone then
  begin
    pathACBr := IncludeTrailingPathDelimiter(DirACBr);
    CopiarArquivoDLLTo(ADestino,'Capicom\capicom.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'Capicom\msxml5.dll',  pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'Capicom\msxml5r.dll', pathACBr, APathBin);

    if ADestino = tdDelphi then
    begin
      RegistrarActiveXServer(APathBin + 'capicom.dll', True);
      RegistrarActiveXServer(APathBin + 'msxml5.dll', True);
    end
    else
    begin
      RegistrarActiveXServer('capicom.dll', True);
      RegistrarActiveXServer('msxml5.dll', True);
    end;
  end;
end;

//copia as dlls da pasta Diversoso para a pasta escolhida pelo usuario
procedure TACBrInstallComponentes.InstalarDiversos(ADestino: TDestino; const DirACBr: String; const APathBin: string);
var
  pathACBr: string;
begin
  if ADestino <> tdNone then
  begin
    pathACBr := IncludeTrailingPathDelimiter(DirACBr);
    CopiarArquivoDLLTo(ADestino,'Diversos\iconv.dll',    pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\inpout32.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\msvcr71.dll',  pathACBr, APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarLibXml2(ADestino: TDestino; const DirACBr: String; const APathBin: string);
var
  pathACBr: string;
begin
  if ADestino <> tdNone then
  begin
    pathACBr := IncludeTrailingPathDelimiter(DirACBr);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxslt.dll',  pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libexslt.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libiconv.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxml2.dll',  pathACBr, APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarOpenSSL(ADestino: TDestino; const DirACBr: String; const APathBin: string);
var
  pathACBr: string;
begin
// copia as dlls da pasta openssl, estas dlls são utilizadas para assinar
// arquivos e outras coisas mais
  if ADestino <> tdNone then
  begin
    pathACBr := IncludeTrailingPathDelimiter(DirACBr);
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.0.2.19\x86\libeay32.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.0.2.19\x86\ssleay32.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.0.2.19\x86\msvcr120.dll', pathACBr, APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarXMLSec(ADestino: TDestino; const DirACBr: string; const APathBin: string);
var
  pathACBr: string;
begin
//copia as dlls da pasta XMLSec para a pasta escolhida pelo usuario
  if ADestino <> tdNone then
  begin
    pathACBr := IncludeTrailingPathDelimiter(DirACBr);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\iconv.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxml2.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxmlsec.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxmlsec-openssl.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxslt.dll', pathACBr, APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\zlib1.dll', pathACBr, APathBin);
  end;
end;

procedure TACBrInstallComponentes.AdicionaLibraryPathNaDelphiVersaoEspecifica(UmaInstalacaoDelphiJcl: TJclBorRADToolInstallation;  const APath: string; const AProcurarRemover: string);
var
  PathsAtuais: string;
  ListaPaths: TStringList;
  I: Integer;
  wParam: Integer;
  lParam: Integer;
  lpdwResult: PDWORD_PTR;
  Resultado: Integer;
const
  cs: PChar = 'Environment Variables';
begin
  // tentar ler o path configurado na ide do delphi, se não existir ler
  // a atual para complementar e fazer o override
  PathsAtuais := Trim(UmaInstalacaoDelphiJcl.EnvironmentVariables.Values['PATH']);
  if PathsAtuais = '' then
    PathsAtuais := GetEnvironmentVariable('PATH');
  // manipular as strings
  ListaPaths := TStringList.Create;
  try
    ListaPaths.Clear;
    ListaPaths.Delimiter := ';';
    ListaPaths.StrictDelimiter := True;
    ListaPaths.DelimitedText := PathsAtuais;
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
    UmaInstalacaoDelphiJcl.ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);
    // enviar um broadcast de atualização para o windows
    wParam := 0;
    lParam := LongInt(cs);
    lpdwResult := NIL;
    Resultado := SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, lpdwResult);
    if Resultado = 0 then
      raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(GetLastError));
  finally
    ListaPaths.Free;
  end;
end;

procedure TACBrInstallComponentes.CopiarOutrosArquivos(const ADirRoot: string; const ADirLibrary: string);
  procedure Copiar(const Extensao : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(IncludeTrailingPathDelimiter(ADirRoot) + 'Fontes', Extensao ,TSearchOption.soAllDirectories ) ;
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo := ExtractFileName(ListArquivos[i]);
      CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(ADirLibrary) + Arquivo), False);
    end;
  end;
begin
  Copiar('*.dcr');
  Copiar('*.res');
  Copiar('*.dfm');
  Copiar('*.ini');
  Copiar('*.inc');
end;


procedure TACBrInstallComponentes.DesligarDefineACBrInc(const ArquivoACBrInc: TFileName; const ADefineName: String; const ADesligar: Boolean);
var
  F: TStringList;
  I: Integer;
begin
  F := TStringList.Create;
  try
    F.LoadFromFile(ArquivoACBrInc);
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
    F.SaveToFile(ArquivoACBrInc);
  finally
    F.Free;
  end;
end;

procedure TACBrInstallComponentes.GetDriveLetters(AList: TStrings);
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

function TACBrInstallComponentes.RunAsAdminAndWaitForCompletion(hWnd: HWND; const filename: string): Boolean;
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
      FApp.ProcessMessages;
      GetExitCodeProcess(sei.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or  FApp.Terminated;
  end;
  Result := True;
end;

procedure TACBrInstallComponentes.RemoverArquivosAntigosDoDisco;
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

  RunAsAdminAndWaitForCompletion(FApp.Handle, PathBat);
end;

end.
