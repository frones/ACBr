{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{$IFDEF FPC}
 {$IFNDEF NOGUI}
  {$DEFINE USE_LCLIntf}
 {$ENDIF}
{$ENDIF}

unit ACBrUtil.FilesIO;

interface

Uses
  SysUtils, Math, Classes,
  ACBrBase, ACBrConsts, IniFiles,
  {$IfDef COMPILER6_UP} StrUtils, DateUtils {$Else} ACBrD5, FileCtrl {$EndIf}
  {$IfDef FPC}
    ,dynlibs, LazUTF8, LConvEncoding, LCLType
    {$IfDef USE_LCLIntf} ,LCLIntf {$EndIf}
  {$EndIf}
  {$IfDef MSWINDOWS}
    ,Windows, ShellAPI
  {$Else}
    {$IfNDef FPC}
      {$IfDef ANDROID}
      ,System.IOUtils
      {$EndIf}
      {$IfDef  POSIX}
      ,Posix.Stdlib
      ,Posix.Unistd
      ,Posix.Fcntl
      {$Else}
      ,Libc
      {$EndIf}
    {$Else}
      ,unix, BaseUnix
    {$EndIf}
    {$IfNDef NOGUI}
      {$IfDef FMX}
        ,FMX.Forms
      {$Else}
        ,Forms
      {$EndIf}
    {$EndIf}
  {$EndIf} ;

type
  TFindFileSortType = (fstNone, fstDateTime, fstFileName);
  TFindFileSortDirection = (fsdNone, fsdAscending, fsdDescending);

function CompareVersions( const VersionStr1, VersionStr2 : String;
  Delimiter: char = '.' ) : Extended;

function InPort(const PortAddr:word): byte;
procedure OutPort(const PortAddr: word; const Databyte: byte); overload ;

function StrCrypt(const AString, StrChave: AnsiString): AnsiString;
function SomaAscII(const AString : AnsiString): Integer;
function StringCrc16(const AString : AnsiString ) : word;
function StringCrcCCITT(const s: AnsiString; initial:Word=$1D0F; polynomial:Word=$1021): Word;

function ApplicationPath: String;
Procedure FindFiles( const FileMask : String; AStringList : TStrings;
  IncludePath : Boolean = True;
  SortType: TFindFileSortType = fstNone;
  SortDirection: TFindFileSortDirection = fsdNone ) ;
Procedure FindSubDirectories( const APath: String; AStringList : TStrings;
  IncludePath : Boolean = True ) ;
function FileAgeACBr(const FileName: string): TDateTime;
Function FilesExists(const FileMask: String) : Boolean ;
Procedure DeleteFiles(const FileMask: String; RaiseExceptionOnFail : Boolean = True)  ;
Procedure TryDeleteFile(const AFile: String; WaitTime: Integer = 1000)  ;
function CopyFileTo(const AFromFileName, AToFileName : String;
   const AFailIfExists : Boolean = false) : Boolean;
Function PathWithDelim( const APath : String ) : String ;
Function PathWithoutDelim( const APath : String ) : String ;
Procedure CopyFilesToDir( FileMask : String ; ToDirName : String;
   const ForceDirectory : Boolean = False)  ;
procedure RunCommand(const Command: String; const Params: String = '';
   Wait : Boolean = false; WindowState : Word = 5);
procedure OpenURL( const URL : String ) ;

function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer): boolean; overload ;
function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer;
   var LibHandle: TLibHandle ): boolean; overload ;
function UnLoadLibrary(const LibName: String ): Boolean ;

function FlushToDisk(const sFile: string): boolean;
function FlushFileToDisk(const sFile: string): boolean;

Procedure DesligarMaquina(Reboot: Boolean = False; Forcar: Boolean = False;
   LogOff: Boolean = False) ;
{$IfNDef NOGUI}
function ForceForeground(AppHandle:{$IfDef FPC}LCLType.HWND{$Else}THandle{$EndIf}): boolean;
{$EndIf}

Procedure WriteToFile( const Arq: String; const ABinaryString : AnsiString;
   const ForceDirectory : Boolean = False);
Procedure WriteToTXT( const ArqTXT : String; const ABinaryString : AnsiString;
   const AppendIfExists : Boolean = True; const AddLineBreak : Boolean = True;
   const ForceDirectory : Boolean = False);
procedure WriteLog(const ArqTXT : String; const ABinaryString: AnsiString;
   const Traduz : Boolean = False) ;
function TranslateUnprintable( const ABinaryString: AnsiString ): String;

function UnZip(S: TStream): AnsiString; overload;
function UnZip(const ABinaryString: AnsiString): AnsiString; overload;
function Zip(AStream: TStream): AnsiString; overload;
function Zip(const ABinaryString: AnsiString): AnsiString; overload;

procedure LerIniArquivoOuString(const IniArquivoOuString: String; AMemIni: TMemIniFile);
function StringIsINI(const AString: String): Boolean;
function StringIsAFile(const AString: String): Boolean;
function StringIsXML(const AString: String): Boolean;
function StrIsIP(const AValue: String): Boolean;
function StringIsPDF(const AString: String): Boolean;

procedure ParseNomeArquivo(const ANome: string; out APath, AName, AExt: string);

function DefinirNomeArquivo(const APath, ANomePadraoComponente: string;
  const ANomePersonalizado: string = ''; const AExtensao: string = 'pdf'): string;

{$IFDEF MSWINDOWS}
var xInp32 : function (wAddr: word): byte; stdcall;
var xOut32 : function (wAddr: word; bOut: byte): byte; stdcall;
var xBlockInput : function (Block: BOOL): BOOL; stdcall;

var InpOutLoaded: Boolean;
var BlockInputLoaded: Boolean;

procedure LoadInpOut;
procedure LoadBlockInput;

function GetLastErrorAsHexaStr(WinErro: DWORD = 0): String;
function GetFileVersion(const AFile: String): String;

{$ENDIF}

implementation

uses
  ACBrCompress, ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.Math;

const
{$IFDEF WIN64}
  CINPOUTDLL = 'inpoutx64.dll';
{$ELSE}
  CINPOUTDLL = 'inpout32.dll';
{$ENDIF}

{$IfDef MSWINDOWS}
  AllFilesMask = '*.*';
{$Else}
  AllFilesMask = '*';
{$EndIf}


{-----------------------------------------------------------------------------
 Compara 2 Strings de controle de versão. Delimiter padrão = '.'
 Retorna 0 se VersionStr1 = VersionStr2
 Retorna Valor Negativo se VersionStr1 < VersionStr2
 Retorna Valor Positivo se VersionStr1 > VersionStr2
 Retorna valor indicando as diferenças encontras de acordo com os niveis. Ex:
 1.0.3; 1=Major=100, 0=Minor=10, 3=Build=1
 VersionStr1 VersionStr2       Result
    1.0.3      1.0.4        0 + 0 -1 = -1
    1.2.5      1.1.4        0 +10 +1 = 11
    2.0.3      1.2.9       100-10 -1 = 89
 ---------------------------------------------------------------------------- }
function CompareVersions(const VersionStr1, VersionStr2: String; Delimiter: char
  ): Extended;
var
  Niveis, I, P1I, P1F, P2I, P2F: Integer;
  SubVer1, SubVer2: String;
  Pow: Extended;

  Function FormataSubVer( ASubVer: String): String;
  const
    cDIGITOS_COMPARAR = 9;
  begin
     Result := Trim(ASubVer);
     if ASubVer = '' then
       Result := StringOfChar('0',cDIGITOS_COMPARAR)
     else if StrIsNumber(Result) then  // Se for numerico, remove zeros a esquerda
       Result := PadLeft(Result,cDIGITOS_COMPARAR,'0') ;
  end;
begin
  Result := 0;
  if Trim(VersionStr1) = Trim(VersionStr2) then
    exit ;

  Niveis := max( CountStr(VersionStr1, Delimiter), CountStr(VersionStr2, Delimiter) ) ;
  P1I := 1;
  P2I := 1;

  I := Niveis;
  while I >= 0 do
  begin
    P1F := PosEx(Delimiter, VersionStr1, P1I);
    P2F := PosEx(Delimiter, VersionStr2, P2I);

    if P1F = 0 then
      P1F := Length(VersionStr1)+1;
    if P2F = 0 then
      P2F := Length(VersionStr2)+1;

    SubVer1 := FormataSubVer( Copy(VersionStr1, P1I, P1F-P1I) );
    SubVer2 := FormataSubVer( Copy(VersionStr2, P2I, P2F-P2I) );

    if SubVer1 <> SubVer2 then
    begin
      Pow := intpower(10, I );

      if (SubVer1 > SubVer2) then
        Result := Result + Pow
      else
        Result := Result - Pow ;
    end;

    P1I := P1F+1;
    P2I := P2F+1;
    Dec( I );
  end;
end;

{-----------------------------------------------------------------------------
 Lê 1 byte de uma porta de Hardware
 Nota: - Essa funçao funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar um device driver que permita acesso direto
          a porta do Hardware a ser acessado (consulte o fabricante do Hardware)
        - Linux: é necessário ser ROOT para acessar man man
          (use: su  ou  chmod u+s SeuPrograma )
 ---------------------------------------------------------------------------- }
{$WARNINGS OFF}
function InPort(const PortAddr:word): byte;
{$IFNDEF MSWINDOWS}
var Buffer : Pointer ;
    FDevice : String ;
    N : Integer ;
    FHandle : Integer ;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  LoadInpOut;
  if not Assigned( xInp32 ) then
  begin
    raise Exception.Create('Erro ao acessar a porta: '+IntToStr(PortAddr));
  end;

  Result := xInp32(PortAddr)
{$ELSE}
  FDevice := '/dev/port' ;
  Buffer  := @Result ;

  FHandle := FileOpen(FDevice, fmOpenRead);
  if FHandle <= 0 then
     raise Exception.Create('Erro abrindo:  '+FDevice+#10+#10+
                            'Você deve ter direito de Leitura nesse diretório.');
  try
     N := FileSeek( FHandle, PortAddr, 0 )  ;
     if N <= 0 then
        raise Exception.Create('Erro ao acessar a porta: '+IntToStr(PortAddr));


     N := FileRead(FHandle, Buffer^, 1) ;
     if N <= 0 then
        raise Exception.Create('Erro ao ler a porta: '+IntToStr(PortAddr));
  finally
     FileClose( FHandle );
  end ;
{$ENDIF}
end ;
{$WARNINGS ON}

{-----------------------------------------------------------------------------
 Envia 1 byte para uma porta de Hardware
 Nota: - Essa funçao funciona normalmente em Win9x,
        - XP /NT /2000, deve-se usar um device driver que permita acesso direto
          a porta do Hardware a ser acessado (consulte o fabricante do Hardware)
        - Linux: é necessário ser ROOT para acessar /dev/port
          (use: su  ou  chmod u+s SeuPrograma )
 ---------------------------------------------------------------------------- }
procedure OutPort(const PortAddr: word; const Databyte: byte);
{$IFNDEF MSWINDOWS}
var Buffer : Pointer ;
    FDevice : String ;
    N : Integer ;
    FHandle : Integer ;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  LoadInpOut;
  if Assigned( xOut32 ) then
     xOut32(PortAddr, Databyte)
{$ELSE}
  Buffer := @Databyte ;
  FDevice := '/dev/port' ;

  FHandle := FileOpen(FDevice, fmOpenWrite);
  if FHandle <= 0 then
     raise Exception.Create('Erro abrindo:  '+FDevice+#10+#10+
                            'Você deve ter direito de Escrita nesse diretório.');
  try
     N := FileSeek( FHandle, PortAddr, 0 )  ;
     if N <= 0 then
        raise Exception.Create('Erro ao acessar a porta: '+IntToStr(PortAddr));

     N := FileWrite(Fhandle, Buffer^, 1) ;
     if N <= 0 then
        raise Exception.Create('Erro ao escrever na porta: '+IntToStr(PortAddr));
  finally
     FileClose( FHandle );
  end ;
//sleep(2)
{$ENDIF}
end ;

{-----------------------------------------------------------------------------
 Retorna a String <AString> encriptada por <StrChave>.
 Use a mesma chave para Encriptar e Desencriptar
 ---------------------------------------------------------------------------- }
function StrCrypt(const AString, StrChave: AnsiString): AnsiString;
var
  i, TamanhoString, pos, PosLetra, TamanhoChave: Integer;
  C : AnsiChar ;
begin
  Result        := AString;
  TamanhoString := Length(AString);
  TamanhoChave  := Length(StrChave);
  if (TamanhoChave = 0) or (TamanhoString = 0) then
    Exit;

  for i := 1 to TamanhoString do
  begin
     pos := (i mod TamanhoChave);
     if pos = 0 then
        pos := TamanhoChave;

     posLetra := ord(Result[i]) xor ord(StrChave[pos]);
     if posLetra = 0 then
        posLetra := ord(Result[i]);

     C := AnsiChr(posLetra);
     Result[i] := C ;
  end;
end ;

{-----------------------------------------------------------------------------
 Retorna a soma dos Valores ASCII de todos os char de <AString>
 -----------------------------------------------------------------------------}
function SomaAscII(const AString : AnsiString): Integer;
Var A , TamanhoString : Integer ;
begin
  Result        := 0 ;
  TamanhoString := Length(AString);

  For A := 1 to TamanhoString do
     Result := Result + ord( AString[A] ) ;
end ;

{-----------------------------------------------------------------------------
 Retorna valor de CRC16 de <AString>    http://www.ibrtses.com/delphi/dcrc.html
 -----------------------------------------------------------------------------}
function StringCrc16(const AString : AnsiString ):word;

  procedure ByteCrc(data:byte;var crc:word);
   Var i : Byte;
  begin
    For i := 0 to 7 do
    begin
       if ((data and $01) xor (crc and $0001)<>0) then
        begin
          crc := crc shr 1;
          crc := crc xor $A001;
        end
       else
          crc := crc shr 1;

       data := data shr 1; // this line is not ELSE and executed anyway.
    end;
  end;

  var len,i : integer;
begin
 len    := length(AString);
 Result := 0;

 for i := 1 to len do
    bytecrc( ord( AString[i] ), Result);
end;

// https://forum.lazarus.freepascal.org/index.php/topic,38279.msg259717.html#msg259717
function StringCrcCCITT(const s: AnsiString; initial:Word=$1D0F; polynomial:Word=$1021): Word;
var
  crc: Cardinal;
  len, I, J: Integer;
  b: Byte;
  bit, c15: Boolean;
begin
  len := Length(s);
  crc := initial; // initial value
  for I := 1 to len do
  begin
    b := Byte(s[I]);
    for J := 0 to 7 do
    begin
      bit := (((b shr (7-J)) and 1) = 1);
      c15 := (((crc shr 15) and 1) = 1);
      crc := crc shl 1;
      if ((c15 xor bit)) then
        crc := crc xor polynomial;
    end;
  end;
  Result := crc and $ffff;
end;


{-----------------------------------------------------------------------------
 Retorna String contendo o Path da Aplicação
-----------------------------------------------------------------------------}
function ApplicationPath: String;
begin
  {$IfDef ANDROID}
    {$IFDEF FPC}
      Result := PathWithDelim(ExtractFilePath(ParamStr(0)));
    {$ELSE}
      Result := PathWithDelim(TPath.GetHomePath);
    {$ENDIF}
  {$Else}
    Result := PathWithDelim(ExtractFilePath(ParamStr(0)));
  {$EndIf}
end;

{-----------------------------------------------------------------------------
 Encontra arquivos que correspondam a "FileMask" e cria lista com o Path e nome
 dos mesmos em "AstringList"
-----------------------------------------------------------------------------}
procedure FindFiles(const FileMask: String; AStringList: TStrings;
  IncludePath: Boolean; SortType: TFindFileSortType;
  SortDirection: TFindFileSortDirection);
var
  SearchRec: TSearchRec;
  RetFind, I: Integer;
  LastFile, AFileName, APath: String;
  SL: TStringList;
  AFileDateTime: TDateTime;
begin
 AStringList.Clear;

  LastFile := '' ;
  if IncludePath then
    APath := ExtractFilePath(FileMask)
  else
    APath := '';

  RetFind := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  try
    while (RetFind = 0) and (LastFile <> SearchRec.Name) do
    begin
      LastFile := SearchRec.Name ;

      if pos(LastFile, '..') = 0 then    { ignora . e .. }
      begin
        AFileName := APath + LastFile;
        if (SortType = fstDateTime) then
        begin
          {$IfDef DELPHIXE_UP}
            AFileDateTime := SearchRec.TimeStamp;
          {$Else}
            AFileDateTime := FileDateToDateTime(SearchRec.Time);
          {$EndIf}

          AFileName := DTtoS(AFileDateTime) + '|' + AFileName;
        end;

        AStringList.Add( AFileName ) ;
      end;

      SysUtils.FindNext(SearchRec) ;
    end ;
  finally
    SysUtils.FindClose(SearchRec) ;
  end;

  if (SortType <> fstNone) then
  begin
    SL := TStringList.Create;
    try
      SL.Assign(AStringList);
      SL.Sort;

      AStringList.Clear;
      For I := 0 to SL.Count-1 do
      begin
        AFileName := SL[I];
        if (SortType = fstDateTime) then
          AFileName := copy(AFileName, pos('|', AFileName)+1, Length(SL[I]));

        if (SortDirection = fsdDescending) then
          AStringList.Insert(0, AFileName)
        else
          AStringList.Add(AFileName);
      end;
    finally
      SL.Free;
    end;
  end;
end;

procedure FindSubDirectories(const APath: String; AStringList: TStrings;
  IncludePath: Boolean);
var
  SearchRec : TSearchRec ;
  RetFind   : Integer ;
  LastFile  : string ;
  Path      : String ;
begin
  LastFile := '' ;
  Path     := PathWithDelim(APath);
  RetFind  := SysUtils.FindFirst(Path + AllFilesMask, faDirectory, SearchRec);
  AStringList.Clear;

  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if (SearchRec.Attr and faDirectory) <> 0 then
          if pos(LastFile, '..') = 0 then    { ignora . e .. }
             AStringList.Add( IfThen(IncludePath, Path, '') + LastFile) ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end;

function FileAgeACBr(const FileName: string): TDateTime;
begin
{$IfDef FPC}
  SysUtils.FileAge(FileName, Result);
{$ELSE}
  {$IfDef DELPHI2007_UP}
    SysUtils.FileAge(FileName, Result);
  {$ELSE}
    Result:= FileDateToDateTime(SysUtils.FileAge(FileName));
  {$ENDIF}
{$ENDIF}
end;

{-----------------------------------------------------------------------------
  Semelhante a FileExists, mas permite uso de mascaras Ex:(*.BAK, TEST*.PX, etc)
 ---------------------------------------------------------------------------- }
function FilesExists(const FileMask : String) : Boolean ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
begin
  LastFile := '' ;
  Result   := false ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec) ;
  try
     while (not Result) and (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;
        Result   := (pos(LastFile, '..') = 0) ;   { ignora . e .. }
        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;


{-----------------------------------------------------------------------------
  Semelhante a DeleteFile, porem tenta deletar o Arquivo por
  <WaitTime> milisegundos. Gera Exceção se não conseguir apagar o arquivo.
 ---------------------------------------------------------------------------- }
procedure TryDeleteFile(const AFile : String ; WaitTime : Integer) ;
Var
  TFim : TDateTime ;
  Ok: Boolean;
begin
  if EstaVazio(AFile) or (not FileExists(AFile)) then
    exit ;

  TFim := IncMilliSecond(now,WaitTime) ;
  repeat
     SysUtils.DeleteFile( AFile ) ;
     Ok := (not FileExists( AFile ));
     if Ok then
       Break;

     Sleep(100);
  until (now > TFim) ;

  if not Ok then
     raise Exception.Create('Erro ao apagar: ' + AFile);
end ;
{-----------------------------------------------------------------------------
  Semelhante a DeleteFile, mas permite uso de mascaras Ex:(*.BAK, TEST*.PX, etc)
  Gera Exceção se não conseguir apagar algum dos arquivos.
 ---------------------------------------------------------------------------- }
procedure DeleteFiles(const FileMask : String ; RaiseExceptionOnFail : Boolean
   ) ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
    Path      : String ;
begin
  LastFile := '' ;
  Path     := ExtractFilePath(FileMask) ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if pos(LastFile, '..') = 0 then    { ignora . e .. }
        begin
           if not SysUtils.DeleteFile(Path + LastFile) then
             if RaiseExceptionOnFail then
               raise Exception.Create('Erro ao apagar: ' + Path + LastFile);
        end ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 *** CopyFileTo Extraida de idGlobals.pas - INDY ***
 Copia arquivo "AFromFilename" para "AToFilename".  Retorna true se OK
 Nao copia, e retorna false se o destino "AToFilename" já existir e
   "AFailIfExists"  for true
 ---------------------------------------------------------------------------- }
function CopyFileTo(const AFromFileName, AToFileName : String;
   const AFailIfExists : Boolean) : Boolean;
{$IFNDEF MSWINDOWS}
var LStream : TStream;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    Result := CopyFile(PChar(AFromFileName), PChar(AToFileName), AFailIfExists);
  {$ELSE}
    if FileExists(AToFileName) and AFailIfExists then
       Result := False
    else
     begin
       LStream := TFileStream.Create(AFromFileName, fmOpenRead or fmShareDenyWrite);
       try
          with TFileStream.Create(AToFileName, fmCreate) do
             try
                CopyFrom(LStream, 0);
             finally
                Free;
             end;
       finally
          FreeAndNil(LStream);
       end;
       Result := True;
     end;
  {$ENDIF}
end;

{-----------------------------------------------------------------------------
  Verifica se <APath> possui "PathDelim" no final. Retorna String com o Path
  já ajustado
 ---------------------------------------------------------------------------- }
function PathWithDelim(const APath : String) : String ;
begin
  Result := Trim(APath) ;
  if Result <> '' then
  begin
     {$IfDef FPC}
      Result := IncludeTrailingPathDelimiter(Result);
     {$Else}
      if RightStr(Result,1) <> PathDelim then   { Tem delimitador no final ? }
         Result := Result + PathDelim ;
     {$EndIf}
  end;
end ;

{-----------------------------------------------------------------------------
  Verifica se <APath> possui "PathDelim" no final. Retorna String SEM o
  DELIMITADOR de Path no final
 ---------------------------------------------------------------------------- }
function PathWithoutDelim(const APath : String) : String ;
Var
  Delimiters : AnsiString ;
begin
  Result := Trim(APath) ;

  Delimiters := PathDelim+'/\' ;
  while (Result <> '') and (pos(String(RightStr(Result,1)), String(Delimiters) ) > 0) do   { Tem delimitador no final ? }
     Result := copy(Result,1,Length(Result)-1)
end;

{-----------------------------------------------------------------------------
  Copia todos os arquivos especificados na mascara <FileMask> para o diretório
  <ToDirName>   Gera Exceção se não conseguir copiar algum dos arquivos.
 ---------------------------------------------------------------------------- }
procedure CopyFilesToDir(FileMask : String ; ToDirName : String ;
   const ForceDirectory : Boolean) ;
var SearchRec : TSearchRec ;
    RetFind   : Integer ;
    LastFile  : string ;
    Path      : String ;
begin
  ToDirName := PathWithDelim(ToDirName) ;
  FileMask  := Trim(FileMask) ;

  if ToDirName = '' then
     raise Exception.Create('Diretório destino não especificado') ;

  if not DirectoryExists(ToDirName) then
  begin
     if not ForceDirectory then
        raise Exception.Create('Diretório ('+ToDirName+') não existente.')
     else
      begin
        ForceDirectories( ToDirName ) ;  { Tenta criar o diretório }
        if not DirectoryExists( ToDirName ) then
           raise Exception.Create( 'Não foi possivel criar o diretório' + sLineBreak +
                                   ToDirName);
      end ;
  end ;

  LastFile := '' ;
  Path     := ExtractFilePath(FileMask) ;
  RetFind  := SysUtils.FindFirst(FileMask, faAnyFile, SearchRec);
  try
     while (RetFind = 0) and (LastFile <> SearchRec.Name) do
     begin
        LastFile := SearchRec.Name ;

        if pos(LastFile, '..') = 0 then    { ignora . e .. }
        begin
           if not CopyFileTo(Path + LastFile, ToDirName + LastFile) then
             raise Exception.Create('Erro ao Copiar o arquivo ('+
                  Path + LastFile + ') para o diretório ('+ToDirName+')') ;
        end ;

        SysUtils.FindNext(SearchRec) ;
     end ;
  finally
     SysUtils.FindClose(SearchRec) ;
  end ;
end ;

{-----------------------------------------------------------------------------
 - Executa programa Externo descrito em "Command", adcionando os Parametros
   "Params" na linha de comando
 - Se "Wait" for true para a execução da aplicação para esperar a conclusao do
   programa externo executado por "Command"
 - WindowState apenas é utilizado na plataforma Windows
 ---------------------------------------------------------------------------- }
procedure RunCommand(const Command: String; const Params: String;
   Wait : Boolean; WindowState : Word);
var
  {$IfDef MSWINDOWS}
    SUInfo: Windows.{$IfDef UNICODE}TSTARTUPINFOW{$Else}TSTARTUPINFO{$EndIf};
    ProcInfo: TProcessInformation;
    Executed: BOOL;
  {$EndIf}
  FullCommand: String;
begin
  FullCommand := Trim(Command) + ' ' + Trim(Params);
  {$IfNDef MSWINDOWS}
    if not Wait then
      FullCommand := FullCommand + ' &' ;  { & = Rodar em BackGround }

    {$IfNDef FPC}
      {$IfDef POSIX}
        _system(PAnsiChar(AnsiString(FullCommand)));
      {$Else}
        Libc.system(PChar(FullCommand));
      {$EndIf}
    {$Else}
      fpSystem(FullCommand)
    {$EndIf}
  {$Else}
    if not Wait then
    begin
      {$IfDef UNICODE}
        ShellExecute(0, LPCWSTR('open'), LPCWSTR(WideString(Trim(Command))), LPCWSTR(WideString(Trim(Params))), nil, WindowState )
      {$Else}
        ShellExecute(0, LPCSTR('open'), LPCSTR(Trim(Command)), LPCSTR(Trim(Params)), nil, WindowState )
      {$EndIf}
    end
    else
    begin
      FillChar(SUInfo, SizeOf(SUInfo), #0);
      with SUInfo do
      begin
        cb := SizeOf(SUInfo);
        dwFlags := STARTF_USESHOWWINDOW;
        wShowWindow := WindowState;
      end;

      {$IfDef UNICODE}
        Executed := CreateProcessW( nil,
                                    PWideChar(WideString(FullCommand)),
                                    nil, nil, false,
                                    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                                    nil, Nil, SUInfo, ProcInfo);

      {$Else}
        Executed := CreateProcess( nil,
                                    PChar(FullCommand),
                                    nil, nil, false,
                                    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                                    nil, Nil, SUInfo, ProcInfo);
      {$EndIf}

      try
        { Aguarda até ser finalizado }
        if Executed then
          WaitForSingleObject(ProcInfo.hProcess, INFINITE);
      finally
        { Libera os Handles }
        CloseHandle(ProcInfo.hProcess);
        CloseHandle(ProcInfo.hThread);
      end;
    end;
  {$EndIf}
end;

procedure OpenURL( const URL : String ) ;
{$IfNDef MSWINDOWS}
Var
  BrowserName : String ;
{$EndIf}
begin
 {$IfDef USE_LCLIntf}
   LCLIntf.OpenURL( URL ) ;
 {$Else}
   {$IfDef MSWINDOWS}
     RunCommand(URL);
   {$Else}
     BrowserName := GetEnvironmentVariable('BROWSER') ;
     if BrowserName = '' then
        BrowserName := 'konqueror' ;

     RunCommand(BrowserName, URL);
   {$EndIf}
 {$EndIf}
end ;

{-----------------------------------------------------------------------------
  Tenta carregar a Biblioteca (DLL) <LibName> e veirica se a função <FuncName>
  existe na DLL. Se existir, retorna ponteiro para a DLL em <LibPointer>
  Veja Exempo de uso em InPort e OutPort (logo acima)
  ( Função encontrada na Internet - Autor desconhecido )
 -----------------------------------------------------------------------------}
function FunctionDetect (const LibName, FuncName: String; var LibPointer: Pointer): boolean;
var
  LibHandle: TLibHandle;
begin
  Result := ACBrUtil.FilesIO.FunctionDetect(LibName, FuncName, LibPointer, LibHandle);
end;

function FunctionDetect(const LibName, FuncName: String; var LibPointer: Pointer;
  var LibHandle: TLibHandle): boolean;
begin
  Result := false;
  LibPointer := NIL;
 {$IFDEF FPC}
   LibHandle := dynlibs.LoadLibrary(LibName) ;
 {$ELSE}
  if LoadLibrary(PChar(LibName)) = 0 then
    exit;                                 { não consegiu ler a DLL }

  LibHandle := GetModuleHandle(PChar(LibName));  { Pega o handle da DLL }
 {$ENDIF}

  if LibHandle <> 0 then                    { Se 0 não pegou o Handle, falhou }
  begin
    LibPointer := GetProcAddress(LibHandle, PChar(FuncName));{Procura a função}
    if LibPointer <> NIL then
      Result := true;
  end;
end;

function UnLoadLibrary(const LibName: String ): Boolean ;
var
  LibHandle: TLibHandle;
begin
  Result := True;

  if LibName = '' then Exit;

{$IFDEF FPC}
  LibHandle := dynlibs.LoadLibrary( LibName );
  if LibHandle <> 0 then
    Result := dynlibs.FreeLibrary(LibHandle);
{$ELSE}
{$IFDEF DELPHI12_UP}
  LibHandle := GetModuleHandle( PWideChar( String( LibName ) ) );
 {$ELSE}
  LibHandle := GetModuleHandle( PChar( LibName ) );
 {$ENDIF}
  if LibHandle <> 0 then
    Result := FreeLibrary( LibHandle )
{$ENDIF}
end;


function FlushToDisk(const sFile: string): boolean;
{$IfDef MSWINDOWS}
 { Fonte: http://stackoverflow.com/questions/1635947/how-to-make-sure-that-a-file-was-permanently-saved-on-usb-when-user-doesnt-use }
 var
   hDrive: THandle;
   AFileName: String;
 begin
   AFileName := '\\.\' + copy(ExtractFileDrive(sFile),1,1) + ':';

   //NOTE: this may only work for the SYSTEM user
   hDrive := Windows.CreateFileW( PWideChar(WideString(AFileName)),
                          GENERIC_READ or GENERIC_WRITE,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil, OPEN_EXISTING, 0, 0);
   Result := FlushFileBuffers(hDrive);
   CloseHandle(hDrive);
 end;
{$Else}
 var
   hDrive: THandle;
 begin
   {$IfDef FPC}
     hDrive := fpOpen(sFile, O_Creat or O_RDWR {$IfDef LINUX}or O_SYNC{$EndIf});
     Result := fpfsync(hDrive) = 0;
     fpClose(hDrive);
   {$Else}
     hDrive := open(PAnsiChar(AnsiString(sFile)), O_Creat or O_RDWR or O_SYNC);
     Result := fsync(hDrive) = 0;
     __close(hDrive);
   {$EndIf}
 end ;
{$EndIf}

function FlushFileToDisk(const sFile: string): boolean;
 {$IfDef MSWINDOWS}
 var
   hFile: THandle;
   AFileName: String;
 begin
   AFileName := '\\.\' + sFile; //Para usar a versão Wide da função CreateFile e aceitar o caminho completo do arquivo

   hFile := Windows.CreateFileW( PWideChar(WideString(AFileName)),
               GENERIC_READ or GENERIC_WRITE,
               FILE_SHARE_READ or FILE_SHARE_WRITE,
               nil, OPEN_EXISTING,
               FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH or FILE_FLAG_NO_BUFFERING, 0);

    Result := FlushFileBuffers(hFile);
    CloseHandle(hFile);
 end;
{$Else}
 begin
   Result := FlushToDisk(sFile);
 end ;
{$EndIf}

{-----------------------------------------------------------------------------
 - Tenta desligar a Maquina.
 - Se "Reboot" for true Reinicializa
 *** Versão Windows extraida do www.forumweb.com.br/forum  por: Rafael Luiz ***
 ---------------------------------------------------------------------------- }
procedure DesligarMaquina(Reboot : Boolean ; Forcar : Boolean ; LogOff : Boolean
   ) ;

{$IFDEF MSWINDOWS}
   function WindowsNT: Boolean;
   var
     osVersao : TOSVersionInfo;
   begin
     osVersao.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
     GetVersionEx(osVersao);
     Result := osVersao.dwPlatformID = VER_PLATFORM_WIN32_NT;
   end;

   procedure ObtemPrivilegios;
   var
     tmpLUID : TLargeInteger;
     hdlProc, hdlToken : THandle;
     tkpNovo, tkpIgnore : TTokenPrivileges;
     dwBuffer, dwIgnoreBuffer : DWord;
   begin
     // Abrir token do processo para ajustar privilégios
     hdlProc := GetCurrentProcess;
     OpenProcessToken(hdlProc, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
         hdlToken);

     // Obter o identificador único do privilégio de shutdown
     LookupPrivilegeValue('', 'SeShutdownPrivilege', tmpLUID);

     // Habilita o privilégio de shutdown em novo token
     tkpNovo.PrivilegeCount := 1;
     tkpNovo.Privileges[0].Luid := tmpLUID;
     tkpNovo.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
     dwBuffer := SizeOf(TTokenPrivileges);

     // Ajusta o privilégio com o novo token
     AdjustTokenPrivileges(hdlToken, False, tkpNovo,
         dwbuffer, tkpIgnore, dwIgnoreBuffer);
   end;


var
  RebootParam: Longword;
begin
  if WindowsNT then
    ObtemPrivilegios;

  if Reboot then
    RebootParam := EWX_REBOOT
  else if LogOff then
    RebootParam := EWX_LOGOFF
  else
    RebootParam := EWX_SHUTDOWN;

  if Forcar then
    RebootParam := RebootParam or EWX_FORCE;

  ExitWindowsEx(RebootParam, 0);
end;
{$ELSE}
   begin
      // Precisa ser o ROOT ou a
      // aplicação ter provilegios de ROOT  (use: su  ,  chmod u+s SeuPrograma )
      //
      if LogOff then
        RunCommand('loginctl terminate-session $XDG_SESSION_ID')
      else if Reboot then
        RunCommand('sudo shutdown -r now')
      else
        RunCommand('sudo shutdown -h now') ;
   end ;
{$ENDIF}


{$IfNDef NOGUI}
{$IfDef MSWINDOWS}
// Origem: https://www.experts-exchange.com/questions/20294536/WM-ACTIVATE.html
function ForceForeground(AppHandle:{$IfDef FPC}LCLType.HWND{$Else}THandle{$EndIf}): boolean;
const
  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
var
  ForegroundThreadID: DWORD;
  ThisThreadID      : DWORD;
  timeout           : DWORD;
  OSVersionInfo     : TOSVersionInfo;
  Win32Platform     : Integer;
begin
  if IsIconic(AppHandle) then
    ShowWindow(AppHandle, SW_RESTORE);

  if (GetForegroundWindow = AppHandle) then
    Result := True
  else
  begin
    Win32Platform := 0;
    OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
    if GetVersionEx(OSVersionInfo) then
      Win32Platform := OSVersionInfo.dwPlatformId;

    { Windows 98/2000 doesn't want to foreground a window when some other window has keyboard focus}

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (OSVersionInfo.dwMajorVersion > 4)) or
       ((Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and ((OSVersionInfo.dwMajorVersion > 4) or
       ((OSVersionInfo.dwMajorVersion = 4) and (OSVersionInfo.dwMinorVersion > 0)))) then
    begin
      Result := False;
      ForegroundThreadID := GetWindowThreadProcessID(GetForegroundWindow,nil);
      ThisThreadID := GetWindowThreadPRocessId(AppHandle,nil);

      if AttachThreadInput(ThisThreadID, ForegroundThreadID, true) then
      begin
        BringWindowToTop(AppHandle);
        SetForegroundWindow(AppHandle);
        AttachThreadInput(ThisThreadID, ForegroundThreadID, false);
        Result := (GetForegroundWindow = AppHandle);
      end;

      if not Result then
      begin
        SystemParametersInfo(SPI_GETFOREGROUNDLOCKTIMEOUT, 0, @timeout, 0);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(0), SPIF_SENDCHANGE);
        BringWindowToTop(AppHandle);
        SetForegroundWindow(AppHandle);
        SystemParametersInfo(SPI_SETFOREGROUNDLOCKTIMEOUT, 0, TObject(timeout), SPIF_SENDCHANGE);
        Result := (GetForegroundWindow = AppHandle);

        if not Result then
        begin
          ShowWindow(AppHandle,SW_HIDE);
          ShowWindow(AppHandle,SW_SHOWMINIMIZED);
          ShowWindow(AppHandle,SW_SHOWNORMAL);
          BringWindowToTop(AppHandle);
          SetForegroundWindow(AppHandle);
        end;
      end;
    end
    else
    begin
      BringWindowToTop(AppHandle);
      SetForegroundWindow(AppHandle);
    end;

    Result := (GetForegroundWindow = AppHandle);
  end;
end;
{$Else}
function ForceForeground(AppHandle: {$IfDef FPC}LCLType.HWND{$Else}THandle{$EndIf}): boolean;
begin
  {$IfDef FMX}
    Application.MainForm.BringToFront;
    Result := True;
  {$Else}
    Application.Restore;
    Application.BringToFront;
    Application.RestoreStayOnTop(True);
    Application.ProcessMessages;
    if Assigned( Screen.ActiveForm ) then
      Result := (Screen.ActiveForm.Handle = AppHandle)
    else
      Result := False;
  {$EndIf}
end;
{$EndIf}
{$EndIf}

procedure WriteToFile(const Arq: String; const ABinaryString: AnsiString;
  const ForceDirectory: Boolean);
begin
  WriteToTXT(Arq, ABinaryString, False, False, ForceDirectory);
end;

{-----------------------------------------------------------------------------
 - Grava conteudo de "AString" no arquivo "ArqTXT".
 - Se arquivo "ArqTXT" não existir, será criado.  Se "ArqTXT" já existir e
   "Append" for verdadeiro adiciona "AString" no final do arquivo
 ---------------------------------------------------------------------------- }
procedure WriteToTXT(const ArqTXT: String; const ABinaryString: AnsiString;
  const AppendIfExists: Boolean; const AddLineBreak: Boolean;
  const ForceDirectory: Boolean);
var
  FS: TFileStream;
  LineBreak: AnsiString;
  VDirectory: String;
  ArquivoExiste: Boolean;
begin
  if EstaVazio(ArqTXT) then
    Exit;

  ArquivoExiste := FileExists(ArqTXT);

  if ArquivoExiste then
  begin
    if (Length(ABinaryString) = 0) then
      Exit;
  end
  else
  begin
    if ForceDirectory then
    begin
      VDirectory := ExtractFileDir(ArqTXT);
      if NaoEstaVazio(VDirectory) and (not DirectoryExists(VDirectory)) then
        ForceDirectories(VDirectory);
    end;
  end;

  FS := TFileStream.Create( ArqTXT,
          IfThen(AppendIfExists and ArquivoExiste,
                 Integer(fmOpenReadWrite), Integer(fmCreate)) or fmShareDenyWrite);
  try
    FS.Seek(0, soEnd);  // vai para EOF
    FS.Write(Pointer(ABinaryString)^,Length(ABinaryString));

    if AddLineBreak then
    begin
      LineBreak := sLineBreak;
      FS.Write(Pointer(LineBreak)^,Length(LineBreak));
    end;
  finally
    FS.Free;
  end;
end;

procedure WriteLog(const ArqTXT : String; const ABinaryString: AnsiString;
  const Traduz: Boolean);
var
  Buf: AnsiString;
begin
  if ArqTXT = '' then
    exit ;

  if Traduz then
    Buf := AnsiString(TranslateUnprintable(ABinaryString))
  else
    Buf := ABinaryString;

  try
    WriteToTXT(ArqTXT, Buf, True, True);
  except
    //Não parar o funcionamento de quem chamou WriteLog por erros causados por ela.
  end;
end;

function TranslateUnprintable(const ABinaryString: AnsiString): String;
var
  Buf, Ch: String;
  I: Integer;
  ASC: Byte;
begin
  Buf := '';

  for I := 1 to Length(ABinaryString) do
  begin
    ASC := Ord(ABinaryString[I]) ;

    case ABinaryString[I] of
       NUL   : Ch := '[NUL]';
       SOH   : Ch := '[SOH]';
       STX   : Ch := '[STX]';
       ETX   : Ch := '[ETX]';
       ENQ   : Ch := '[ENQ]';
       ACK   : Ch := '[ACK]';
       TAB   : Ch := '[TAB]';
       BS    : Ch := '[BS]';
       LF    : Ch := '[LF]';
       FF    : Ch := '[FF]';
       CR    : Ch := '[CR]';
       WAK   : Ch := '[WAK]';
       NAK   : Ch := '[NAK]';
       ESC   : Ch := '[ESC]';
       FS    : Ch := '[FS]';
       GS    : Ch := '[GS]';
       #32..#126 : Ch := String(ABinaryString[I]);
    else ;
      Ch := '['+IntToStr(ASC)+']'
    end;

    Buf := Buf + Ch;
  end;

  Result := Buf;
end;

function UnZip(S: TStream): AnsiString;
begin
  Result := ACBrCompress.DeCompress(S);
end;

function UnZip(const ABinaryString: AnsiString): AnsiString;
begin
  Result := ACBrCompress.DeCompress(ABinaryString);
end;

function Zip(AStream: TStream): AnsiString;
begin
  Result := ACBrCompress.ZLibCompress(AStream);
end;

function Zip(const ABinaryString: AnsiString): AnsiString;
begin
  Result := ACBrCompress.ZLibCompress(ABinaryString);
end;

{------------------------------------------------------------------------------
   Valida se é um arquivo válido para carregar em um MenIniFile, caso contrário
   adiciona a String convertendo representações em Hexa.
 ------------------------------------------------------------------------------}
procedure LerIniArquivoOuString(const IniArquivoOuString: String;
  AMemIni: TMemIniFile);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if StringIsINI(IniArquivoOuString) then
      SL.Text := String(StringToBinaryString( IniArquivoOuString ))
    else
    begin
      if not StringIsAFile(IniArquivoOuString) then
        raise Exception.Create(ACBrStr('String INI informada não é válida.'))
      else
      begin
        if FileExists(IniArquivoOuString) then
          SL.LoadFromFile(IniArquivoOuString)
        else
          raise Exception.CreateFmt(ACBrStr('Arquivo: %s não encontrado.'), [IniArquivoOuString] );
      end;
    end;

    AMemIni.SetStrings(SL);
  finally
    SL.Free;
  end;
end;

{------------------------------------------------------------------------------
   Valida se é um arquivo contém caracteres existentes em um ini
 ------------------------------------------------------------------------------}
function StringIsINI(const AString: String): Boolean;
begin
  Result :=(pos('[', AString) > 0) and (pos(']', AString) > 0) and (pos('=', AString) > 0);
end;

{------------------------------------------------------------------------------
   Valida as características básicas de um File válido
 ------------------------------------------------------------------------------}
function StringIsAFile(const AString: String): Boolean;
begin
  Result := (AString <> '') and
            (not StringIsXML(AString)) and
            (not StringIsINI(AString)) and
            (Length(AString) < 255) ;
end;

{------------------------------------------------------------------------------
   Valida se é um arquivo contém caracteres existentes em um xml
 ------------------------------------------------------------------------------}
function StringIsXML(const AString: String): Boolean;
begin
  Result :=(pos('<', AString) > 0) and (pos('>', AString) > 0);
end;

{-----------------------------------------------------------------------------
 ** Baseada em "IsIp" de synautil.pas - Synapse http://www.ararat.cz/synapse/ **
  Retorna <True> se <Value> é um IP Valido
 ---------------------------------------------------------------------------- }
function StrIsIP(const AValue: String): Boolean;
var
  TempIP : string;

  function ByteIsOk(const AValue: string): Boolean;
  var
    x: integer;
  begin
    x := StrToIntDef(AValue, -1);
    Result := (x >= 0) and (x < 256);
    // X may be in correct range, but value still may not be correct value!
    // i.e. "$80"
    if Result then
       Result := StrIsNumber( AValue ) ;
  end;

  function Fetch(var AValue: string; const Delimiter: string): string;
  var
    p : Integer ;
  begin
    p := pos(Delimiter,AValue) ;
    Result := copy(AValue, 1, p-1);
    AValue := copy(AValue, p+1, Length(AValue));
  end;
begin
  TempIP := AValue;
  Result := False;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if ByteIsOk(TempIP) then
    Result := True;
end;

function StringIsPDF(const AString: String): Boolean;
begin
  Result := (Pos('%PDF-', AString) <> 0);
end;

procedure ParseNomeArquivo(const ANome: string; out APath, AName, AExt: string);
begin
  APath := ExtractFilePath(ANome);
  AName := ExtractFileName(ANome);

  if (APath <> '') then
    APath := PathWithDelim( APath );

  if (AName <> '') then
  begin
    AExt := ExtractFileExt(AName);

    AName := StringReplace(AName, AExt, '', []);
  end;
end;

function DefinirNomeArquivo(const APath, ANomePadraoComponente: string;
  const ANomePersonalizado: string = ''; const AExtensao: string = 'pdf'): string;
var
  xPath, xNome, xPathParse, xNomeParse, xExtParse: string;
begin
  xPath := PathWithDelim(APath);
  xNome := Trim(ANomePersonalizado);

  if xNome <> '' then
  begin
    ParseNomeArquivo(xNome, xPathParse, xNomeParse, xExtParse);
  end
  else
  begin
    xNome := Trim(ANomePadraoComponente);
    ParseNomeArquivo(xNome, xPathParse, xNomeParse, xExtParse);
  end;

  if xPathParse <> '' then
    xNome := xPathParse + xNomeParse
  else
    xNome := xPath + xNomeParse;

  if xExtParse <> '' then
    xNome := xNome + xExtParse
  else
    xNome := xNome + AExtensao;

  Result := xNome;
end;

{$IFDEF MSWINDOWS}
procedure LoadInpOut;
begin
  if InpOutLoaded then exit;

  if not ACBrUtil.FilesIO.FunctionDetect(CINPOUTDLL,'Inp32',@xInp32) then
    xInp32 := NIL ;

  if not ACBrUtil.FilesIO.FunctionDetect(CINPOUTDLL,'Out32',@xOut32) then
    xOut32 := NIL ;

  InpOutLoaded := True;
end;

procedure LoadBlockInput;
begin
  if BlockInputLoaded then exit;

  if not ACBrUtil.FilesIO.FunctionDetect('USER32.DLL', 'BlockInput', @xBlockInput) then
     xBlockInput := NIL ;

  BlockInputLoaded := True;
end;

function GetLastErrorAsHexaStr(WinErro: DWORD): String;
begin
  if WinErro = 0 then
    WinErro := GetLastError;

  Result := IntToHex(WinErro, 8);
end;

function GetFileVersion(const AFile: String): String;
var
  Major, Minor, Release, Build: Integer;
  Zero, VersionInfoSize: DWORD;
  PVersionData: Pointer;
  lplBuffer: PVSFixedFileInfo;
  puLen: Cardinal;
begin
  // http://www.planetadelphi.com.br/dica/688/pega-informacoes-de-versao-de-qualquer-exe-ou-dll
  Result := '';
  Zero := 0;
  VersionInfoSize := GetFileVersionInfoSize(PChar(AFile), Zero);
  if VersionInfoSize = 0 then
     exit;
  PVersionData := AllocMem(VersionInfoSize);
  try
    if GetFileVersionInfo(PChar(AFile), 0, VersionInfoSize, PVersionData) then
    begin
      lplBuffer := nil;
      puLen := 0;
      if VerQueryValue(PVersionData, '', pointer(lplBuffer), puLen) then
      begin
        Major   := HIWORD(lplBuffer^.dwFileVersionMS);
        Minor   := LOWORD(lplBuffer^.dwFileVersionMS);
        Release := HIWORD(lplBuffer^.dwFileVersionLS);
        Build   := LOWORD(lplBuffer^.dwFileVersionLS);
        Result  := IntToStr(Major)+'.'+IntToStr(Minor)+'.'+IntToStr(Release)+'.'+IntToStr(Build);
      end;
    end;
  finally
    FreeMem(PVersionData);
  end;
end;
{$ENDIF}

initialization
{$IfDef MSWINDOWS}
  InpOutLoaded := False;
  BlockInputLoaded := False;
  xInp32 := Nil;
  xOut32 := Nil;
  xBlockInput := Nil;
{$EndIf}

end.
