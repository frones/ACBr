{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibComum;

interface

uses
  Classes, SysUtils, fileinfo,
  ACBrLibConfig;

type

  { EACBrLibException }

  EACBrLibException = class(Exception)
  private
    FErro: Integer;
  public
    constructor Create(const err: Integer; const msg: String); reintroduce;

    property Erro: Integer read FErro;
  end;

  { TLibRetorno }

  TLibRetorno = record
    Codigo: Integer;
    Mensagem: String;
  end;

  { TACBrLib }

  TACBrLib = class
  private
    FLogNome: String;
    FLogData: TDate;
    FNome: String;
    FDescricao: String;
    FVersao: String;

    function GetNome: String;
    function GetDescricao: String;
    function GetVersao: String;

  protected
    fpConfig: TLibConfig;
    fpFileVerInfo: TFileVersionInfo;  // Informações da Aplicação e Versão, definida em Opções do Projeto

    procedure Inicializar; virtual;
    procedure CriarConfiguracao(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); virtual;
    procedure Executar; virtual;
    procedure Finalizar; virtual;

    function CalcularNomeArqLog: String; virtual;
  public
    constructor Create(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); virtual;
    destructor Destroy; override;

    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);

    property Config: TLibConfig read fpConfig;

    property Nome: String read GetNome;
    property Versao: String read GetVersao;
    property Descricao: String read GetDescricao;
  end;

  TACBrLibClass = class of TACBrLib;

{%region Declaração da funções externas}

{%region Constructor/Destructor}
function LIB_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
function LIB_Finalizar: longint;
function LIB_Inicalizada: Boolean;
{%endregion}

{%region Versao/Retorno}
function LIB_Nome(const sNome: PChar; var esTamanho: longint): longint;
function LIB_Versao(const sVersao: PChar; var esTamanho: longint): longint;
function LIB_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
{%endregion}

{%region Ler/Gravar Config }
function LIB_ConfigLer(const eArqConfig: PChar): longint;
function LIB_ConfigGravar(const eArqConfig: PChar): longint;
function LIB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
function LIB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
{%endregion}

{%endregion}

{%region Funcoes auxiliares para Funcionamento da Lib}
procedure VerificarLibInicializada;
procedure MoverStringParaPChar(const AString: String; sDest: PChar; var esTamanho: longint);
procedure VerificarArquivoExiste(const NomeArquivo: String);
procedure LiberarLib;
function SetRetorno(const ACodigo: Integer; const AMensagem: String = ''): Integer;
{%endregion}

{%region Funcoes auxiliares Diversas }
// Le um arquivo em Disco e retorna o seu conteúdo //
function LerArquivoParaString(AArquivo: String): AnsiString;

function StringToB64Crypt(AString: String; AChave: AnsiString = ''): String;
function B64CryptToString(ABase64Str: String; AChave: AnsiString = ''): String;

function StringEhXML(AString: String): Boolean;
function StringEhArquivo(AString: String): Boolean;
{%endregion}

var
  pLib: TACBrLib;            // Classe com a Lib
  pLibClass: TACBrLibClass;  // Tipo de Classe a ser Instanciada (definir no LPR de cada nova Lib)
  pLibRetorno: TLibRetorno;  // Último Retorno do método executado

implementation

uses
  strutils, strings,
  synacode, synautil,
  ACBrConsts, ACBrUtil,
  ACBrLibConsts;

{ EACBrLibException }

constructor EACBrLibException.Create(const err: Integer; const msg: String);
begin
  FErro := err;
  inherited Create(msg);
end;

{%endregion}

{ TACBrLib }

constructor TACBrLib.Create(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  inherited Create;

  FLogData := -1;
  FLogNome := '';
  FNome := '';
  FVersao := '';
  FDescricao := '';

  fpFileVerInfo := TFileVersionInfo.Create(Nil);
  fpFileVerInfo.ReadFileInfo;

  CriarConfiguracao(ArqConfig, ChaveCrypt);
end;

destructor TACBrLib.Destroy;
begin
  fpFileVerInfo.Free;
  Finalizar;
  inherited Destroy;
end;

function TACBrLib.GetVersao: String;
begin
  if (FVersao = '') then
    if Assigned(fpFileVerInfo) then
      FVersao := fpFileVerInfo.VersionStrings.Values['FileVersion'];

  Result := FVersao;
end;

function TACBrLib.GetNome: String;
begin
  if (FNome = '') then
    if Assigned(fpFileVerInfo) then
      FNome := fpFileVerInfo.VersionStrings.Values['InternalName'];

  Result := FNome;
end;

function TACBrLib.GetDescricao: String;
begin
  if (FDescricao = '') then
    if Assigned(fpFileVerInfo) then
      FDescricao := fpFileVerInfo.VersionStrings.Values['FileDescription'];

  Result := FDescricao;
end;

procedure TACBrLib.Inicializar;
begin
  with pLibRetorno do
  begin
    Codigo := 0;
    Mensagem := '';
  end;

  FLogData := 0;
  FLogNome := '';

  Config.Ler;
end;

procedure TACBrLib.Finalizar;
begin
  GravarLog('Finalizar', logCompleto);
  FreeAndNil(fpConfig);
end;

procedure TACBrLib.CriarConfiguracao(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  fpConfig := TLibConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLib.Executar;
begin
  GravarLog('Executar', logCompleto);
end;

function TACBrLib.CalcularNomeArqLog: String;
var
  APath: String;
begin
  if (Date <> FLogData) then
  begin
    FLogData := Date;
    APath := PathWithDelim(fpConfig.Log.Path);
    if NaoEstaVazio(APath) then
    begin
      if (not DirectoryExists(APath)) then
        raise EACBrLibException.Create(ErrDiretorioNaoExiste, Format(SErrDiretorioInvalido, [APath]));
    end
    else
      APath := ApplicationPath;

    FLogNome := APath + Self.Nome + '-' + DtoS(FLogData) + '.log';
  end;

  Result := FLogNome;
end;

procedure TACBrLib.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
var
  NomeArq: String;
begin
  if (FLogData < 0) or (Self.Nome = '') or
    (not Assigned(fpConfig)) or (NivelLog > fpConfig.Log.Nivel) then
    Exit;

  NomeArq := CalcularNomeArqLog;
  WriteLog(NomeArq, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) + ' - ' + AMsg, Traduzir);
end;

{%region Constructor/Destructor}

function LIB_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
var
  ArqConfig, ChaveCrypt: String;
begin
  try
    ArqConfig := strpas(eArqConfig);
    ChaveCrypt := strpas(eChaveCrypt);

    if (pLib = nil) then
      pLib := pLibClass.Create(ArqConfig, ChaveCrypt);

    pLib.Inicializar;
    pLib.GravarLog('LIB_Inicializar( ' + ArqConfig + ', ' + StringOfChar('*', Length(ChaveCrypt)) + ' )', logSimples);
    pLib.GravarLog('   ' + pLib.Nome + ' - ' + pLib.Versao, logSimples);

    Result := SetRetorno(ErrOK);
  except
    on E: EACBrLibException do
    begin
      Result := SetRetorno(E.Erro, E.Message);
      LiberarLib;
    end;

    on E: Exception do
    begin
      Result := SetRetorno(ErrLibNaoInicializada, E.Message);
      LiberarLib;
    end
  end;
end;

function LIB_Finalizar: longint;
begin
  try
    if (pLib <> nil) then
    begin
      pLib.GravarLog('LIB_Finalizar', logSimples);
      FreeAndNil(pLib);
    end;

    Result := SetRetorno(ErrOK);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrLibNaoFinalizada, E.Message);
  end;
end;

function LIB_Inicalizada: Boolean;
begin
  Result := (pLib <> nil);
end;

{%endregion}

{%region Versao/Retorno}

function LIB_Nome(const sNome: PChar; var esTamanho: longint): longint;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('LIB_Nome', logNormal);
    MoverStringParaPChar(pLib.Nome, sNome, esTamanho);
    if pLib.Config.Log.Nivel >= logCompleto then
      pLib.GravarLog('   Nome:' + strpas(sNome) + ', len:' + IntToStr(esTamanho), logCompleto, True);
    Result := SetRetorno(ErrOK, pLib.Nome);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function LIB_Versao(const sVersao: PChar; var esTamanho: longint): longint;
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('LIB_Versao', logNormal);
    MoverStringParaPChar(pLib.Versao, sVersao, esTamanho);
    if pLib.Config.Log.Nivel >= logCompleto then
      pLib.GravarLog('   Versao:' + strpas(sVersao) + ', len:' + IntToStr(esTamanho), logCompleto, True);
    Result := SetRetorno(ErrOK, pLib.Versao);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function LIB_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
begin
  try
    if Assigned(pLib) then pLib.GravarLog('LIB_UltimoRetorno', logNormal);
    MoverStringParaPChar(pLibRetorno.Mensagem, sMensagem, esTamanho);
    Result := pLibRetorno.Codigo;
    if Assigned(pLib) and (pLib.Config.Log.Nivel >= logCompleto) then
      pLib.GravarLog('   Codigo:' + IntToStr(Result) + ', Mensagem:' + strpas(sMensagem), logCompleto, True);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

{%endregion}

{%region Ler/Gravar Config }

function LIB_ConfigLer(const eArqConfig: PChar): longint;
var
  ArqConfig: String;
begin
  try
    VerificarLibInicializada;
    ArqConfig := strpas(eArqConfig);
    pLib.GravarLog('LIB_ConfigLer(' + ArqConfig + ')', logNormal);

    if NaoEstaVazio(ArqConfig) then
      pLib.Config.NomeArquivo := ArqConfig;

    pLib.Config.Ler;
    Result := SetRetorno(ErrOK);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrConfigLer, E.Message);
  end;
end;

function LIB_ConfigGravar(const eArqConfig: PChar): longint;
var
  ArqConfig: String;
begin
  try
    VerificarLibInicializada;
    ArqConfig := strpas(eArqConfig);
    pLib.GravarLog('LIB_ConfigGravar(' + ArqConfig + ')', logNormal);

    if NaoEstaVazio(ArqConfig) then
      pLib.Config.NomeArquivo := ArqConfig;

    pLib.Config.Gravar;
    Result := SetRetorno(ErrOK);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrConfigGravar, E.Message);
  end;
end;

function LIB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
var
  Sessao, Chave, Valor: Ansistring;
begin
  try
    VerificarLibInicializada;
    Sessao := Ansistring(eSessao);
    Chave := Ansistring(eChave);
    pLib.GravarLog('LIB_ConfigLerValor(' + Sessao + ', ' + Chave + ')', logNormal);

    Valor := pLib.Config.LerValor(Sessao, Chave);

    MoverStringParaPChar(Valor, sValor, esTamanho);

    if (pLib.Config.Log.Nivel >= logCompleto) then
      pLib.GravarLog('   Valor:' + IfThen(pLib.Config.PrecisaCriptografar(Sessao, Chave),
                                          StringOfChar('*', esTamanho), sValor) +
                                          ', len:' + IntToStr(esTamanho), logCompleto, True);

    Result := SetRetorno(ErrOK, Valor);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrConfigLer, E.Message);
  end;
end;

function LIB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
var
  Sessao, Chave, Valor: Ansistring;
begin
  try
    VerificarLibInicializada;
    Sessao := Ansistring(eSessao);
    Chave := Ansistring(eChave);
    Valor := Ansistring(eValor);

    pLib.GravarLog('LIB_ConfigGravarValor(' + Sessao + ', ' + Chave + ', ' +
                                          IfThen(pLib.Config.PrecisaCriptografar(Sessao, Chave),
                                          StringOfChar('*', Length(Valor)), Valor) + ')', logNormal);

    pLib.Config.GravarValor(Sessao, Chave, Valor);
    Result := SetRetorno(ErrOK);
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrConfigGravar, E.Message);
  end;
end;

{%endregion}

{%region Funcoes auxiliares }

procedure VerificarLibInicializada;
begin
  if not Assigned(pLib) then
  begin
    if (LIB_Inicializar('', '') <> ErrOK) then
      raise EACBrLibException.Create(ErrLibNaoInicializada, SErrLibNaoInicializada);
  end;
end;

procedure MoverStringParaPChar(const AString: String; sDest: PChar; var esTamanho: longint);
var
  AStringLen: Integer;
begin
  AStringLen := Length(AString);

  if (esTamanho <= 0) then
    esTamanho := AStringLen
  else
  begin
    StrLCopy(sDest, PChar(AString), esTamanho);
    esTamanho := AStringLen;
  end;
end;

procedure VerificarArquivoExiste(const NomeArquivo: String);
begin
  if not FileExists(NomeArquivo) then
    raise EACBrLibException.Create(ErrArquivoNaoExiste, Format(SErrArquivoNaoExiste, [NomeArquivo]));
end;

procedure LiberarLib;
begin
  if Assigned(pLib) then
    FreeAndNil(pLib);
end;

function SetRetorno(const ACodigo: Integer; const AMensagem: String): Integer;
begin
  Result := ACodigo;
  pLibRetorno.Codigo := ACodigo;
  pLibRetorno.Mensagem := AMensagem;

  if Assigned(pLib) then
    pLib.GravarLog('   SetRetorno(' + IntToStr(pLibRetorno.Codigo) + ', ' + pLibRetorno.Mensagem + ')', logParanoico);
end;

function LerArquivoParaString(AArquivo: String): AnsiString;
var
  FS: TFileStream;
begin
  VerificarArquivoExiste(AArquivo);
  FS := TFileStream.Create(AArquivo, fmOpenRead or fmShareExclusive);
  try
    FS.Position := 0;
    Result := ReadStrFromStream(FS, FS.Size);
  finally
    FS.Free;
  end;
end;

function StringToB64Crypt(AString: String; AChave: AnsiString = ''): String;
begin
  if (Length(AChave) = 0) then
    AChave := CLibChaveCrypt;

  Result := EncodeBase64(StrCrypt(AString, AChave));
end;

function B64CryptToString(ABase64Str: String; AChave: AnsiString = ''): String;
begin
  if (Length(AChave) = 0) then
    AChave := CLibChaveCrypt;

  Result := StrCrypt(DecodeBase64(ABase64Str), AChave);
end;

function StringEhXML(AString: String): Boolean;
var
  p1: Integer;
begin
  p1 := pos('<', AString);
  Result := (p1 > 0) and (PosEx('>', AString, p1 + 1) > 0);
end;

function StringEhINI(AString: String): Boolean;
var
  p1, p2, p3: Integer;
begin
  p1 := pos('[', AString);
  p2 := PosEx(']', AString, p1 + 1);
  p3 := PosEx(LF, AString, p2 + 1);
  Result := (p1 > 0) and (p2 > 0) and (p3 > 0) and (PosEx('=', AString, p2) > 0);
end;

function StringEhArquivo(AString: String): Boolean;
begin
  Result := (AString <> '') and
    (Length(AString) <= 255) and
    (pos(LF, AString) = 0) and
    (pos('<', AString) = 0) and
    (pos('=', AString) = 0);
end;

initialization
  pLib := nil;
  pLibClass := TACBrLib;
  pLibRetorno.Codigo := 0;
  pLibRetorno.Mensagem := '';

finalization
  LiberarLib;

end.
