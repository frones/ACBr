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
  Classes, SysUtils,
  ACBrLibConfig;

type

  { TLibRetorno }

  TLibRetorno = record
    Codigo: Integer;
    Mensagem: String;
  end;

  { TACBrLib }

  TACBrLib = class
  private
    FRetorno: TLibRetorno;
    FLogNome: String;
    FLogData: TDate;

  protected
    fpNome: String;
    fpVersao: String;
    fpConfig: TLibConfig;

    procedure Inicializar; virtual;
    procedure CriarConfiguracao(ArqConfig: String = ''; ChaveCrypt: AnsiString = ''); virtual;
    procedure Executar; virtual;
    procedure Finalizar; virtual;

    function CalcularNomeArqLog: String; virtual;
  public
    constructor Create(ArqConfig: String = ''; ChaveCrypt: AnsiString = '');
    destructor Destroy; override;

    procedure GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean = False);

    function SetRetorno(const ACodigo: Integer; const AMensagem: String = ''): Integer;
    property Retorno: TLibRetorno read FRetorno;

    property Config: TLibConfig read fpConfig;

    property Nome: String read fpNome;
    property Versao: String read fpVersao;
  end;

  TACBrLibClass = class of TACBrLib;

{%region Declaração da funções externas}

{%region Constructor/Destructor}
function LIB_Inicializar(const eArqConfig, eChaveCrypt: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Versao/Retorno}
function LIB_NomeEVersao(const sNome, sVersao: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_UltimoRetorno(const sMensagem: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Ler/Gravar Config }
function LIB_ConfigLer(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_ConfigGravar(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

{%region Funcoes auxiliares}
function VerificarInicializacao: Boolean;

// Le um arquivo em Disco e retorna o seu conteúdo //
function LerArquivoParaString(AArquivo: String): AnsiString;

function StringToB64Crypt(AString: String; AChave: AnsiString = ''): String;
function B64CryptToString(ABase64Str: String; AChave: AnsiString = ''): String;

function StringEhXML(AString: String): Boolean;
function StringEhArquivo(AString: String): Boolean;

{%endregion}

var
  pLib: TACBrLib;
  pLibClass: TACBrLibClass;

implementation

uses
  strutils,
  synacode, synautil,
  ACBrConsts, ACBrUtil,
  ACBrLibConsts;


{%region Funcoes auxiliares }

function VerificarInicializacao: Boolean;
begin
  if not Assigned(pLib) then
    Result := (LIB_Inicializar('', '') = ErrOK)
  else
  begin
    pLib.SetRetorno(ErrOK);
    Result := True;
  end;
end;

function LerArquivoParaString(AArquivo: String): AnsiString;
var
  FS: TFileStream;
begin
  if not FileExists(AArquivo) then
    raise Exception.Create(Format(SErrArquivoNaoExiste, [AArquivo]));

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

{%endregion}

{ TACBrLib }

constructor TACBrLib.Create(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  inherited Create;

  CriarConfiguracao(ArqConfig, ChaveCrypt);
  Inicializar;
  Executar;
end;

destructor TACBrLib.Destroy;
begin
  Finalizar;
  inherited Destroy;
end;

procedure TACBrLib.Inicializar;
begin
  GravarLog('Inicializar', logCompleto);
  with Retorno do
  begin
    Codigo := 0;
    Mensagem := '';
  end;

  FLogData := 0;
  FLogNome := '';

  fpNome := CLibNome;
  fpVersao := CLibVersao;
end;

procedure TACBrLib.Finalizar;
begin
  GravarLog('Finalizar', logCompleto);
  fpConfig.Free;
end;

procedure TACBrLib.CriarConfiguracao(ArqConfig: String; ChaveCrypt: AnsiString);
begin
  fpConfig := TLibConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLib.Executar;
begin
  GravarLog('Executar', logCompleto);
  fpConfig.Ler;
end;

function TACBrLib.CalcularNomeArqLog: String;
begin
  if (Date <> FLogData) then
  begin
    if EstaVazio(Nome) then
      raise Exception.Create(SErrLibSemNome);

    FLogData := Date;
    FLogNome := fpConfig.Log.Path + PathDelim + Nome + '-' + DtoS(FLogData) + '.log';
  end;

  Result := FLogNome;
end;

procedure TACBrLib.GravarLog(AMsg: String; NivelLog: TNivelLog; Traduzir: Boolean);
var
  NomeArq: String;
begin
  if (not Assigned(fpConfig)) or (NivelLog > fpConfig.Log.Nivel) then
    Exit;

  NomeArq := CalcularNomeArqLog;
  WriteLog(NomeArq, FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) + ' - ' + AMsg, Traduzir);
end;

function TACBrLib.SetRetorno(const ACodigo: Integer; const AMensagem: String): Integer;
begin
  Result := ACodigo;
  FRetorno.Codigo := ACodigo;
  FRetorno.Mensagem := AMensagem;
end;


{%region Constructor/Destructor}

function LIB_Inicializar(const eArqConfig, eChaveCrypt: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArqConfig, ChaveCrypt: String;
begin
  try
    ArqConfig := string(eArqConfig);
    ChaveCrypt := string(eChaveCrypt);

    if (pLib = nil) then
    begin
      pLib := pLibClass.Create(ArqConfig, ChaveCrypt);
      pLib.GravarLog('LIB_Inicializar( ' + ArqConfig + ', ' + StringOfChar('*', Length(ChaveCrypt)) + ' )', logSimples);
      pLib.GravarLog(pLib.Nome + ' - ' + pLib.Versao, logSimples);
    end;

    Result := pLib.SetRetorno(ErrOK);
  except
    on E: Exception do
    begin
      if Assigned(pLib) then
        FreeAndNil(pLib);

      Result := ErrLibNaoInicializada;
    end
  end;
end;

function LIB_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    if (pLib <> nil) then
    begin
      pLib.GravarLog('LIB_Finalizar', logSimples);
      FreeAndNil(pLib);
    end;

    Result := ErrOK;
  except
    on E: Exception do
    begin
      Result := ErrLibNaoFinalizada;
    end
  end;
end;

{%endregion}

{%region Versao/Retorno}

function LIB_NomeEVersao(const sNome, sVersao: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  StrPCopy(sNome, pLib.Nome);
  StrPCopy(sVersao, pLib.Versao);

  Result := ErrOK;
end;

function LIB_UltimoRetorno(const sMensagem: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  if not Assigned(pLib) then
  begin
    StrPCopy(sMensagem, SErrLibNaoInicializada);
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  StrPCopy(sMensagem, pLib.Retorno.Mensagem);
  Result := pLib.Retorno.Codigo;
end;

{%endregion}

{%region Ler/Gravar Config }

function LIB_ConfigLer(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArqConfig: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  ArqConfig := string(eArqConfig);

  try
    pLib.Config.NomeArquivo := ArqConfig;
    pLib.Config.Ler;
    Result := pLib.SetRetorno(ErrOK);
  except
    on E: Exception do
    begin
      Result := pLib.SetRetorno(ErrConfigLer, E.Message);
    end
  end;
end;

function LIB_ConfigGravar(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ArqConfig: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  ArqConfig := string(eArqConfig);

  try
    if NaoEstaVazio(ArqConfig) then
      pLib.Config.NomeArquivo := ArqConfig;

    pLib.Config.Gravar;
    Result := pLib.SetRetorno(ErrOK);
  except
    on E: Exception do
    begin
      Result := pLib.SetRetorno(ErrConfigGravar, E.Message);
    end
  end;
end;

function LIB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Sessao, Chave, Valor: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  try
    Sessao := string(eSessao);
    Chave := string(eChave);

    Valor := pLib.Config.LerValor(Chave, Sessao);
    StrPCopy(sValor, Valor);
    Result := ErrOK;
  except
    on E: Exception do
    begin
      Result := pLib.SetRetorno(ErrConfigLer, E.Message);
    end
  end;
end;

function LIB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Sessao, Chave, Valor: String;
begin
  if not VerificarInicializacao then
  begin
    Result := ErrLibNaoInicializada;
    Exit;
  end;

  try
    Sessao := string(eSessao);
    Chave := string(eChave);
    Valor := string(eValor);

    pLib.Config.GravarValor(Chave, Sessao, Valor);
    Result := ErrOK;
  except
    on E: Exception do
    begin
      Result := pLib.SetRetorno(ErrConfigGravar, E.Message);
    end
  end;
end;

{%endregion}

exports
  //Inicialiar Finalizar
  LIB_Inicializar,
  LIB_Finalizar,

  // Versao Retorno
  LIB_NomeEVersao,
  LIB_UltimoRetorno,

  // Configurações
  LIB_ConfigLer,
  LIB_ConfigLerValor,
  LIB_ConfigGravar,
  LIB_ConfigGravarValor;

initialization
  pLib := nil;
  pLibClass := TACBrLib;

finalization
  if Assigned(pLib) then
    FreeAndNil(pLib);

end.
