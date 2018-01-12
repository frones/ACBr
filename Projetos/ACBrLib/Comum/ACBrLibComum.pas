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
  Classes, SysUtils;

type
  { TLibRetorno }

  TLibRetorno = record
    Codigo: Integer;
    Mensagem: String;
  end;

{%region Declaração da funções}

{%region Versao/Retorno}
function LIB_Versao(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function LIB_UltimoRetorno(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Ler/Gravar Config }
function LIB_LerConfig(const PArquivo: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};

function LIB_GravarConfig(const PArquivo: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
{%endregion}

{%endregion}

{%region Funcoes auxiliares }
procedure LIB_Inicializar( const ANome, AVersao: String; AChave: AnsiString);

Function SetRetorno( const ACodigo: Integer; const AMensagem: String = ''): Integer;

procedure GravarLog( AMsg: String; Traduzir: Boolean = False );

function StringToB64Crypt(AString: String; AChave: AnsiString = ''): String;
function B64CryptToString(ABase64Str: String; AChave: AnsiString = ''): String;
{%endregion}

var
  pLibRetorno: TLibRetorno;
  pLibNome: String;
  pLibVersao: String;
  pLibChaveCrypt: AnsiString;

implementation

uses
  ACBrLibConsts, ACBrLibConfig,
  synacode,
  ACBrUtil;

{%region Funcoes auxiliares }

procedure LIB_Inicializar(const ANome, AVersao: String; AChave: AnsiString);
begin
  pLibNome := ANome;
  pLibVersao := AVersao;
  if Length(AChave) = 0 then
    pLibChaveCrypt := CLibChaveCrypt
  else
    pLibChaveCrypt := AChave;
end;

function SetRetorno(const ACodigo: Integer; const AMensagem: String): Integer;
begin
  Result := ACodigo;
  pLibRetorno.Codigo := ACodigo;
  pLibRetorno.Mensagem := AMensagem;
end;

function CalcularNomeArqLog: String;
begin
  VerificarLibConfigFoiInstaciado;
  if EstaVazio(pLibNome) then
    raise Exception.Create('Erro pLibNome não foi definido');

  Result := pLibConfig.Log.Path + PathDelim + pLibNome + '-' + DtoS(Now) + '.log';
end;

procedure GravarLog(AMsg: String; Traduzir: Boolean);
var
  NomeArq: String;
begin
  NomeArq := CalcularNomeArqLog;

  WriteLog(NomeArq, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + AMsg, Traduzir);
end;

function StringToB64Crypt(AString: String; AChave: AnsiString = ''): String;
begin
  if (Length(AChave) = 0) then
    AChave := pLibChaveCrypt;

  Result := EncodeBase64( StrCrypt(AString, AChave) );
end;

function B64CryptToString(ABase64Str: String; AChave: AnsiString = ''): String;
begin
  if (Length(AChave) = 0) then
    AChave := pLibChaveCrypt;

  Result := StrCrypt( DecodeBase64(ABase64Str), AChave );
end;

{%endregion}

{%region Versao/Retorno}

function LIB_Versao(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  NomeCompleto: String;
begin
  NomeCompleto := Trim(pLibNome + ' ' + pLibVersao);
  StrPCopy(Buffer, NomeCompleto);
  Result := SetRetorno(ErrOK);
end;

function LIB_UltimoRetorno(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  StrPCopy(Buffer, pLibRetorno.Mensagem);
  Result := pLibRetorno.Codigo;
end;

{%endregion}

{%region Ler/Gravar Config }

function LIB_LerConfig(const PArquivo: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
var
  ArqConfStr: String;
begin
  if not Assigned(pLibConfig) then
  begin
    Result := SetRetorno( ErrConfigNaoInicializado, SErrConfNaoInstanciado );
    Exit;
  end;

  ArqConfStr := String(PArquivo);
  if NaoEstaVazio(ArqConfStr) then
  begin
    if not FileExists(ArqConfStr) then
    begin
      Result := SetRetorno( ErrConfigNaoExiste, SErrConfArqNaoEncontrado );
      Exit;
    end;

    pLibConfig.NomeArquivo := ArqConfStr;
  end;

  try
    pLibConfig.Ler;
    Result := SetRetorno(ErrOK);
  except
    on E: Exception do
    begin
      Result := SetRetorno(ErrConfigLer, E.Message);
    end
  end;
end;

function LIB_GravarConfig(const PArquivo: PChar): Integer;
  {$IfDef STDCALL}stdcall{$Else}cdecl{$EndIf};
var
  ArqConfStr: String;
begin
  if not Assigned(pLibConfig) then
  begin
    Result := SetRetorno( ErrConfigNaoInicializado, SErrConfNaoInstanciado );
    Exit;
  end;

  ArqConfStr := String(PArquivo);
  if NaoEstaVazio(ArqConfStr) then
    pLibConfig.NomeArquivo := ArqConfStr;

  try
    pLibConfig.Gravar;
    Result := SetRetorno(ErrOK);
  except
    on E: Exception do
    begin
      Result := SetRetorno(ErrConfigGravar, E.Message);
    end
  end;
end;

{%endregion}

exports
  // Versao Retorno
  LIB_Versao,
  LIB_UltimoRetorno,

  // Configurações
  LIB_LerConfig,
  LIB_GravarConfig;

initialization
  pLibNome := '';
  pLibVersao := '';
  pLibRetorno.Codigo := 0;
  pLibRetorno.Mensagem := '';
  pLibChaveCrypt := CLibChaveCrypt;

end.

