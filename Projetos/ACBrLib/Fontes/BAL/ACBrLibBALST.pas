{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

unit ACBrLibBALST;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibBALBase;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function BAL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Balança}
function BAL_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_LePesoStr(MillisecTimeOut: Integer; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_SolicitarPeso: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_UltimoPesoLido(var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_UltimoPesoLidoStr(sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function BAL_InterpretarRespostaPesoStr(eResposta: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;


{%region BAL}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function BAL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibBAL, eArqConfig, eChaveCrypt);
end;

function BAL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
end;

function BAL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function BAL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function BAL_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(pLib, sOpenSSLInfo, esTamanho);
end;

function BAL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function BAL_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function BAL_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function BAL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function BAL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function BAL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function BAL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

{%endregion}

{%region Balanço}

function BAL_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).Ativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).Desativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).LePeso(MillisecTimeOut, Peso);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_LePesoStr(MillisecTimeOut: Integer; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Peso: Double;
  Resposta: string;
begin
  try
    VerificarLibInicializada(pLib);
    Peso := 0;
    Result := TACBrLibBAL(pLib^.Lib).LePeso(MillisecTimeOut, Peso);
    if Result = 0 then
    begin
      Resposta := FloatToString(Peso);
      TACBrLibBAL(pLib^.Lib).MoverStringParaPChar(Resposta, sValor, esTamanho);
      TACBrLibBAL(pLib^.Lib).SetRetorno(Result, Resposta);
    end;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_SolicitarPeso: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).SolicitarPeso;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_UltimoPesoLido(var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).UltimoPesoLido(Peso);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_UltimoPesoLidoStr(sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Peso: Double;
  Resposta: string;
begin
  try
    VerificarLibInicializada(pLib);
    Peso := 0;
    Result := TACBrLibBAL(pLib^.Lib).UltimoPesoLido(Peso);
    if Result = 0 then
    begin
      Resposta := FloatToString(Peso);
      TACBrLibBAL(pLib^.Lib).MoverStringParaPChar(Resposta, sValor, esTamanho);
      TACBrLibBAL(pLib^.Lib).SetRetorno(Result, Resposta);
    end;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBAL(pLib^.Lib).InterpretarRespostaPeso(eResposta, Peso);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function BAL_InterpretarRespostaPesoStr(eResposta: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
Var
  Peso: Double;
  Resposta: string;
begin
  try
    VerificarLibInicializada(pLib);
    Peso := 0;
    Result := TACBrLibBAL(pLib^.Lib).InterpretarRespostaPeso(eResposta, Peso);
    if Result = 0 then
    begin
      Resposta := FloatToString(Peso);
      TACBrLibBAL(pLib^.Lib).MoverStringParaPChar(Resposta, sValor, esTamanho);
      TACBrLibBAL(pLib^.Lib).SetRetorno(Result, Resposta);
    end;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

{%endregion}

end.

