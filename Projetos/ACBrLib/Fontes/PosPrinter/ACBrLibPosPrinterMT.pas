{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
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

unit ACBrLibPosPrinterMT;

interface

uses
  Classes, SysUtils,
  ACBrLibPosPrinterBase, ACBrLibComum;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function POS_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Inicializada(const libHandle: PLibHandle): Boolean;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Ativar}
function POS_Ativar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Desativar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Impressão}
function POS_Imprimir(const libHandle: PLibHandle; eString: PChar; PulaLinha, DecodificarTags,
  CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirLinha(const libHandle: PLibHandle; eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirCmd(const libHandle: PLibHandle; eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirTags(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirImagemArquivo(const libHandle: PLibHandle; aPath: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirLogo(const libHandle: PLibHandle; nAKC1, nAKC2, nFatorX, nFatorY: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirCheque(const libHandle: PLibHandle; CodBanco: Integer; const AValor, ADataEmissao, AFavorecido,
  ACidade, AComplemento: PChar; LerCMC7: Boolean; SegundosEspera: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ImprimirTextoCheque(const libHandle: PLibHandle; const X, Y: Integer; const AString: PChar;
  AguardaCheque: Boolean; SegundosEspera: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Diversos}
function POS_TxRx(const libHandle: PLibHandle; eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer;
  WaitForTerminator: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Zerar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_InicializarPos(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_Reset(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_PularLinhas(const libHandle: PLibHandle; NumLinhas: Integer): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_CortarPapel(const libHandle: PLibHandle; Parcial: Boolean): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_AbrirGaveta(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerInfoImpressora(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerStatusImpressora(const libHandle: PLibHandle; Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_RetornarTags(const libHandle: PLibHandle; IncluiAjuda: Boolean; const sResposta: PChar;
                          var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_AcharPortas(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_GravarLogoArquivo(const libHandle: PLibHandle; aPath: PChar; nAKC1, nAKC2: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_ApagarLogo(const libHandle: PLibHandle; nAKC1, nAKC2: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LeituraCheque(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerCMC7(const libHandle: PLibHandle; AguardaCheque: Boolean; SegundosEspera: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_EjetarCheque(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_PodeLerDaPorta(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function POS_LerCaracteristicas(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts;

{%region PosPrinter}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function POS_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibPosPrinter,eArqConfig, eChaveCrypt);
end;

function POS_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
end;

function POS_Inicializada(const libHandle: PLibHandle): Boolean;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicalizada(libHandle);
end;

function POS_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function POS_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function POS_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(libHandle, sOpenSSLInfo, esTamanho);
end;

function POS_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function POS_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function POS_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function POS_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function POS_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function POS_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function POS_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;
{%endregion}

{%region Ativar}
function POS_Ativar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).Ativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_Desativar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).Desativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;
{%endregion}

{%region Impressão}
function POS_Imprimir(const libHandle: PLibHandle; eString: PChar; PulaLinha, DecodificarTags,
  CodificarPagina: Boolean; Copias: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).Imprimir(eString, PulaLinha, DecodificarTags, CodificarPagina, Copias);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirLinha(const libHandle: PLibHandle; eString: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirLinha(eString);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirCmd(const libHandle: PLibHandle; eComando: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirCmd(eComando);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirTags(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirTags;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirImagemArquivo(const libHandle: PLibHandle; aPath: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirImagemArquivo(aPath);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirLogo(const libHandle: PLibHandle; nAKC1, nAKC2, nFatorX, nFatorY: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirLogo(nAKC1, nAKC2, nFatorX, nFatorY);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirCheque(const libHandle: PLibHandle; CodBanco: Integer; const AValor, ADataEmissao,
                AFavorecido, ACidade, AComplemento: PChar; LerCMC7: Boolean; SegundosEspera: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirCheque(CodBanco, AValor, ADataEmissao, AFavorecido, ACidade,
                                                           AComplemento, LerCMC7, SegundosEspera);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ImprimirTextoCheque(const libHandle: PLibHandle; const X, Y: Integer; const AString: PChar;
              AguardaCheque: Boolean; SegundosEspera: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ImprimirTextoCheque(X, Y, AString, AguardaCheque, SegundosEspera);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;
{%endregion}

{%region Diversos}
function POS_TxRx(const libHandle: PLibHandle; eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer;
  WaitForTerminator: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).TxRx(eCmd, BytesToRead, ATimeOut, WaitForTerminator, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_Zerar(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).Zerar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_InicializarPos(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).InicializarPos;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_Reset(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).Reset;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_PularLinhas(const libHandle: PLibHandle; NumLinhas: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).PularLinhas(NumLinhas);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_CortarPapel(const libHandle: PLibHandle; Parcial: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).CortarPapel(Parcial);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_AbrirGaveta(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).AbrirGaveta;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_LerInfoImpressora(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).LerInfoImpressora(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_LerStatusImpressora(const libHandle: PLibHandle; Tentativas: Integer; var status: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).LerStatusImpressora(Tentativas, status);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_RetornarTags(const libHandle: PLibHandle; IncluiAjuda: Boolean; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).RetornarTags(IncluiAjuda, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_AcharPortas(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).AcharPortas(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_GravarLogoArquivo(const libHandle: PLibHandle; aPath: PChar; nAKC1, nAKC2: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).GravarLogoArquivo(aPath, nAKC1, nAKC2);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_ApagarLogo(const libHandle: PLibHandle; nAKC1, nAKC2: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).ApagarLogo(nAKC1, nAKC2);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_LeituraCheque(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).LeituraCheque(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_LerCMC7(const libHandle: PLibHandle; AguardaCheque: Boolean; SegundosEspera: Integer;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).LerCMC7(AguardaCheque, SegundosEspera, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_EjetarCheque(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).EjetarCheque;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_PodeLerDaPorta(const libHandle: PLibHandle): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).PodeLerDaPorta;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function POS_LerCaracteristicas(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPosPrinter(libHandle^.Lib).LerCaracteristicas(sResposta, esTamanho);
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

