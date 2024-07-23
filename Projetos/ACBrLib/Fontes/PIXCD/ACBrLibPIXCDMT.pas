{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }
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

unit ACBrLibPIXCDMT;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum, ACBrPIXBase;

function PIXCD_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: Pchar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Finalizar(libHandle: PLibHandle): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Nome(const libHandle : PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Versao(const libHandle : PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_UltimoRetorno(const libHandle : PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigImportar(const libHandle : PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigExportar(const libHandle : PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigLer(const libHandle : PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigGravar(const libHandle : PLibHandle; const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigLerValor(const libHandle : PLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigGravarValor(const libHandle : PLibHandle; const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_GerarQRCodeEstatico(const libHandle: PLibHandle; AValor: Double; const AinfoAdicional: PChar; const ATxID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarPix(const libHandle: PLibHandle; const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarPixRecebidos(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_SolicitarDevolucaoPix(const libHandle: PLibHandle; AInfDevolucao: PChar; const Ae2eid: PChar; AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarDevolucaoPix(const libHandle: PLibHandle; const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CriarCobrancaImediata(const libHandle: PLibHandle; AInfCobSolicitada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobrancaImediata(const libHandle: PLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobrancasCob(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PChar; ALocationPresente: Boolean; AStatus: longint; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_RevisarCobrancaImediata(const libHandle: PLibHandle; AInfCobRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CancelarCobrancaImediata(const libHandle: PLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CriarCobranca(const libHandle: PLibHandle; AInfCobVSolicitada: PChar; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobranca(const libHandle: PLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobrancasCobV(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PChar; ALocationPresente: Boolean; AStatus: longint; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_RevisarCobranca(const libHandle: PLibHandle; AInfCobVRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CancelarCobranca(const libHandle: PLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

//Matera
function PIXCD_Matera_IncluirConta(const libHandle: PLibHandle; aInfIncluirConta: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarConta(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_InativarConta(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_IncluirChavePix(const libHandle: PLibHandle; aAccountId, aExternalID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarChavePix(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ExcluirChavePix(const libHandle: PLibHandle; aAccountId, aChavePIX: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_GerarQRCode(const libHandle: PLibHandle; aInfQRCode: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarTransacao(const libHandle: PLibHandle; aAccountId, aTransactionID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarSaldoEC(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarExtratoEC(const libHandle: PLibHandle; aAccountId: PChar; aInicio: TDateTime; aFim: TDateTime; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarMotivosDevolucao(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_SolicitarDevolucao(const libHandle: PLibHandle; aInfSolicitarDevolucao: PChar; aAccountId, aTransactionID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_ConsultarAliasRetirada(const libHandle: PLibHandle; aAccountId, aAlias: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Matera_SolicitarRetirada(const libHandle: PLibHandle; aInfSolicitarRetirada: PChar; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

implementation

Uses
  ACBrLibConsts, ACBrLibPIXCDBase;

function PIXCD_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: Pchar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibPIXCD, eArqConfig, eChaveCrypt);
end;

function PIXCD_Finalizar (libHandle: PLibHandle): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
  libHandle := Nil;
end;

function PIXCD_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function PIXCD_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(libHandle, sOpenSSLInfo, esTamanho);
end;

function PIXCD_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function PIXCD_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function PIXCD_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function PIXCD_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function PIXCD_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function PIXCD_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function PIXCD_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function PIXCD_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

function PIXCD_GerarQRCodeEstatico(const libHandle: PLibHandle; AValor: Double; const AinfoAdicional: PChar; const ATxID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).GerarQRCodeEstatico(AValor, AinfoAdicional, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarPix(const libHandle: PLibHandle; const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).ConsultarPix(Ae2eid, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarPixRecebidos(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).ConsultarPixRecebidos(ADataInicio, ADataFim, ATxId, ACpfCnpj, PagAtual, ItensPorPagina, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_SolicitarDevolucaoPix(const libHandle: PLibHandle; AInfDevolucao: PChar; const Ae2eid: PChar; AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).SolicitarDevolucaoPix(AInfDevolucao, Ae2eid, AidDevolucao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarDevolucaoPix(const libHandle: PLibHandle; const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).ConsultarDevolucaoPix(Ae2eid, AidDevolucao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CriarCobrancaImediata(const libHandle: PLibHandle; AInfCobSolicitada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).CriarCobrancaImediata(AInfCobSolicitada, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobrancaImediata(const libHandle: PLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).ConsultarCobrancaImediata(ATxId, ARevisao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobrancasCob(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PChar; ALocationPresente: Boolean; AStatus: longint; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).ConsultarCobrancasCob(ADataInicio, ADataFim, ACpfCnpj, ALocationPresente, AStatus, PagAtual, ItensPorPagina, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_RevisarCobrancaImediata(const libHandle: PLibHandle; AInfCobRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).RevisarCobrancaImediata(AInfCobRevisada, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CancelarCobrancaImediata(const libHandle: PLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).CancelarCobrancaImediata(ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CriarCobranca(const libHandle: PLibHandle; AInfCobVSolicitada: PChar; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).CriarCobranca(AInfCobVSolicitada, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobranca(const libHandle: PLibHandle; const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).ConsultarCobranca(ATxId, ARevisao, sResposta, esTamanho);
  except
        on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobrancasCobV(const libHandle: PLibHandle; ADataInicio: TDateTime; ADataFim: TDateTime; ACpfCnpj: PChar; ALocationPresente: Boolean; AStatus: longint; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).ConsultarCobrancasCobV(ADataInicio, ADataFim, ACpfCnpj, ALocationPresente, AStatus, PagAtual, ItensPorPagina, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_RevisarCobranca(const libHandle: PLibHandle; AInfCobVRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).RevisarCobranca(AInfCobVRevisada, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CancelarCobranca(const libHandle: PLibHandle; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).CancelarCobranca(ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_IncluirConta(const libHandle: PLibHandle; aInfIncluirConta: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraIncluirConta(aInfIncluirConta, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarConta(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarConta(aAccountId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_InativarConta(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraInativarConta(aAccountId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_IncluirChavePix(const libHandle: PLibHandle; aAccountId, aExternalID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraIncluirChavePix(aAccountId, aExternalID, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarChavePix(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarChavePix(aAccountId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ExcluirChavePix(const libHandle: PLibHandle; aAccountId, aChavePIX: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraExcluirChavePix(aAccountId, aChavePIX, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_GerarQRCode(const libHandle: PLibHandle; aInfQRCode: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraGerarQRCode(aInfQRCode, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarTransacao(const libHandle: PLibHandle; aAccountId, aTransactionID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarTransacao(aAccountId, aTransactionID, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarSaldoEC(const libHandle: PLibHandle; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarSaldoEC(aAccountId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarExtratoEC(const libHandle: PLibHandle; aAccountId: PChar; aInicio: TDateTime; aFim: TDateTime; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarExtratoEC(aAccountId, aInicio, aFim, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarMotivosDevolucao(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarMotivosDevolucao(sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_SolicitarDevolucao(const libHandle: PLibHandle; aInfSolicitarDevolucao: PChar; aAccountId, aTransactionID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraSolicitarDevolucao(aInfSolicitarDevolucao, aAccountId, aTransactionID, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_ConsultarAliasRetirada(const libHandle: PLibHandle; aAccountId, aAlias: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraConsultarAliasRetirada(aAccountId, aAlias, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_Matera_SolicitarRetirada(const libHandle: PLibHandle; aInfSolicitarRetirada: PChar; aAccountId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibPIXCD(libHandle^.Lib).MateraSolicitarRetirada(aInfSolicitarRetirada, aAccountId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

end.
