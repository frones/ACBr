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

unit ACBrLibPIXCDST;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum;

function PIXCD_Inicializar (const eArqConfig, eChaveCrypt: Pchar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Finalizar: longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Nome (const sNome: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_Versao (const sVersao: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_UltimoRetorno (const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigImportar (const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigExportar (const sMensagem: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigLer (const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigGravar (const eArqConfig: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigLerValor (const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConfigGravarValor (const eSessao, eChave, eValor: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_GerarQRCodeEstatico (AValor: Currency; const AinfoAdicional: PChar; const ATxID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarPix (const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_SolicitarDevolucaoPix(const Ae2eid: PChar; AidDevolucao: PChar; AValor: Currency; ANaturezaDevolucao: longint; ADescricao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint):longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CriarCobrancaImediata(AChavePIX: PChar; ACobrancaExpiracao: longint; ASolicitacaoPagador: PChar; ANomeDevedor: PChar; ACPFCNPJDevedor: PChar; AValor: Currency; APermitirAlterarValor: Boolean; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_RevisarCobrancaImediata(const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_CriarCobranca(AChavePIX: PChar; ADataVencimento: TDateTime; AValidadeAposVencimento: longint; ANomeDevedor:PChar; ACPFCNPJDevedor: PChar; AValorOriginal: Currency; AMultaModalidade: longint; AMultaValorPercentual: Currency; AJurosModalidade: longint; AJurosValorPercentual: Currency; ADescontoModalidade: longint; ADescontoValorPercentual: Currency; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_RevisarCobranca(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function PIXCD_ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

implementation

Uses
  ACBrLibConsts, ACBrLibPIXCDBase;

function PIXCD_Inicializar(const eArqConfig, eChaveCrypt: Pchar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibPIXCD, eArqConfig, eChaveCrypt);
end;

function PIXCD_Finalizar: longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
  pLib := Nil;
end;

function PIXCD_Nome(const sNome: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function PIXCD_Versao(const sVersao: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function PIXCD_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function PIXCD_ConfigImportar(const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function PIXCD_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function PIXCD_ConfigLer(const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function PIXCD_ConfigGravar(const eArqConfig: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function PIXCD_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function PIXCD_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

function PIXCD_GerarQRCodeEstatico(AValor: Currency; const AinfoAdicional: PChar; const ATxID: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibPIXCD(pLib^.Lib).GerarQRCodeEstatico(AValor, AinfoAdicional, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibPIXCD(pLib^.Lib).ConsultarPix(Ae2eid, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibPIXCD(pLib^.Lib).ConsultarPixRecebidos(ADataInicio, ADataFim, ATxId, ACpfCnpj, PagAtual, ItensPorPagina, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_SolicitarDevolucaoPix(const Ae2eid: PChar; AidDevolucao: PChar; AValor: Currency; ANaturezaDevolucao: longint; ADescricao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibPIXCD(pLib^.Lib).SolicitarDevolucaoPix(Ae2eid, AidDevolucao, AValor, ANaturezaDevolucao, ADescricao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).ConsultarDevolucaoPix(Ae2eid, AidDevolucao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CriarCobrancaImediata(AChavePIX: PChar; ACobrancaExpiracao: longint; ASolicitacaoPagador: PChar; ANomeDevedor: PChar; ACPFCNPJDevedor: PChar; AValor: Currency; APermitirAlterarValor: Boolean; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint):longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).CriarCobrancaImediata(AChavePIX, ACobrancaExpiracao, ASolicitacaoPagador, ANomeDevedor, ACPFCNPJDevedor, AValor, APermitirAlterarValor, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).ConsultarCobrancaImediata(ATxId, Revisao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_RevisarCobrancaImediata(const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).RevisarCobrancaImediata(ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_CriarCobranca(AChavePIX: PChar; ADataVencimento: TDateTime; AValidadeAposVencimento: longint; ANomeDevedor:PChar; ACPFCNPJDevedor: PChar; AValorOriginal: Currency; AMultaModalidade: longint; AMultaValorPercentual: Currency; AJurosModalidade: longint; AJurosValorPercentual: Currency; ADescontoModalidade: longint; ADescontoValorPercentual: Currency; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result:= TACBrLibPIXCD(libHandle^.Lib).CriarCobranca(AChavePIX, ADataVencimento, AValidadeAposVencimento, ANomeDevedor, ACPFCNPJDevedor, AValorOriginal, AMultaModalidade, AMultaValorPercentual, AJurosModalidade, AJurosValorPercentual, ADescontoModalidade, ADescontoValorPercentual, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_RevisarCobranca(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).RevisarCobranca(AStatus, ATxId, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function PIXCD_ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibPIXCD(pLib^.Lib).ConsultarCobranca(ATxId, Revisao, sResposta, esTamanho);
  except
        on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

end.

