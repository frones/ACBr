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

unit ACBrLibNFSeST;

interface

uses
  Classes, SysUtils, Forms,
  ACBrLibComum;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}
  ;
function NFSE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%region NFSe}
function NFSE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Emitir(const aLote: PChar; aModoEnvio: longint;  aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Cancelar(aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_LinkNFSe(aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GerarLote(const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GerarToken(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorRps(const ANumeroRps, ASerie, ATipo, ACodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorNumero(const ANumero:PChar; APagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Imprimir(const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_EnviarEvento(aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarEvento(const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarDFe(aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterDANFSE(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarParametros(aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%endregion}

implementation

uses
  ACBrLibNFSeBase, ACBrLibConsts;

{%region NFSe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibNFSe, eArqConfig, eChaveCrypt);
end;

function NFSE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
  pLib := nil;
end;

function NFSE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function NFSE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib,sVersao, esTamanho);
end;

function NFSE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function NFSE_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function NFSE_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function NFSE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function NFSE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function NFSE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

{%endregion}

function NFSE_CarregarXML(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).CarregarXML(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_CarregarINI(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).CarregarINI(eArquivoOuINI);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ObterXml(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ObterXml(AIndex, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_GravarXml(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).GravarXml(AIndex, eNomeArquivo, ePathArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ObterIni(AIndex: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ObterIni(AIndex, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_GravarIni(AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).GravarIni(AIndex, eNomeArquivo, ePathArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_LimparLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).LimparLista;
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ObterCertificados(sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_Emitir(const aLote: PChar; aModoEnvio: longint;  aImprimir: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).Emitir(aLote, aModoEnvio, aImprimir, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_Cancelar(aInfCancelamentoNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).Cancelar(aInfCancelamentoNFSe, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).SubstituirNFSe(aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_LinkNFSe(aNumeroNFSe: PChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).LinkNFSe(aNumeroNFSe, aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_GerarLote(const aLote: PChar; aQtdMaximaRps, aModoEnvio: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).GerarLote(aLote, aQtdMaximaRps, aModoEnvio, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_GerarToken(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result:= TACBrLibNFSe(pLib^.Lib).GerarToken(sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarSituacao(AProtocolo, ANumLote, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSe_ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarLoteRps(AProtocolo, ANumLote, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSePorRps(const ANumeroRps, ASerie, ATipo, ACodigoVerificacao, sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSePorRps(ANumeroRps, ASerie, ATipo, ACodigoVerificacao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeporNumero(const ANumero: PChar; APagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeporNumero(ANumero, APagina, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeporPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: integer; aNumeroLote: PChar; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSePorPeriodo(aDataInicial, aDataFinal, aPagina, aNumeroLote, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
    end;
end;

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PChar; aPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSePorFaixa(aNumeroInicial, aNumeroFinal, aPagina, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeGenerico(aInfConsultaNFSe, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).EnviarEmail(ePara, eXmlNFSe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_Imprimir(const cImpressora: PChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PChar): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).Imprimir(cImpressora, nNumCopias, bGerarPDF, bMostrarPreview, cCancelada);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ImprimirPDF: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ImprimirPDF;
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).SalvarPDF(sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint ): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoPrestadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoPrestadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoPrestadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoTomadoPorNumero(aNumero, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoTomadoPorPrestador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoTomadoPorTomador(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: longint; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal, aPagina, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PChar; aPagina: longint; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: longint; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSeServicoTomadoPorIntermediario(aCNPJ, aInscMun, aPagina, aDataInicial, aDataFinal, aTipoPeriodo, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_EnviarEvento(aInfEvento: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).EnviarEvento(aInfEvento, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarDPSPorChave(aChaveDPS, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarNFSePorChave(aChaveNFSe, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarEvento(const aChave: PChar; aTipoEvento: longint; aNumSeq: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarEvento(aChave, aTipoEvento, aNumSeq, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarDFe(aNSU: longint; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarDFe(aNSU, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ObterDANFSE(const aChaveNFSe: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ObterDANFSE(aChaveNFSe, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_ConsultarParametros(aTipoParametroMunicipio: longint; const aCodigoServico: PChar; aCompetencia: TDateTime; aNumeroBeneficio: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarParametros(aTipoParametroMunicipio, aCodigoServico, aCompetencia, aNumeroBeneficio, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

end.
