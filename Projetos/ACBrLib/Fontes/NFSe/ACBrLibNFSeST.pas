{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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
function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}
  ;
function NFSE_Nome(const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Versao(const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigImportar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigExportar(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigLer(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigGravar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%region NFSe}
function NFSE_CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_CarregarLoteXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_LimparLista: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Emitir(const aLote: PAnsiChar; aModoEnvio: Integer;  aImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Cancelar(aInfCancelamentoNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_LinkNFSe(aNumeroNFSe: PAnsiChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GerarLote(const aLote: PAnsiChar; aQtdMaximaRps, aModoEnvio: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_GerarToken(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorRps(const ANumeroRps, ASerie, ATipo, ACodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorNumero(const ANumero: PAnsiChar; APagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aNumeroLote: PAnsiChar; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PAnsiChar; aPagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarLinkNFSe(aInfConsultaLinkNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PAnsiChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar):Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ImprimirPDF: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_EnviarEvento(aInfEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarEvento(const aChave: PAnsiChar; aTipoEvento: Integer; aNumSeq: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarDFe(aNSU: Integer; sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterDANFSE(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ConsultarParametros(aTipoParametroMunicipio: Integer; const aCodigoServico: PAnsiChar; aCompetencia: TDateTime; aNumeroBeneficio: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function NFSE_ObterInformacoesProvedor(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%endregion}

implementation

uses
  ACBrLibNFSeBase, ACBrLibConsts;

{%region NFSe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function NFSE_Inicializar(const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibNFSe, eArqConfig, eChaveCrypt);
end;

function NFSE_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
  pLib := nil;
end;

function NFSE_Nome(const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function NFSE_Versao(const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib,sVersao, esTamanho);
end;

function NFSE_OpenSSLInfo(const sOpenSSLInfo: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(pLib, sOpenSSLInfo, esTamanho);
end;

function NFSE_UltimoRetorno(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function NFSE_ConfigImportar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function NFSE_ConfigExportar(const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function NFSE_ConfigLer(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function NFSE_ConfigGravar(const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function NFSE_ConfigLerValor(const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function NFSE_ConfigGravarValor(const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

{%endregion}

function NFSE_CarregarXML(const eArquivoOuXML: PAnsiChar): Integer;
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

function NFSE_CarregarLoteXML(const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).CarregarLoteXML(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_CarregarINI(const eArquivoOuINI: PAnsiChar): Integer;
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

function NFSE_ObterXml(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_GravarXml(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
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

function NFSE_ObterIni(AIndex: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_GravarIni(AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
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

function NFSE_LimparLista: Integer;
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

function NFSE_ObterCertificados(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_Emitir(const aLote: PAnsiChar; aModoEnvio: Integer;  aImprimir: Boolean; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_Cancelar(aInfCancelamentoNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_SubstituirNFSe(const aNumeroNFSe, aSerieNFSe, aCodigoCancelamento, aMotivoCancelamento, aNumeroLote, aCodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_LinkNFSe(aNumeroNFSe: PAnsiChar; const aCodigoVerificacao, aChaveAcesso, aValorServico, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_GerarLote(const aLote: PAnsiChar; aQtdMaximaRps, aModoEnvio: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_GerarToken(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarSituacao(const AProtocolo, ANumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSe_ConsultarLoteRps(const AProtocolo, ANumLote, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSePorRps(const ANumeroRps, ASerie, ATipo, ACodigoVerificacao, sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeporNumero(const ANumero: PAnsiChar; APagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeporPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: integer; aNumeroLote: PAnsiChar; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSePorFaixa(const aNumeroInicial, aNumeroFinal: PAnsiChar; aPagina: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeGenerico(aInfConsultaNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarLinkNFSe(aInfConsultaLinkNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ConsultarLinkNFSe(aInfConsultaLinkNFSe, sResposta, esTamanho);
  except
      on E: EACBrLibException do
       Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
  end;
end;

function NFSE_EnviarEmail(const ePara, eXmlNFSe: PAnsiChar; const AEnviaPDF: boolean; const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar):Integer;
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

function NFSE_Imprimir(const cImpressora: PAnsiChar; nNumCopias: integer; const bGerarPDF, bMostrarPreview, cCancelada: PAnsiChar): Integer;
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

function NFSE_ImprimirPDF: Integer;
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

function NFSE_SalvarPDF(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoPrestadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer ): Integer;
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

function NFSE_ConsultarNFSeServicoPrestadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoPrestadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoPrestadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoTomadoPorNumero(const aNumero: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoTomadoPorPrestador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoTomadoPorTomador(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoTomadoPorPeriodo(aDataInicial, aDataFinal: TDateTime; aPagina: Integer; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSeServicoTomadoPorIntermediario(const aCNPJ, aInscMun: PAnsiChar; aPagina: Integer; aDataInicial, aDataFinal: TDateTime; aTipoPeriodo: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_EnviarEvento(aInfEvento: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarDPSPorChave(const aChaveDPS: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarNFSePorChave(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarEvento(const aChave: PAnsiChar; aTipoEvento: Integer; aNumSeq: Integer; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarDFe(aNSU: Integer; sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ObterDANFSE(const aChaveNFSe: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ConsultarParametros(aTipoParametroMunicipio: Integer; const aCodigoServico: PAnsiChar; aCompetencia: TDateTime; aNumeroBeneficio: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
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

function NFSE_ObterInformacoesProvedor(const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibNFSe(pLib^.Lib).ObterInformacoesProvedor(sResposta, esTamanho);
  except
      on E: EACBrLibException do
         Result := E.Erro;

      on E: Exception do
         Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

end.
