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

unit ACBrLibCTeMT;

interface

uses
  Classes, SysUtils, ACBrLibComum;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CTE_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region CTe}
function CTE_CarregarXML(const libHandle: PLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_CarregarINI(const libHandle: PLibHandle; const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ObterXml(const libHandle: PLibHandle; AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_GravarXml(const libHandle: PLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ObterIni(const libHandle: PLibHandle; AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_GravarIni(const libHandle: PLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_CarregarEventoXML(const libHandle: PLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_CarregarEventoINI(const libHandle: PLibHandle; const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_LimparLista(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_LimparListaEventos(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Assinar(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Validar(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ValidarRegrasdeNegocios(const libHandle: PLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_VerificarAssinatura(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_GerarChave(const libHandle: PLibHandle; ACodigoUF, ACodigoNumerico, AModelo,
  ASerie, ANumero, ATpEmi: longint; AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ObterCertificados(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_GetPath(const libHandle: PLibHandle; ATipo: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_GetPathEvento(const libHandle: PLibHandle; ACodEvento: PChar; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function CTE_StatusServico(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Consultar(const libHandle: PLibHandle; const eChaveOuCTe: PChar; AExtrairEventos: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Inutilizar(const libHandle: PLibHandle; const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Enviar(const libHandle: PLibHandle; ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConsultarRecibo(const libHandle: PLibHandle; ARecibo: PChar;
    const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Cancelar(const libHandle: PLibHandle; const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_EnviarEvento(const libHandle: PLibHandle; idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ConsultaCadastro(const libHandle: PLibHandle; cUF, nDocumento: PChar; nIE: boolean;
    const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_DistribuicaoDFePorUltNSU(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_DistribuicaoDFe(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eultNSU, eArquivoOuXML: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_DistribuicaoDFePorNSU(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_DistribuicaoDFePorChave(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_EnviarEmail(const libHandle: PLibHandle; const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_EnviarEmailEvento(const libHandle: PLibHandle; const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_Imprimir(const libHandle: PLibHandle; const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ImprimirPDF(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_SalvarPDF(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ImprimirEvento(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ImprimirEventoPDF(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_SalvarEventoPDF(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ImprimirInutilizacao(const libHandle: PLibHandle; const eArquivoXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function CTE_ImprimirInutilizacaoPDF(const libHandle: PLibHandle; const eArquivoXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibCTeBase, ACBrLibConsts;

{%region CTe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function CTE_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibCTe, eArqConfig, eChaveCrypt);
end;

function CTE_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
end;

function CTE_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function CTE_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function CTE_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function CTE_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function CTE_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function CTE_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function CTE_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function CTE_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function CTE_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

{%endregion}

function CTE_CarregarXML(const libHandle: PLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).CarregarXML(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_CarregarINI(const libHandle: PLibHandle; const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).CarregarINI(eArquivoOuINI);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ObterXml(const libHandle: PLibHandle; AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ObterXml(AIndex, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_GravarXml(const libHandle: PLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).GravarXml(AIndex, eNomeArquivo, ePathArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ObterIni(const libHandle: PLibHandle; AIndex: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ObterIni(AIndex, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_GravarIni(const libHandle: PLibHandle; AIndex: longint; const eNomeArquivo, ePathArquivo: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).GravarIni(AIndex, eNomeArquivo, ePathArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_CarregarEventoXML(const libHandle: PLibHandle; const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).CarregarEventoXML(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_CarregarEventoINI(const libHandle: PLibHandle; const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).CarregarEventoINI(eArquivoOuINI);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_LimparLista(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).LimparLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_LimparListaEventos(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).LimparListaEventos;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Assinar(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Assinar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Validar(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Validar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ValidarRegrasdeNegocios(const libHandle: PLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ValidarRegrasdeNegocios(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_VerificarAssinatura(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).VerificarAssinatura(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_GerarChave(const libHandle: PLibHandle; ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero,
  ATpEmi: longint; AEmissao, ACNPJCPF: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).GerarChave(ACodigoUF, ACodigoNumerico, AModelo, ASerie, ANumero, ATpEmi,
                                                AEmissao, ACNPJCPF, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ObterCertificados(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ObterCertificados(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_GetPath(const libHandle: PLibHandle; ATipo: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).GetPath(ATipo, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_GetPathEvento(const libHandle: PLibHandle; ACodEvento: PChar; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).GetPathEvento(ACodEvento, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

{%region Servicos}
function CTE_StatusServico(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).StatusServico(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Consultar(const libHandle: PLibHandle; const eChaveOuCTe: PChar; AExtrairEventos: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Consultar(eChaveOuCTe, AExtrairEventos, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Inutilizar(const libHandle: PLibHandle; const ACNPJ, AJustificativa: PChar;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer;
  const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Inutilizar(ACNPJ, AJustificativa, Ano, Modelo, Serie, NumeroInicial, NumeroFinal,
                                                sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Enviar(const libHandle: PLibHandle; ALote: Integer; AImprimir, ASincrono: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Enviar(ALote, AImprimir, ASincrono, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ConsultarRecibo(const libHandle: PLibHandle; ARecibo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ConsultarRecibo(ARecibo, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Cancelar(const libHandle: PLibHandle; const eChave, eJustificativa, eCNPJ: PChar; ALote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Cancelar(eChave, eJustificativa, eCNPJ, ALote, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_EnviarEvento(const libHandle: PLibHandle; idLote: Integer;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).EnviarEvento(idLote, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ConsultaCadastro(const libHandle: PLibHandle; cUF, nDocumento: PChar; nIE: boolean;
    const sResposta: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ConsultaCadastro(cUF, nDocumento, nIE, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_DistribuicaoDFePorUltNSU(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eultNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).DistribuicaoDFePorUltNSU(AcUFAutor, eCNPJCPF, eultNSU, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_DistribuicaoDFe(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eultNSU, eArquivoOuXML: PChar;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).DistribuicaoDFe(AcUFAutor, eCNPJCPF, eultNSU, eArquivoOuXML, sResposta, esTamanho);
  except
    on E: EACBrLibException do
     Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_DistribuicaoDFePorNSU(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, eNSU: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).DistribuicaoDFePorNSU(AcUFAutor, eCNPJCPF, eNSU, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_DistribuicaoDFePorChave(const libHandle: PLibHandle; const AcUFAutor: integer; eCNPJCPF, echCTe: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).DistribuicaoDFePorChave(AcUFAutor, eCNPJCPF, echCTe, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_EnviarEmail(const libHandle: PLibHandle; const ePara, eChaveCTe: PChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).EnviarEmail(ePara, eChaveCTe, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_EnviarEmailEvento(const libHandle: PLibHandle; const ePara, eChaveEvento, eChaveCTe: PChar;
  const AEnviaPDF: Boolean; const eAssunto, eCC, eAnexos, eMensagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).EnviarEmailEvento(ePara, eChaveEvento, eChaveCTe, AEnviaPDF,
                                                       eAssunto, eCC, eAnexos, eMensagem);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_Imprimir(const libHandle: PLibHandle; const cImpressora: PChar; nNumCopias: Integer; const cProtocolo,
  bMostrarPreview: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).Imprimir(cImpressora, nNumCopias, cProtocolo, bMostrarPreview);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ImprimirPDF(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ImprimirPDF;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_SalvarPDF(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).SalvarPDF(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ImprimirEvento(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ImprimirEvento(eArquivoXmlCTe, eArquivoXmlEvento);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ImprimirEventoPDF(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ImprimirEventoPDF(eArquivoXmlCTe, eArquivoXmlEvento);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_SalvarEventoPDF(const libHandle: PLibHandle; const eArquivoXmlCTe, eArquivoXmlEvento: PChar; sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).SalvarEventoPDF(eArquivoXmlCTe, eArquivoXmlEvento, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ImprimirInutilizacao(const libHandle: PLibHandle; const eArquivoXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ImprimirInutilizacao(eArquivoXml);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function CTE_ImprimirInutilizacaoPDF(const libHandle: PLibHandle; const eArquivoXml: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibCTe(libHandle^.Lib).ImprimirInutilizacaoPDF(eArquivoXml);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

end.
