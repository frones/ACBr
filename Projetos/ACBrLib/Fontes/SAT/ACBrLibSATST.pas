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

unit ACBrLibSATST;

interface

uses
  Classes, SysUtils, typinfo;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_Finalizar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigImportar(const eArqConfig: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigLer(const eArqConfig: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigGravar(const eArqConfig: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Ativar}
function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_DesInicializar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Funções SAT}
function SAT_AtivarSAT(CNPJvalue: PChar; cUF: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar; opcao: integer; novoCodigo: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
function SAT_ConsultarUltimaSessaoFiscal(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConsultarStatusOperacional(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ConsultarNumeroSessao(cNumeroDeSessao: integer; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_SetNumeroSessao(cNumeroDeSessao: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_AtualizarSoftwareSAT(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ComunicarCertificadoICPBRASIL(certificado: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ExtrairLogs(eArquivo: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_TesteFimAFim(eArquivoXmlVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region CFe}
function SAT_CriarCFe(eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_CriarEnviarCFe(eArquivoIni: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ValidarCFe(eArquivoXml: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_EnviarCFe(eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_CancelarCFe(eArquivoXml: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Impressão}
function SAT_ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_SalvarPDF(const sResposta: PChar; var esTamanho: longint)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_GerarImpressaoFiscalMFe(eArqXMLVenda: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function SAT_EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibComum, ACBrLibSATBase;

{ ACBrLibSAT }

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function SAT_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibSAT, eArqConfig, eChaveCrypt);
end;

function SAT_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
end;

function SAT_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function SAT_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function SAT_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function SAT_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function SAT_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function SAT_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function SAT_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function SAT_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function SAT_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

{%endregion}

{%region Ativar}
function SAT_InicializarSAT: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).InicializarSAT;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_DesInicializar: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).DesInicializar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;
{%endregion}

{%region Funções SAT}
function SAT_AtivarSAT(CNPJvalue: PChar; cUF: longint;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).AtivarSAT(CNPJvalue, cUF, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_AssociarAssinatura(CNPJvalue, assinaturaCNPJs: PChar;
  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).AssociarAssinatura(CNPJvalue, assinaturaCNPJs, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_BloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).BloquearSAT(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_DesbloquearSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).DesbloquearSAT(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia: PChar;
  opcao: integer; novoCodigo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).TrocarCodigoDeAtivacao(codigoDeAtivacaoOuEmergencia, opcao, novoCodigo,
                                                            sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ConsultarSAT(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ConsultarSAT(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ConsultarUltimaSessaoFiscal(const sResposta: PChar; var esTamanho: longint): longint;
        {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ConsultarUltimaSessaoFiscal(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;


function SAT_ConsultarStatusOperacional(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ConsultarStatusOperacional(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ConsultarNumeroSessao(cNumeroDeSessao: integer; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ConsultarNumeroSessao(cNumeroDeSessao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_SetNumeroSessao(cNumeroDeSessao: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).SetNumeroSessao(cNumeroDeSessao);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_AtualizarSoftwareSAT(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).AtualizarSoftwareSAT(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ComunicarCertificadoICPBRASIL(certificado: PChar;  const sResposta: PChar; var esTamanho: longint): longint;
{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ComunicarCertificadoICPBRASIL(certificado, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ExtrairLogs(eArquivo: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ExtrairLogs(eArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_TesteFimAFim(eArquivoXmlVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).TesteFimAFim(eArquivoXmlVenda, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente: PChar; const sResposta: PChar; var esTamanho: longint): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).GerarAssinaturaSAT(eCNPJSHW, eCNPJEmitente, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

{%region CFe}

function SAT_CriarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).CriarCFe(eArquivoIni, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_CriarEnviarCFe(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).CriarEnviarCFe(eArquivoIni, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ValidarCFe(eArquivoXml: PChar):longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ValidarCFe(eArquivoXml);
  except
    on E: EACBrLibException do
       Result := E.Erro;

    on E: Exception do
       Result := ErrExecutandoMetodo;
  end;
end;

function SAT_EnviarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).EnviarCFe(eArquivoXml, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_CancelarCFe(eArquivoXml: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).CancelarCFe(eArquivoXml, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

{%region Impressão}

function SAT_ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora: PChar)
  : longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ImprimirExtratoVenda(eArqXMLVenda, eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ImprimirExtratoResumido(eArqXMLVenda, eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).ImprimirExtratoCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_SalvarPDF(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).SalvarPDF(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_GerarImpressaoFiscalMFe(eArqXMLVenda: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).GerarImpressaoFiscalMFe(eArqXMLVenda, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo: PChar; const sResposta: PChar;
                                  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).GerarPDFExtratoVenda(eArqXMLVenda, eNomeArquivo, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo: PChar; const sResposta: PChar;
  var esTamanho: longint): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).GerarPDFCancelamento(eArqXMLVenda, eArqXMLCancelamento, eNomeArquivo,
                                                          sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function SAT_EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
  sCC, eAnexos: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibSAT(pLib^.Lib).EnviarEmail(eArqXMLVenda, sPara, sAssunto, eNomeArquivo, sMensagem,
                                                 sCC, eAnexos);
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
