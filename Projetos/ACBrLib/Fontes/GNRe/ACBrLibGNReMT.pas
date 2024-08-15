{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrLibGNReMT;

interface

uses
  Classes, SysUtils, ACBrLibComum;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function GNRE_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Finalizar(libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Nome(const libHandle: PLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Versao(const libHandle: PLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region GNRe}
function GNRE_LimparLista(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarXML(const libHandle: PLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarINI(const libHandle: PLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ObterXml(const libHandle: PLibHandle; AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_GravarXml(const libHandle: PLibHandle; AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_LimparListaGuiaRetorno(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_CarregarGuiaRetorno(const libHandle: PLibHandle; const eArquivoOuXml: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Assinar(const libHandle: PLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Validar(const libHandle: PLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_VerificarAssinatura(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ObterCertificados(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Servicos}
function GNRE_Enviar(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Consultar(const libHandle: PLibHandle; const eUF: PAnsiChar; const AReceita: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_EnviarEmail(const libHandle: PLibHandle; const ePara, eArquivoOuXml: PAnsiChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_Imprimir(const libHandle: PLibHandle; const eNomeImpressora, eMostrarPreview: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function GNRE_ImprimirPDF(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibGNReBase;

{%region GNRe}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function GNRE_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibGNRe, eArqConfig, eChaveCrypt);
end;

function GNRE_Finalizar(libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
end;

function GNRE_Nome(const libHandle: PLibHandle; const sNome: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function GNRE_Versao(const libHandle: PLibHandle; const sVersao: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function GNRE_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function GNRE_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function GNRE_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PAnsiChar; var esTamanho: Integer): Integer;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function GNRE_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function GNRE_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function GNRE_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PAnsiChar; sValor: PAnsiChar;
  var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function GNRE_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

{%endregion}

function GNRE_LimparLista(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).LimparLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_CarregarXML(const libHandle: PLibHandle; const eArquivoOuXML: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).CarregarXML(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_CarregarINI(const libHandle: PLibHandle; const eArquivoOuINI: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).CarregarINI(eArquivoOuINI);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_ObterXml(const libHandle: PLibHandle; AIndex: Integer; const sResposta: PAnsiChar;
  var esTamanho: Integer): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).ObterXml(AIndex, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_GravarXml(const libHandle: PLibHandle; AIndex: Integer; const eNomeArquivo, ePathArquivo: PAnsiChar): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).GravarXml(AIndex, eNomeArquivo, ePathArquivo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_LimparListaGuiaRetorno(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).LimparListaGuiaRetorno;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_CarregarGuiaRetorno(const libHandle: PLibHandle; const eArquivoOuXml: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).CarregarGuiaRetorno(eArquivoOuXML);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_Assinar(const libHandle: PLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).Assinar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_Validar(const libHandle: PLibHandle): Integer; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).Validar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_VerificarAssinatura(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).VerificarAssinatura(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_ObterCertificados(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).ObterCertificados(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

{%region Servicos}
function GNRE_Enviar(const libHandle: PLibHandle; const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).Enviar(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_Consultar(const libHandle: PLibHandle; const eUF: PAnsiChar; const AReceita: Integer;
  const sResposta: PAnsiChar; var esTamanho: Integer): Integer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).Consultar(eUF, AReceita, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_EnviarEmail(const libHandle: PLibHandle; const ePara, eArquivoOuXml: PAnsiChar; const AEnviaPDF: Boolean;
  const eAssunto, eCC, eAnexos, eMensagem: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).EnviarEmail(ePara, eArquivoOuXml, AEnviaPDF, eAssunto, eCC, eAnexos, eMensagem);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_Imprimir(const libHandle: PLibHandle; const eNomeImpressora, eMostrarPreview: PAnsiChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).Imprimir(eNomeImpressora, eMostrarPreview);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function GNRE_ImprimirPDF(const libHandle: PLibHandle): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibGNRe(libHandle^.Lib).ImprimirPDF;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

end.
