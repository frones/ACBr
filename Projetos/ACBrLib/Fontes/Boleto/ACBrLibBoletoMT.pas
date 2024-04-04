{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: José M. S. Junior                               }
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

unit ACBrLibBoletoMT;


interface

uses
  Classes, SysUtils, ACBrLibComum;

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function Boleto_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%region Boleto}

function Boleto_ConfigurarDados(const libHandle: PLibHandle; eArquivoIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_IncluirTitulos(const libHandle: PLibHandle; eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_LimparLista(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_TotalTitulosLista(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Imprimir(const libHandle: PLibHandle; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ImprimirBoleto(const libHandle: PLibHandle; eIndice: longint; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarPDF(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SalvarPDF(const libHandle: PLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarPDFBoleto(const libHandle: PLibHandle;
  eIndice: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SalvarPDFBoleto(const libHandle: PLibHandle; eIndice: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarHTML(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarRemessa(const libHandle: PLibHandle; eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_LerRetorno(const libHandle: PLibHandle; eDir, eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ObterRetorno(const libHandle: PLibHandle; eDir, eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarEmail(const libHandle: PLibHandle; ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarEmailBoleto(const libHandle: PLibHandle; eIndice: longint; ePara, eAssunto,
  eMensagem, eCC: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SetDiretorioArquivo(const libHandle: PLibHandle; eDir, eArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaBancos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaCaractTitulo(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaOcorrencias(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaOcorrenciasEX(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_TamNossoNumero(const libHandle: PLibHandle; eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_CodigosMoraAceitos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SelecionaBanco(const libHandle: PLibHandle; eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_MontarNossoNumero(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_RetornaLinhaDigitavel(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_RetornaCodigoBarras(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarBoleto(const libHandle: PLibHandle; eCodigoOperacao: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConsultarTitulosPorPeriodo(const libHandle: PLibHandle; eArquivoIni: PChar;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

implementation

uses
  ACBrLibBoletoBase, ACBrLibConsts;

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function Boleto_Inicializar(var libHandle: PLibHandle; const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(libHandle, TACBrLibBoleto, eArqConfig, eChaveCrypt);
end;

function Boleto_Finalizar(libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(libHandle);
end;

function Boleto_Nome(const libHandle: PLibHandle; const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(libHandle, sNome, esTamanho);
end;

function Boleto_Versao(const libHandle: PLibHandle; const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(libHandle, sVersao, esTamanho);
end;

function Boleto_OpenSSLInfo(const libHandle: PLibHandle; const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(libHandle, sOpenSSLInfo, esTamanho);
end;

function Boleto_UltimoRetorno(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(libHandle, sMensagem, esTamanho);
end;

function Boleto_ConfigImportar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(libHandle, eArqConfig);
end;

function Boleto_ConfigExportar(const libHandle: PLibHandle; const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(libHandle, sMensagem, esTamanho);
end;

function Boleto_ConfigLer(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(libHandle, eArqConfig);
end;

function Boleto_ConfigGravar(const libHandle: PLibHandle; const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(libHandle, eArqConfig);
end;

function Boleto_ConfigLerValor(const libHandle: PLibHandle; const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(libHandle, eSessao, eChave, sValor, esTamanho);
end;

function Boleto_ConfigGravarValor(const libHandle: PLibHandle; const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(libHandle, eSessao, eChave, eValor);
end;

{%endregion}

{%region Boleto}

function Boleto_ConfigurarDados(const libHandle: PLibHandle; eArquivoIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ConfigurarDados(eArquivoIni);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_IncluirTitulos(const libHandle: PLibHandle; eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).IncluirTitulos(eArquivoIni, eTpSaida);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_LimparLista(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).LimparLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_TotalTitulosLista(const libHandle: PLibHandle): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).TotalTitulosLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_Imprimir(const libHandle: PLibHandle; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).Imprimir(eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ImprimirBoleto(const libHandle: PLibHandle; eIndice: longint; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ImprimirBoleto(eIndice, eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarPDF(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).GerarPDF;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SalvarPDF(const libHandle: PLibHandle; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).SalvarPDF(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;


function Boleto_GerarPDFBoleto(const libHandle: PLibHandle; eIndice: longint
  ): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).GerarPDFBoleto(eIndice);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;

  end;
end;

function Boleto_SalvarPDFBoleto(const libHandle: PLibHandle; eIndice: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).SalvarPDFBoleto(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarHTML(const libHandle: PLibHandle): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).GerarHTML;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarRemessa(const libHandle: PLibHandle; eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).GerarRemessa(eDir, eNumArquivo, eNomeArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_LerRetorno(const libHandle: PLibHandle; eDir, eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).LerRetorno(eDir, eNomeArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ObterRetorno(const libHandle: PLibHandle; eDir,
  eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ObterRetorno(eDir, eNomeArq, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarEmail(const libHandle: PLibHandle; ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).EnviarEmail(ePara, eAssunto, eMensagem, eCC);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarEmailBoleto(const libHandle: PLibHandle; eIndice: longint; ePara, eAssunto, eMensagem,
  eCC: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).EnviarEmailBoleto(eIndice, ePara, eAssunto, eMensagem, eCC);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SetDiretorioArquivo(const libHandle: PLibHandle; eDir, eArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).SetDiretorioArquivo(eDir, eArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaBancos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ListaBancos(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaCaractTitulo(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ListaCaractTitulo(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaOcorrencias(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ListaOcorrencias(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaOcorrenciasEX(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ListaOcorrenciasEX(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_TamNossoNumero(const libHandle: PLibHandle; eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).TamNossoNumero(eCarteira, enossoNumero, eConvenio);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_CodigosMoraAceitos(const libHandle: PLibHandle; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).CodigosMoraAceitos(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SelecionaBanco(const libHandle: PLibHandle; eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).SelecionaBanco(eCodBanco);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_MontarNossoNumero(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).MontarNossoNumero(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_RetornaLinhaDigitavel(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).RetornaLinhaDigitavel(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_RetornaCodigoBarras(const libHandle: PLibHandle; eIndice: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).RetornaCodigoBarras(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarBoleto(const libHandle: PLibHandle; eCodigoOperacao: longint;
  const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).EnviarBoleto(eCodigoOperacao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ConsultarTitulosPorPeriodo(const libHandle: PLibHandle;
  eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(libHandle);
    Result := TACBrLibBoleto(libHandle^.Lib).ConsultarTitulosPorPeriodo(eArquivoIni, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

{%endregion}

end.

