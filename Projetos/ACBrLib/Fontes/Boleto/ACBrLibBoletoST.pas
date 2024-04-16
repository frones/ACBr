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

unit ACBrLibBoletoST;


interface

uses
  Classes, SysUtils;

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function Boleto_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

{%endregion}

{%region Boleto}

function Boleto_ConfigurarDados(eArquivoIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_IncluirTitulos(eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_LimparLista: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_TotalTitulosLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_Imprimir(eNomeImpressora: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ImprimirBoleto(eIndice: longint; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarPDF: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SalvarPDF(const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarPDFBoleto(eIndice: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SalvarPDFBoleto(eIndice: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarHTML: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_GerarRemessaStream(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_LerRetorno(eDir, eNomeArq: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ObterRetorno(eDir, eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarEmailBoleto(eIndice: longint; ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SetDiretorioArquivo(eDir, eArq: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_TamNossoNumero(eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_SelecionaBanco(eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_EnviarBoleto(eCodigoOperacao: longint; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function Boleto_ConsultarTitulosPorPeriodo(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
   {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};


{%endregion}

implementation

uses
  ACBrLibComum, ACBrLibBoletoBase, ACBrLibConsts;

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}

function Boleto_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibBoleto, eArqConfig, eChaveCrypt);
end;

function Boleto_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
end;

function Boleto_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function Boleto_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function Boleto_OpenSSLInfo(const sOpenSSLInfo: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_OpenSSLInfo(pLib, sOpenSSLInfo, esTamanho);
end;

function Boleto_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function Boleto_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function Boleto_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function Boleto_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function Boleto_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function Boleto_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function Boleto_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;

{%endregion}

{%region Boleto}

function Boleto_ConfigurarDados(eArquivoIni: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ConfigurarDados(eArquivoIni);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_IncluirTitulos(eArquivoIni, eTpSaida: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).IncluirTitulos(eArquivoIni, eTpSaida);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_LimparLista: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).LimparLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_TotalTitulosLista: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).TotalTitulosLista;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_Imprimir(eNomeImpressora: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).Imprimir(eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ImprimirBoleto(eIndice: longint; eNomeImpressora: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ImprimirBoleto(eIndice, eNomeImpressora);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarPDF: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).GerarPDF;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SalvarPDF(const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).SalvarPDF(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarPDFBoleto(eIndice: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).GerarPDFBoleto(eIndice);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SalvarPDFBoleto(eIndice: longint; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).SalvarPDFBoleto(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarHTML: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).GerarHTML;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarRemessa(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).GerarRemessa(eDir, eNumArquivo, eNomeArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_GerarRemessaStream(eDir: PChar; eNumArquivo: longInt; eNomeArq: PChar; const sResposta: PChar; var esTamanho: longint): longint;
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).GerarRemessaStream(eDir, eNumArquivo, eNomeArq, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_LerRetorno(eDir, eNomeArq: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).LerRetorno(eDir, eNomeArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ObterRetorno(eDir, eNomeArq: PChar; const sResposta: PChar;
  var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ObterRetorno(eDir, eNomeArq, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarEmail(ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).EnviarEmail(ePara, eAssunto, eMensagem, eCC);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarEmailBoleto(eIndice: longint; ePara, eAssunto, eMensagem, eCC: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).EnviarEmailBoleto(eIndice, ePara, eAssunto, eMensagem, eCC);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SetDiretorioArquivo(eDir, eArq: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).SetDiretorioArquivo(eDir, eArq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaBancos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ListaBancos(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaCaractTitulo(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ListaCaractTitulo(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaOcorrencias(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ListaOcorrencias(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ListaOcorrenciasEX(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).ListaOcorrenciasEX(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_TamNossoNumero(eCarteira, enossoNumero, eConvenio: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).TamNossoNumero(eCarteira, enossoNumero, eConvenio);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_CodigosMoraAceitos(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).CodigosMoraAceitos(sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_SelecionaBanco(eCodBanco: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).SelecionaBanco(eCodBanco);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_MontarNossoNumero(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).MontarNossoNumero(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_RetornaLinhaDigitavel(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).RetornaLinhaDigitavel(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_RetornaCodigoBarras(eIndice: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).RetornaCodigoBarras(eIndice, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_EnviarBoleto(eCodigoOperacao: longint; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibBoleto(pLib^.Lib).EnviarBoleto(eCodigoOperacao, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function Boleto_ConsultarTitulosPorPeriodo(eArquivoIni: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
      VerificarLibInicializada(pLib);
      Result := TACBrLibBoleto(pLib^.Lib).ConsultarTitulosPorPeriodo(eArquivoIni, sResposta, esTamanho);
    except
      on E: EACBrLibException do
        Result := E.Erro;

      on E: Exception do
        Result := ErrExecutandoMetodo;
    end;
end;


{%endregion}

end.

