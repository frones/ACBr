{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrLibETQST;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibETQBase;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function ETQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Diversos}
function ETQ_Ativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_Desativar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_IniciarEtiqueta: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_CarregarImagem(const eArquivoImagem, eNomeImagem: PChar;
      Flipped: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Impressão}
function ETQ_Imprimir(const ACopias, AAvancoEtq: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_GerarStreamBase64(const ACopias, AAvancoEtq: Integer; const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

function ETQ_ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
  MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
  const SubFonte: Integer; const ImprimirReverso: Boolean): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
  MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
  const SubFonte: Integer; const ImprimirReverso: Boolean): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
  LarguraBarraFina, Vertical, Horizontal: Integer; const eTexto: PChar;
  const AlturaCodBarras, ExibeCodigo: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
  EspessuraVertical, EspessuraHorizontal: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
  const eNomeImagem: PChar): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirQRCode(const Vertical, Horizontal: Integer; const Texto: PChar;
  LarguraModulo, ErrorLevel, Tipo: Integer): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ComandoGravaRFIDASCII(const Texto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ComandoGravaRFIDHexaDecimal(const Texto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibETQConfig;

{%region ETQ}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function ETQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(pLib, TACBrLibETQ, eArqConfig, eChaveCrypt);
end;

function ETQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar(pLib);
end;

function ETQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(pLib, sNome, esTamanho);
end;

function ETQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(pLib, sVersao, esTamanho);
end;

function ETQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(pLib, sMensagem, esTamanho);
end;

function ETQ_ConfigImportar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigImportar(pLib, eArqConfig);
end;

function ETQ_ConfigExportar(const sMensagem: PChar; var esTamanho: longint): longint;
      {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigExportar(pLib, sMensagem, esTamanho);
end;

function ETQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(pLib, eArqConfig);
end;

function ETQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(pLib, eArqConfig);
end;

function ETQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(pLib, eSessao, eChave, sValor, esTamanho);
end;

function ETQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(pLib, eSessao, eChave, eValor);
end;
{%endregion}

{%region Diversos}
function ETQ_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).Ativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).Desativar;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_IniciarEtiqueta: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).IniciarEtiqueta;
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).FinalizarEtiqueta(ACopias, AAvancoEtq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_CarregarImagem(const eArquivoImagem, eNomeImagem: PChar;
      Flipped: Boolean): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).CarregarImagem(eArquivoImagem, eNomeImagem, Flipped);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;
{%endregion}

{%region Impressão}
function ETQ_Imprimir(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).Imprimir(ACopias, AAvancoEtq);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_GerarStreamBase64(const ACopias, AAvancoEtq: Integer; const sResposta: PChar; var esTamanho: longint): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).GerarStreamBase64(ACopias, AAvancoEtq, sResposta, esTamanho);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;

end;

function ETQ_ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirTexto(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical,
                                                   Horizontal, eTexto, SubFonte, ImprimirReverso);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirTextoStr(Orientacao, Fonte, MultiplicadorH, MultiplicadorV, Vertical,
                                                      Horizontal, eTexto, SubFonte, ImprimirReverso);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirBarras(Orientacao, TipoBarras, LarguraBarraLarga,
                                                    LarguraBarraFina, Vertical, Horizontal, eTexto,
                                                    AlturaCodBarras, ExibeCodigo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirLinha(Vertical, Horizontal, Largura, Altura);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
                                                   EspessuraVertical, EspessuraHorizontal);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal, eNomeImagem);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ImprimirQRCode(const Vertical, Horizontal: Integer; const Texto: PChar;
          LarguraModulo, ErrorLevel, Tipo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ImprimirQRCode(Vertical, Horizontal, Texto, LarguraModulo, ErrorLevel, Tipo);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ComandoGravaRFIDASCII(const Texto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ComandoGravaRFIDASCII(Texto);
  except
    on E: EACBrLibException do
      Result := E.Erro;

    on E: Exception do
      Result := ErrExecutandoMetodo;
  end;
end;

function ETQ_ComandoGravaRFIDHexaDecimal(const Texto: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada(pLib);
    Result := TACBrLibETQ(pLib^.Lib).ComandoGravaRFIDHexaDecimal(Texto);
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

