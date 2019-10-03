{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibETQClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibETQDataModule, ACBrDevice, ACBrETQ;

type

  { TACBrLibETQ }

  TACBrLibETQ = class(TACBrLib)
  private
    FETQDM: TLibETQDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property ETQDM: TLibETQDM read FETQDM;
  end;

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
function ETQ_Imprimir(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function ETQ_ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibETQConsts, ACBrLibConfig, ACBrLibETQConfig;

{ TACBrLibETQ }

constructor TACBrLibETQ.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FETQDM := TLibETQDM.Create(nil);
end;

destructor TACBrLibETQ.Destroy;
begin
  FETQDM.Free;
  inherited Destroy;
end;

procedure TACBrLibETQ.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibETQ.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibETQ.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibETQConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibETQ.Executar;
begin
  inherited Executar;
  FETQDM.AplicarConfiguracoes;
end;

{%region ETQ}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function ETQ_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function ETQ_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function ETQ_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function ETQ_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function ETQ_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function ETQ_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function ETQ_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function ETQ_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function ETQ_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Diversos}
function ETQ_Ativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('ETQ_Ativar', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.Ativar;
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_Desativar: longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('ETQ_Desativar', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.Desativar;
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_IniciarEtiqueta: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('ETQ_IniciarEtiqueta', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.IniciarEtiqueta;
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_FinalizarEtiqueta( ' + IntToStr(ACopias) + ',' +
                                 IntToStr(AAvancoEtq) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_FinalizarEtiqueta', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.FinalizarEtiqueta(ACopias, AAvancoEtq);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_CarregarImagem(const eArquivoImagem, eNomeImagem: PChar;
      Flipped: Boolean): longint; {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AArquivoImagem: AnsiString;
  ANomeImagem: AnsiString;
begin
  try
    VerificarLibInicializada;
    AArquivoImagem := AnsiString(eArquivoImagem);
    ANomeImagem := AnsiString(eNomeImagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_CarregarImagem( ' + AArquivoImagem + ',' +
        ANomeImagem + ',' + BoolToStr(Flipped, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_CarregarImagem', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.CarregarImagem(AArquivoImagem, ANomeImagem, Flipped);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;
{%endregion}

{%region Impressão}
function ETQ_Imprimir(const ACopias, AAvancoEtq: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_Imprimir( ' + IntToStr(ACopias) + ',' +
                                 IntToStr(AAvancoEtq) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_Imprimir', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.Imprimir(ACopias, AAvancoEtq);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
    ATexto: AnsiString;
begin
  try
    VerificarLibInicializada;
    ATexto := AnsiString(eTexto);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirTexto( ' + IntToStr(Orientacao) + ',' +
        IntToStr(Fonte) + ',' + IntToStr(MultiplicadorH) + ',' +
        IntToStr(MultiplicadorV) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(SubFonte) + ',' +
        BoolToStr(ImprimirReverso, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirTexto', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirTexto(TACBrETQOrientacao(Orientacao), Fonte, MultiplicadorH,
              MultiplicadorV, Vertical, Horizontal, ATexto,
              SubFonte, ImprimirReverso);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
    ATexto, AFonte: AnsiString;
begin
  try
    VerificarLibInicializada;
    ATexto := AnsiString(eTexto);
    AFonte := AnsiString(Fonte);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirTextoStr( ' + IntToStr(Orientacao) + ',' +
        AFonte + ',' + IntToStr(MultiplicadorH) + ',' +
        IntToStr(MultiplicadorV) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(SubFonte) + ',' +
        BoolToStr(ImprimirReverso, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirTextoStr', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirTexto(TACBrETQOrientacao(Orientacao), AFonte, MultiplicadorH,
              MultiplicadorV, Vertical, Horizontal, ATexto,
              SubFonte, ImprimirReverso);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ATexto: AnsiString;
begin
  try
    VerificarLibInicializada;
    ATexto := AnsiString(eTexto);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirBarras( ' + IntToStr(Orientacao) + ',' +
        IntToStr(TipoBarras) + ',' + IntToStr(LarguraBarraLarga) + ',' +
        IntToStr(LarguraBarraFina) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(AlturaCodBarras) + ',' +
        IntToStr(ExibeCodigo) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirBarras', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirBarras(TACBrETQOrientacao(Orientacao),
           TACBrTipoCodBarra(TipoBarras), LarguraBarraLarga, LarguraBarraFina,
           Vertical, Horizontal, ATexto, AlturaCodBarras,
           TACBrETQBarraExibeCodigo(ExibeCodigo));
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirLinha( ' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + IntToStr(Largura) + ',' +
        IntToStr(Altura) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirLinha', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirLinha(Vertical, Horizontal, Largura, Altura);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirCaixa( ' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + IntToStr(Largura) + ',' +
        IntToStr(Altura) + IntToStr(EspessuraVertical) + ',' +
        IntToStr(EspessuraHorizontal) + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirCaixa', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirCaixa(Vertical, Horizontal, Largura, Altura,
      EspessuraVertical, EspessuraHorizontal);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function ETQ_ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ANomeImagem: AnsiString;
begin
  try
    VerificarLibInicializada;
    ANomeImagem := AnsiString(eNomeImagem);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('ETQ_ImprimirImagem( ' + IntToStr(MultiplicadorImagem) + ',' +
        IntToStr(Vertical) + ',' + IntToStr(Horizontal) +
        ANomeImagem  + ' )', logCompleto, True)
    else
      pLib.GravarLog('ETQ_ImprimirImagem', logNormal);

    with TACBrLibETQ(pLib) do
    begin
      ETQDM.Travar;
      try
        ETQDM.ACBrETQ1.ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal,
           ANomeImagem);
        Result := SetRetorno(ErrOK);
      finally
        ETQDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;
{%endregion}

{%endregion}

end.

