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

unit ACBrLibETQBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibETQDataModule, ACBrDevice, ACBrETQ, ACBrUtil.FilesIO;

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

    function Ativar: longint;
    function Desativar: longint;
    function IniciarEtiqueta: longint;
    function FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;
    function CarregarImagem(const eArquivoImagem, eNomeImagem: PChar; Flipped: Boolean): longint;
    function Imprimir(const ACopias, AAvancoEtq: Integer): longint;
    function ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
                                       MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
                                       const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
    function ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
                                       MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
                                       const SubFonte: Integer; const ImprimirReverso: Boolean): longint;
    function ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
                                       LarguraBarraFina, Vertical, Horizontal: Integer;
                                       const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;
    function ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;
    function ImprimirCaixa(const Vertical, Horizontal, Largura, Altura,
                                       EspessuraVertical, EspessuraHorizontal: Integer): longint;
    function ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
                                       const eNomeImagem: PChar): longint;
    function ImprimirQRCode(const Vertical, Horizontal: Integer; const Texto: PChar;
                                       LarguraModulo, ErrorLevel, Tipo: Integer): longint;
    function ComandoGravaRFIDASCII(const Texto: PChar): longint;
    function ComandoGravaRFIDHexaDecimal(const Texto: PChar): longint;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibETQConfig, ACBrETQClass;

{ TACBrLibETQ }

constructor TACBrLibETQ.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FETQDM := TLibETQDM.Create(nil);
  FETQDM.Lib := Self;
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

function TACBrLibETQ.Ativar: longint; 
begin
  try
    GravarLog('ETQ_Ativar', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.Ativar;
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.Desativar: longint; 
begin
  try
    GravarLog('ETQ_Desativar', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.Desativar;
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.IniciarEtiqueta: longint;  
begin
  try
    GravarLog('ETQ_IniciarEtiqueta', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.IniciarEtiqueta;
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.FinalizarEtiqueta(const ACopias, AAvancoEtq: Integer): longint;  
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_FinalizarEtiqueta( ' + IntToStr(ACopias) + ',' +
                                 IntToStr(AAvancoEtq) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_FinalizarEtiqueta', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.FinalizarEtiqueta(ACopias, AAvancoEtq);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.CarregarImagem(const eArquivoImagem, eNomeImagem: PChar; Flipped: Boolean): longint;
var
  AArquivoImagem: AnsiString;
  ANomeImagem: AnsiString;
begin
  try
    AArquivoImagem := ConverterAnsiParaUTF8(eArquivoImagem);
    ANomeImagem := ConverterAnsiParaUTF8(eNomeImagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_CarregarImagem( ' + AArquivoImagem + ',' +
        ANomeImagem + ',' + BoolToStr(Flipped, True) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_CarregarImagem', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.CarregarImagem(AArquivoImagem, ANomeImagem, Flipped);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.Imprimir(const ACopias, AAvancoEtq: Integer): longint;  
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_Imprimir( ' + IntToStr(ACopias) + ',' +
                                 IntToStr(AAvancoEtq) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_Imprimir', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.Imprimir(ACopias, AAvancoEtq);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirTexto(const Orientacao, Fonte, MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;  
var
  ATexto: AnsiString;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);
    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirTexto( ' + IntToStr(Orientacao) + ',' +
        IntToStr(Fonte) + ',' + IntToStr(MultiplicadorH) + ',' +
        IntToStr(MultiplicadorV) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(SubFonte) + ',' +
        BoolToStr(ImprimirReverso, True) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirTexto', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirTexto(TACBrETQOrientacao(Orientacao), Fonte, MultiplicadorH,
                                   MultiplicadorV, Vertical, Horizontal, ATexto,
                                   SubFonte, ImprimirReverso);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirTextoStr(const Orientacao: Integer; const Fonte: PChar; const MultiplicadorH,
            MultiplicadorV, Vertical, Horizontal: Integer; const eTexto: PChar;
            const SubFonte: Integer; const ImprimirReverso: Boolean): longint;  
var
  ATexto, AFonte: AnsiString;
begin
  try
    
    ATexto := ConverterAnsiParaUTF8(eTexto);
    AFonte := ConverterAnsiParaUTF8(Fonte);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirTextoStr( ' + IntToStr(Orientacao) + ',' +
        AFonte + ',' + IntToStr(MultiplicadorH) + ',' +
        IntToStr(MultiplicadorV) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(SubFonte) + ',' +
        BoolToStr(ImprimirReverso, True) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirTextoStr', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirTexto(TACBrETQOrientacao(Orientacao), AFonte, MultiplicadorH,
                                   MultiplicadorV, Vertical, Horizontal, ATexto,
                                   SubFonte, ImprimirReverso);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirBarras(const Orientacao, TipoBarras, LarguraBarraLarga,
            LarguraBarraFina, Vertical, Horizontal: Integer;
     const eTexto: PChar; const AlturaCodBarras, ExibeCodigo: Integer): longint;  
var
  ATexto: AnsiString;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirBarras( ' + IntToStr(Orientacao) + ',' +
        IntToStr(TipoBarras) + ',' + IntToStr(LarguraBarraLarga) + ',' +
        IntToStr(LarguraBarraFina) + ',' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(AlturaCodBarras) + ',' +
        IntToStr(ExibeCodigo) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirBarras', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirBarras(TACBrETQOrientacao(Orientacao), TACBrTipoCodBarra(TipoBarras), LarguraBarraLarga,
                                    LarguraBarraFina, Vertical, Horizontal, ATexto, AlturaCodBarras,
                                    TACBrETQBarraExibeCodigo(ExibeCodigo));
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirLinha(const Vertical, Horizontal, Largura, Altura: Integer): longint;  
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirLinha( ' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + IntToStr(Largura) + ',' +
        IntToStr(Altura) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirLinha', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirLinha(Vertical, Horizontal, Largura, Altura);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirCaixa(const Vertical, Horizontal, Largura, Altura, EspessuraVertical,
            EspessuraHorizontal: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirCaixa( ' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + IntToStr(Largura) + ',' +
        IntToStr(Altura) + IntToStr(EspessuraVertical) + ',' +
        IntToStr(EspessuraHorizontal) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirCaixa', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirCaixa(Vertical, Horizontal, Largura, Altura, EspessuraVertical, EspessuraHorizontal);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirImagem(const MultiplicadorImagem, Vertical, Horizontal: Integer;
      const eNomeImagem: PChar): longint;  
var
  ANomeImagem: AnsiString;
begin
  try
    ANomeImagem := ConverterAnsiParaUTF8(eNomeImagem);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirImagem( ' + IntToStr(MultiplicadorImagem) + ',' +
        IntToStr(Vertical) + ',' + IntToStr(Horizontal) +
        ANomeImagem  + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirImagem', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirImagem(MultiplicadorImagem, Vertical, Horizontal, ANomeImagem);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ImprimirQRCode(const Vertical, Horizontal: Integer; const Texto: PChar;
          LarguraModulo, ErrorLevel, Tipo: Integer): longint;  
Var
  ATexto: string;
begin
  try
    ATexto := ConverterAnsiParaUTF8(Texto);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ImprimirQRCode( ' + IntToStr(Vertical) + ',' +
        IntToStr(Horizontal) + ',' + ATexto + ',' + IntToStr(LarguraModulo)
        + ',' + IntToStr(ErrorLevel) + ',' + IntToStr(Tipo) + ' )', logCompleto, True)
    else
      GravarLog('ETQ_ImprimirQRCode', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ImprimirQRCode(Vertical, Horizontal, ATexto, LarguraModulo, ErrorLevel, Tipo);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ComandoGravaRFIDASCII(const Texto: PChar): longint;
Var
  ATexto: string;
begin
  try
    ATexto := ConverterAnsiParaUTF8(Texto);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ComandoGravaRFIDASCII( ' +  ATexto + ' )', logCompleto)
    else
      GravarLog('ETQ_ComandoGravaRFIDASCII', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ComandoGravaRFIDASCII(ATexto);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibETQ.ComandoGravaRFIDHexaDecimal(const Texto: PChar): longint;
Var
  ATexto: string;
begin
  try
    ATexto := ConverterAnsiParaUTF8(Texto);

    if Config.Log.Nivel > logNormal then
      GravarLog('ETQ_ComandoGravaRFIDHexaDecimal( ' +  ATexto + ' )', logCompleto)
    else
      GravarLog('ETQ_ComandoGravaRFIDHexaDecimal', logNormal);

    ETQDM.Travar;
    try
      ETQDM.ACBrETQ1.ComandoGravaRFIDHexaDecimal(ATexto);
      Result := SetRetorno(ErrOK);
    finally
      ETQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

