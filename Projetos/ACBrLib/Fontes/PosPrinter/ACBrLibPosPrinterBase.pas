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

unit ACBrLibPosPrinterBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrPosPrinter, ACBrLibComum, ACBrLibPosPrinterDataModule;

type
  PACBrPosPrinter = ^TACBrPosPrinter;

  { TACBrLibPosPrinter }

  TACBrLibPosPrinter = class(TACBrLib)
  private
    FPosDM: TLibPosPrinterDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property PosDM: TLibPosPrinterDM read FPosDM;

    function Ativar: longint;
    function Desativar: longint;
    function Imprimir(eString: PChar; PulaLinha, DecodificarTags,  CodificarPagina: Boolean; Copias: Integer): longint;
    function ImprimirLinha(eString: PChar): longint;
    function ImprimirCmd(eComando: PChar): longint;
    function ImprimirTags: longint;
    function ImprimirImagemArquivo(aPath: PChar): longint;
    function ImprimirLogo(nAKC1, nAKC2, nFatorX, nFatorY: longint): longint;
    function ImprimirCheque(CodBanco: Integer; const AValor, ADataEmissao, AFavorecido,	ACidade, AComplemento: PChar;
                            LerCMC7: Boolean; SegundosEspera: Integer): longint;
    function ImprimirTextoCheque(const X, Y: Integer; const AString: PChar; AguardaCheque: Boolean;
                                 SegundosEspera: Integer): longint;
    function TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
                  const sResposta: PChar; var esTamanho: longint): longint;

    function Zerar: longint;
    function InicializarPos: longint;
    function Reset: longint;
    function PularLinhas(NumLinhas: Integer): longint;
    function CortarPapel(Parcial: Boolean): longint;
    function AbrirGaveta: longint;
    function LerInfoImpressora(const sResposta: PChar; var esTamanho: longint): longint;

    function LerStatusImpressora(Tentativas: Integer; var status: longint): longint;
    function LerStatusImpressoraFormatado(Tentativas: Integer; const sResposta: PChar; var esTamanho: longint): longint;

    function RetornarTags(IncluiAjuda: Boolean; const sResposta: PChar; var esTamanho: longint): longint;
    function AcharPortas(const sResposta: PChar; var esTamanho: longint): longint;
    function GravarLogoArquivo(aPath: PChar; nAKC1, nAKC2: longint): longint;
    function ApagarLogo(nAKC1, nAKC2: longint): longint;
    function LeituraCheque(const sResposta: PChar; var esTamanho: longint): longint;
    function LerCMC7(AguardaCheque: Boolean; SegundosEspera: Integer;
                     const sResposta: PChar; var esTamanho: longint): longint;
    function EjetarCheque: longint;
    function PodeLerDaPorta: longint;
    function LerCaracteristicas(const sResposta: PChar; var esTamanho: longint): longint;
    function GetPosPrinter: Pointer;

  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibPosPrinterConfig,
  ACBrLibDeviceUtils, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings, StrUtils;

{ TACBrLibPosPrinter }

constructor TACBrLibPosPrinter.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FPosDM := TLibPosPrinterDM.Create(nil);
  FPosDM.Lib := self;
end;

destructor TACBrLibPosPrinter.Destroy;
begin
  FPosDM.Free;
  inherited Destroy;
end;

procedure TACBrLibPosPrinter.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibPosPrinter.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibPosPrinter.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibPosPrinterConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibPosPrinter.Executar;
begin
  inherited Executar;
  FPosDM.AplicarConfiguracoes;
end;

function TACBrLibPosPrinter.Ativar: longint;
begin
  try
    GravarLog('POS_Ativar', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.Ativar;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.Desativar: longint;
begin
  try
    GravarLog('POS_Desativar', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.Desativar;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.Imprimir(eString: PChar; PulaLinha, DecodificarTags,
  CodificarPagina: Boolean; Copias: Integer): longint;  
var
  AString: AnsiString;
  UTF8Str: String;
begin
  try
    AString := AnsiString(eString);

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_Imprimir(' + AString + ',' + BoolToStr(PulaLinha, True) + ',' +
        BoolToStr(DecodificarTags, True) + ',' + BoolToStr(CodificarPagina, True) + ',' +
        IntToStr(Copias) +' )', logCompleto, True)
    else
      GravarLog('POS_Imprimir', logNormal);

    PosDM.Travar;
    try
      UTF8Str := ConverterAnsiParaUTF8(AString);
      PosDM.ACBrPosPrinter1.Imprimir(UTF8Str, PulaLinha, DecodificarTags, CodificarPagina, Copias);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirLinha(eString: PChar): longint;  
var
  AString: AnsiString;
  UTF8Str: String;
begin
  try
    AString := AnsiString(eString);

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirLinha(' + AString + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirLinha', logNormal);

    PosDM.Travar;
    try
      UTF8Str := ConverterAnsiParaUTF8(AString);
      PosDM.ACBrPosPrinter1.ImprimirLinha(UTF8Str);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirCmd(eComando: PChar): longint;  
var
  AComando: AnsiString;
begin
  try
    AComando := AnsiString(eComando);

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirCmd(' + AComando + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirCmd', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirCmd(AComando);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirTags: longint;
begin
  try
    GravarLog('POS_ImprimirTags', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirTags;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirImagemArquivo(aPath: PChar): longint;  
Var
  Path: AnsiString;
begin
  try
    Path := AnsiString(aPath);

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirImagemArquivo(' + Path + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirImagemArquivo', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirImagemArquivo(Path);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirLogo(nAKC1, nAKC2, nFatorX, nFatorY: longint): longint;  
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirLogo(' + IntToStr(nAKC1) + ',' + IntToStr(nAKC2) + ',' + IntToStr(nFatorX) + ','
                 + IntToStr(nFatorY) + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirLogo', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirLogo(nAKC1, nAKC2, nFatorX, nFatorY);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirCheque(CodBanco: Integer;
      const AValor, ADataEmissao, AFavorecido, ACidade, AComplemento: PChar; LerCMC7: Boolean;
      SegundosEspera: Integer): longint;  
Var
  Valor: Double;
  DataEmissao: TDateTime;
  Favorecido, Cidade, Complemento: String;
begin
  try
    Valor := StringToFloatDef(AValor, 0);
    DataEmissao := StrToDate(ADataEmissao);
    Favorecido := ConverterAnsiParaUTF8(ansistring(AFavorecido));
    Cidade := ConverterAnsiParaUTF8(ansistring(ACidade));
    Complemento := ConverterAnsiParaUTF8(ansistring(AComplemento));

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirCheque(' + IntToStr(CodBanco) + ','
                                         + FloatToStr(Valor) + ','
                                         + DateToStr(DataEmissao) + ','
                                         + Favorecido + ','
                                         + Cidade + ','
                                         + Complemento + ','
                                         + BoolToStr(LerCMC7, True) + ','
                                         + IntToStr(SegundosEspera) + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirCheque', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirCheque(CodBanco, Valor, DataEmissao, Favorecido, Cidade,
                                           Complemento, LerCMC7, SegundosEspera);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ImprimirTextoCheque(const X, Y: Integer; const AString: PChar;
  AguardaCheque: Boolean; SegundosEspera: Integer): longint;  
Var
  Texto: string;
begin
   try
    Texto := ConverterAnsiParaUTF8(ansistring(AString));

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ImprimirTextoCheque(' + IntToStr(x) + ',' + IntToStr(y) + ',' + Texto + ','
                 + BoolToStr(AguardaCheque, True) + ',' + IntToStr(SegundosEspera) + ')', logCompleto, True)
    else
      GravarLog('POS_ImprimirTextoCheque', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ImprimirTextoCheque(x, y, Texto, AguardaCheque, SegundosEspera);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.TxRx(eCmd: PChar; BytesToRead: Byte; ATimeOut: Integer; WaitForTerminator: Boolean;
  const sResposta: PChar; var esTamanho: longint): longint;  
var
  ACmd, Resposta: AnsiString;
begin
  try
    ACmd := AnsiString(eCmd);
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_TxRx(' + ACmd + ',' + IntToStr(BytesToRead) + ',' + IntToStr(ATimeOut) + ',' +
                 BoolToStr(WaitForTerminator, True) + ' )', logCompleto, True)
    else
      GravarLog('POS_TxRx', logNormal);

    PosDM.Travar;
    try
      Resposta := '';
      Resposta := PosDM.ACBrPosPrinter1.TxRx(ACmd, BytesToRead, ATimeOut, WaitForTerminator);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.Zerar: longint;
begin
  try
    GravarLog('POS_Zerar', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.Zerar;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.InicializarPos: longint;
begin
  try
    GravarLog('POS_Inicializar', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.Inicializar;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.Reset: longint;
begin
  try
    GravarLog('POS_Reset', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.Reset;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.PularLinhas(NumLinhas: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_PularLinhas(' + IntToStr(NumLinhas) + ' )', logCompleto, True)
    else
      GravarLog('POS_PularLinhas', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.PularLinhas(NumLinhas);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.CortarPapel(Parcial: Boolean): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_CortarPapel(' + BoolToStr(Parcial, True) + ' )', logCompleto, True)
    else
      GravarLog('POS_CortarPapel', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.CortarPapel(Parcial);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.AbrirGaveta: longint;
begin
  try
    GravarLog('POS_AbrirGaveta', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.AbrirGaveta;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LerInfoImpressora(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta: string;
begin
  try
    GravarLog('POS_LerInfoImpressora', logNormal);

    PosDM.Travar;
    try
      Resposta := '';
      Resposta := PosDM.ACBrPosPrinter1.LerInfoImpressora;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LerStatusImpressora(Tentativas: Integer; var status: longint): longint;  
Var
  RetStatus: TACBrPosPrinterStatus;
  i: TACBrPosTipoStatus;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_LerStatusImpressora(' + IntToStr(Tentativas) + ' )', logCompleto, True)
    else
      GravarLog('POS_LerStatusImpressora', logNormal);

    PosDM.Travar;
    status := 0;
    try
      RetStatus := PosDM.ACBrPosPrinter1.LerStatusImpressora(Tentativas);
      if RetStatus <> [] then
      begin
        for i := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
        begin
          if i in RetStatus then
            status := status + (1 << Ord(i));
        end;
      end;

      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LerStatusImpressoraFormatado(Tentativas: Integer; const sResposta: PChar; var esTamanho: longint): longint;
var
  RetStatus: TACBrPosPrinterStatus;
  Resposta: string;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_LerStatusImpressoraFormatado(' + IntToStr(Tentativas) + ' )', logCompleto, True)
    else
      GravarLog('POS_LerStatusImpressoraFormatado', logNormal);

    Resposta := '';
    PosDM.Travar;
    try
      RetStatus := PosDM.ACBrPosPrinter1.LerStatusImpressora(Tentativas);
      Resposta := ACBrPosPrinter.FormataPrinterStatus(RetStatus);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;

end;

function TACBrLibPosPrinter.RetornarTags(IncluiAjuda: Boolean; const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Tags: TStringList;
  Resposta: string;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_RetornarTags(' + BoolToStr(IncluiAjuda, True) + ' )', logCompleto, True)
    else
      GravarLog('POS_RetornarTags', logNormal);

    PosDM.Travar;
    Tags := TStringList.Create;

    try
      PosDM.ACBrPosPrinter1.RetornarTags(Tags, IncluiAjuda);
      Resposta := StringReplace(Tags.Text, sLineBreak, '|', [rfReplaceAll]);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      Tags.Free;
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.AcharPortas(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta, Portas: string;
begin
  try
    GravarLog('POS_AcharPortas', logNormal);

    PosDM.Travar;

    Resposta := '';
    try
      Resposta := PortasSeriais(PosDM.ACBrPosPrinter1.Device);

      Portas := '';
      Portas := PortasUSB(PosDM.ACBrPosPrinter1.Device);
      Resposta := IfThen(Portas.IsEmpty, Resposta,  Resposta + '|' + Portas);

      Portas := '';
      Portas := PortasRAW(PosDM.ACBrPosPrinter1.Device);
      Resposta := IfThen(Portas.IsEmpty, Resposta, Resposta + '|' + Portas);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.GravarLogoArquivo(aPath: PChar; nAKC1, nAKC2: longint): longint;  
Var
  Path: AnsiString;
begin
  try
    Path := AnsiString(aPath);

    if Config.Log.Nivel > logNormal then
      GravarLog('POS_GravarLogoArquivo(' + Path + ',' + IntToStr(nAKC1) + ',' + IntToStr(nAKC2) +')', logCompleto, True)
    else
      GravarLog('POS_GravarLogoArquivo', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.GravarLogoArquivo(Path, nAKC1, nAKC2);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.ApagarLogo(nAKC1, nAKC2: longint): longint;  
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_ApagarLogo(' + IntToStr(nAKC1) + ',' + IntToStr(nAKC2) + ')', logCompleto, True)
    else
      GravarLog('POS_ApagarLogo', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.ApagarLogo(nAKC1, nAKC2);
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LeituraCheque(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta: string;
begin
  try
    GravarLog('POS_LeituraCheque', logNormal);

    PosDM.Travar;
    try
      Resposta := '';
      Resposta := PosDM.ACBrPosPrinter1.LeituraCheque;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LerCMC7(AguardaCheque: Boolean; SegundosEspera: Integer; const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta: string;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('POS_LerCMC7(' + BoolToStr(AguardaCheque, True) + ',' + IntToStr(SegundosEspera) + ')', logCompleto, True)
    else
      GravarLog('POS_LerCMC7', logNormal);

    PosDM.Travar;
    try
      Resposta := '';
      PosDM.ACBrPosPrinter1.LerCMC7(AguardaCheque, SegundosEspera);
      Resposta := PosDM.ACBrPosPrinter1.LeituraCheque;
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.EjetarCheque: longint;
begin
  try
    GravarLog('POS_EjetarCheque', logNormal);

    PosDM.Travar;
    try
      PosDM.ACBrPosPrinter1.EjetarCheque;
      Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.PodeLerDaPorta: longint;
begin
  try
    GravarLog('POS_PodeLerDaPorta', logNormal);

    PosDM.Travar;
    try
      if PosDM.ACBrPosPrinter1.PodeLerDaPorta then
        Result := SetRetorno(1)
      else
        Result := SetRetorno(ErrOK);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.LerCaracteristicas(const sResposta: PChar; var esTamanho: longint): longint;  
Var
  Resposta: string;
begin
  try
    GravarLog('POS_LerCaracteristicas', logNormal);

    PosDM.Travar;
    try
      Resposta := IntToStr(PosDM.ACBrPosPrinter1.TemGuilhotina);
      Resposta := Resposta + '|' + IntToStr(PosDM.ACBrPosPrinter1.TemCheque);
      Resposta := Resposta + '|' + IntToStr(PosDM.ACBrPosPrinter1.TemAutenticacao);
      Resposta := Resposta + '|' + IntToStr(PosDM.ACBrPosPrinter1.TemMICR);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPosPrinter.GetPosPrinter: Pointer;  
begin
  try
    GravarLog('POS_GetPosPrinter', logNormal);

    PosDM.Travar;
    try
      Result := PosDM.ACBrPosPrinter1;
      with TACBrPosPrinter(Result) do
          Self.GravarLog('  '+ClassName+', '+ Name, logParanoico);
    finally
      PosDM.Destravar;
    end;
  except
    on E: EACBrLibException do
    begin
      SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));
      Result := Nil;
    end;

    on E: Exception do
    begin
      SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
      Result := Nil;
    end;
  end;
end;

end.

