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

unit ACBrLibCHQBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibCHQDataModule, ACBrCHQ, ACBrUtil.FilesIO;

type

  { TACBrLibCHQ }

  TACBrLibCHQ = class(TACBrLib)
  private
    FCHQDM: TLibCHQDM;

    procedure StringToMemo( AString : AnsiString; Memo : TStringList );

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property CHQDM: TLibCHQDM read FCHQDM;

    function Ativar: longint;
    function Desativar: longint;
    function ImprimirCheque: longint;
    function ImprimirLinha(const eLinha: PChar): longint;
    function ImprimirVerso(const eLinhas: PChar): longint;
    function TravarCheque: longint;
    function DestravarCheque: longint;
    function SetBanco(const eBanco: PChar): longint;
    function SetValor(const Valor: Double): longint;
    function SetData(const eData: PChar): longint;
    function SetCidade(const eCidade: PChar): longint;
    function SetFavorecido(const eFavorecido: PChar): longint;
    function SetObservacao(const eObservacao: PChar): longint;
    function SetBomPara(const eBomPara: PChar): longint;
    function SetArquivoBemaFiINI(const eArquivoBemaFiINI: PChar): longint;

  end;


implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibCHQConfig;

{ TACBrLibCHQ }

constructor TACBrLibCHQ.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FCHQDM := TLibCHQDM.Create(nil);
  FCHQDM.Lib := Self;
end;

destructor TACBrLibCHQ.Destroy;
begin
  FCHQDM.Free;
  inherited Destroy;
end;

procedure TACBrLibCHQ.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibCHQ.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibCHQ.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibCHQConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibCHQ.Executar;
begin
  inherited Executar;
  FCHQDM.AplicarConfiguracoes;
end;

procedure TACBrLibCHQ.StringToMemo( AString : AnsiString; Memo : TStringList );
begin
  AString   := StringReplace(AString,#13+#10,'|',[rfReplaceAll]) ;
  AString   := StringReplace(AString,#10,'|',[rfReplaceAll]) ;
  Memo.Text := StringReplace(AString,'|',sLineBreak,[rfReplaceAll]) ;
end ;

function TACBrLibCHQ.Ativar: longint;
begin
  try
    GravarLog('CHQ_Ativar', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Ativar;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.Desativar: longint;
begin
  try
    GravarLog('CHQ_Desativar', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Desativar;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.ImprimirCheque: longint;
begin
  try
    GravarLog('CHQ_ImprimirCheque', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.ImprimirCheque;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.ImprimirLinha(const eLinha: PChar): longint;
var
  ALinha: AnsiString;
begin
  try
    ALinha := ConverterAnsiParaUTF8(eLinha);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_ImprimirLinha( ' + ALinha + ' )', logCompleto, True)
    else
      GravarLog('CHQ_ImprimirLinha', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.ImprimirLinha(ALinha);
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.ImprimirVerso(const eLinhas: PChar): longint;
var
  ALinha: AnsiString;
  Linhas: TStringList;
begin
  try
    ALinha := ConverterAnsiParaUTF8(eLinhas);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_ImprimirVerso( ' + ALinha + ' )', logCompleto, True)
    else
      GravarLog('CHQ_ImprimirVerso', logNormal);

    CHQDM.Travar;
    Linhas := TStringList.Create;
    try
      StringToMemo( ALinha, Linhas ); {Linha separadas por | (pipe)}
      CHQDM.ACBrCHQ1.ImprimirVerso(Linhas);
      Result := SetRetorno(ErrOK);
    finally
      Linhas.Free;
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.TravarCheque: longint;
begin
  try
    GravarLog('CHQ_TravarCheque', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.TravarCheque;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.DestravarCheque: longint;
begin
  try
    GravarLog('CHQ_DestravarCheque', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.DestravarCheque;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetBanco(const eBanco: PChar): longint;
var
  ABanco: AnsiString;
begin
  try
    ABanco := ConverterAnsiParaUTF8(eBanco);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetBanco( ' + ABanco + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetBanco', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Banco := ABanco;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetValor(const Valor: Double): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetValor( ' + FloatToStr(Valor) + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetValor', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Valor := Valor;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetData(const eData: PChar): longint;
Var
  Data: TDateTime;
begin
  try
    Data := StrToDateTime(ConverterAnsiParaUTF8(eData));

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetData( ' + DateTimeToStr(Data) + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetData', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Data := Data;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetCidade(const eCidade: PChar): longint;
var
  ACidade: AnsiString;
begin
  try
    ACidade := ConverterAnsiParaUTF8(eCidade);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetCidade( ' + ACidade + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetCidade', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Cidade := ACidade;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetFavorecido(const eFavorecido: PChar): longint;
var
  AFavorecido: AnsiString;
begin
  try
    AFavorecido := ConverterAnsiParaUTF8(eFavorecido);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetFavorecido( ' + AFavorecido + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetFavorecido', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Favorecido := AFavorecido;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetObservacao(const eObservacao: PChar): longint;
var
  AObservacao: AnsiString;
begin
  try
    AObservacao := ConverterAnsiParaUTF8(eObservacao);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetObservacao( ' + AObservacao + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetObservacao', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.Observacao := AObservacao;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetBomPara(const eBomPara: PChar): longint;
Var
  BomPara: TDateTime;
begin
  try
    BomPara := StrToDateTime(ConverterAnsiParaUTF8(eBomPara));

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetBomPara( ' + DateTimeToStr(BomPara) + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetBomPara', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.BomPara := BomPara;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibCHQ.SetArquivoBemaFiINI(const eArquivoBemaFiINI: PChar): longint;
Var
  ArquivoBemaFiINI: String;
begin
  try
    ArquivoBemaFiINI := ConverterAnsiParaUTF8(eArquivoBemaFiINI);

    if Config.Log.Nivel > logNormal then
      GravarLog('CHQ_SetArquivoBemaFiINI( ' + ArquivoBemaFiINI + ' )', logCompleto, True)
    else
      GravarLog('CHQ_SetArquivoBemaFiINI', logNormal);

    CHQDM.Travar;
    try
      CHQDM.ACBrCHQ1.ArquivoBemaFiINI := ArquivoBemaFiINI;
      Result := SetRetorno(ErrOK);
    finally
      CHQDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

