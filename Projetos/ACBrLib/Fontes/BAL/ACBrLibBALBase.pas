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

unit ACBrLibBALBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibBALDataModule, ACBrBAL, ACBrUtil.FilesIO;

type

  { TACBrLibBAL }

  TACBrLibBAL = class(TACBrLib)
  private
    FBALDM: TLibBALDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property BALDM: TLibBALDM read FBALDM;

    function Ativar: longint;
    function Desativar: longint;
    function LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
    function SolicitarPeso: longint;
    function UltimoPesoLido(var Peso: Double): longint;
    function InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;

  end;


implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibBALConfig;

{ TACBrLibBAL }

constructor TACBrLibBAL.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FBALDM := TLibBALDM.Create(nil);
  FBALDM.Lib := Self;
end;

destructor TACBrLibBAL.Destroy;
begin
  FBALDM.Free;
  inherited Destroy;
end;

procedure TACBrLibBAL.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibBAL.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibBAL.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibBALConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibBAL.Executar;
begin
  inherited Executar;
  FBALDM.AplicarConfiguracoes;
end;

function TACBrLibBAL.Ativar: longint;
begin
  try
    GravarLog('BAL_Ativar', logNormal);

    BALDM.Travar;
    try
      BALDM.ACBrBAL1.Ativar;
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibBAL.Desativar: longint;
begin
  try
    GravarLog('BAL_Desativar', logNormal);

    BALDM.Travar;
    try
      BALDM.ACBrBAL1.Desativar;
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibBAL.LePeso(MillisecTimeOut: Integer; var Peso: Double): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('BAL_LePeso( ' + IntToStr(MillisecTimeOut) + ' )', logCompleto, True)
    else
      GravarLog('BAL_LePeso', logNormal);

    BALDM.Travar;
    try
      Peso := BALDM.ACBrBAL1.LePeso(MillisecTimeOut);
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibBAL.SolicitarPeso: longint;
begin
  try

    GravarLog('BAL_SolicitarPeso', logNormal);

    BALDM.Travar;
    try
      BALDM.ACBrBAL1.SolicitarPeso;
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibBAL.UltimoPesoLido(var Peso: Double): longint;
begin
  try
    GravarLog('BAL_UltimoPesoLido', logNormal);

    BALDM.Travar;
    try
      Peso := BALDM.ACBrBAL1.UltimoPesoLido;
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibBAL.InterpretarRespostaPeso(eResposta: PChar; var Peso: Double): longint;
var
  AResposta: AnsiString;
begin
  try
    AResposta := AnsiString(eResposta);

    if Config.Log.Nivel > logNormal then
      GravarLog('BAL_InterpretarRepostaPeso( ' + AResposta + ' )', logCompleto, True)
    else
      GravarLog('BAL_InterpretarRepostaPeso', logNormal);

    BALDM.Travar;
    try
      Peso := BALDM.ACBrBAL1.InterpretarRepostaPeso(AResposta);
      Result := SetRetorno(ErrOK);
    finally
      BALDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

