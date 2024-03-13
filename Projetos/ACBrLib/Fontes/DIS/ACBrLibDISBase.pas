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

unit ACBrLibDISBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibDISDataModule, ACBrDIS, ACBrUtil.FilesIO;

type

  { TACBrLibDIS }

  TACBrLibDIS = class(TACBrLib)
  private
    FDISDM: TLibDISDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property DISDM: TLibDISDM read FDISDM;

    function Ativar: longint;
    function Desativar: longint;
    function LimparDisplay: longint;
    function LimparLinha(const Linha: Integer): longint;
    function PosicionarCursor(const Linha, Coluna: Integer): longint;
    function Escrever(const eTexto: PChar): longint;
    function ExibirLinha(const Linha: Integer ; const eTexto: PChar): longint;
    function ExibirLinhaAlinhada(const Linha: Integer ; const eTexto: PChar; const Alinhamento: Integer): longint;
    function ExibirLinhaEfeito(const Linha: Integer ; const eTexto: PChar; const Efeito: Integer): longint;
    function RolarLinha(const Linha, Efeito: Integer): longint;
    function Parar: longint;
    function Continuar: longint;
    function PararLinha(const Linha: Integer): longint;
    function ContinuarLinha(const Linha: Integer): longint;
  end;

implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibDISConfig;

{ TACBrLibDIS }
constructor TACBrLibDIS.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FDISDM := TLibDISDM.Create(nil);
  FDISDM.Lib := Self;
end;

destructor TACBrLibDIS.Destroy;
begin
  FDISDM.Free;
  inherited Destroy;
end;

procedure TACBrLibDIS.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibDIS.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibDIS.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibDISConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibDIS.Executar;
begin
  inherited Executar;
  FDISDM.AplicarConfiguracoes;
end;

function TACBrLibDIS.Ativar: longint;
begin
  try
    GravarLog('DIS_Ativar', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.Ativar;
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.Desativar: longint;
begin
  try
    GravarLog('DIS_Desativar', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.Desativar;
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.LimparDisplay: longint;
begin
  try
    GravarLog('DIS_LimparDisplay', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.LimparDisplay;
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.LimparLinha(const Linha: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_LimparLinha( ' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      GravarLog('DIS_LimparLinha', logNormal);

    DISDM.Travar;

    try
      DISDM.ACBrDIS1.LimparLinha(Linha);
      Result := SetRetorno(ErrOK);
    finally
      DISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.PosicionarCursor(const Linha, Coluna: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_PosicionarCursor( ' + IntToStr(Linha) + ',' + IntToStr(Coluna) + ' )', logCompleto, True)
    else
      GravarLog('DIS_PosicionarCursor', logNormal);

    DISDM.Travar;
    try
      DISDM.ACBrDIS1.PosicionarCursor(Linha, Coluna);
      Result := SetRetorno(ErrOK);
    finally
      DISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.Escrever(const eTexto: PChar): longint;
var
  ATexto: String;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);

    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_Escrever( ' + ATexto + ' )', logCompleto, True)
    else
      GravarLog('DIS_Escrever', logNormal);

    DISDM.Travar;
    try
      DISDM.ACBrDIS1.Escrever(ATexto);
      Result := SetRetorno(ErrOK);
    finally
      DISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.ExibirLinha(const Linha: Integer; const eTexto: PChar): longint;
Var
  ATexto: String;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);

    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_ExibirLinha(' + IntToStr(Linha) + ', ' + ATexto  + ' )', logCompleto, True)
    else
      GravarLog('DIS_ExibirLinha', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.ExibirLinha(Linha, ATexto);
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.ExibirLinhaAlinhada(const Linha: Integer ; const eTexto: PChar; const Alinhamento: Integer): longint;
Var
  ATexto: String;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);

    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_ExibirLinhaAlinhada(' + IntToStr(Linha) + ', ' + ATexto  + ', '
                                           + IntToStr(Alinhamento) + ' )', logCompleto, True)
    else
      GravarLog('DIS_ExibirLinhaAlinhada', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.ExibirLinha(Linha, ATexto, TACBrDISAlinhamento(Alinhamento));
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.ExibirLinhaEfeito(const Linha: Integer ; const eTexto: PChar; const Efeito: Integer): longint;
Var
  ATexto: String;
begin
  try
    ATexto := ConverterAnsiParaUTF8(eTexto);

    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_ExibirLinhaEfeito(' + IntToStr(Linha) + ', ' + ATexto  + ', '
                                   + IntToStr(Efeito) + ' )', logCompleto, True)
    else
      GravarLog('DIS_ExibirLinhaEfeito', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.ExibirLinha(Linha, ATexto, TACBrDISEfeitoExibir(Efeito));
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.RolarLinha(const Linha, Efeito: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_RolarLinha(' + IntToStr(Linha) + ', ' + IntToStr(Efeito) + ' )', logCompleto, True)
    else
      GravarLog('DIS_RolarLinha', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.RolarLinha(Linha, TACBrDISEfeitoRolar(Efeito));
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.Parar: longint;
begin
  try
    GravarLog('DIS_Parar', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.Parar;
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.Continuar: longint;
begin
  try
    GravarLog('DIS_Continuar', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.Continuar;
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.PararLinha(const Linha: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_PararLinha(' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      GravarLog('DIS_PararLinha', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.PararLinha(Linha);
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibDIS.ContinuarLinha(const Linha: Integer): longint;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('DIS_ContinuarLinha(' + IntToStr(Linha) + ' )', logCompleto, True)
    else
      GravarLog('DIS_ContinuarLinha', logNormal);

    FDISDM.Travar;
    try
      FDISDM.ACBrDIS1.ContinuarLinha(Linha);
      Result := SetRetorno(ErrOK);
    finally
      FDISDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

