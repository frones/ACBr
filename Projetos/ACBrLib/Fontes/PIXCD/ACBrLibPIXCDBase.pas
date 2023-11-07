{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Antonio Carlos Junior                           }

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
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibPIXCDBase;

interface

uses
  Classes, SysUtils, ACBrLibComum, ACBrLibPIXCDDataModule, ACBrPIXCD;

type
  { TACBrLibPIXCD }
  TACBrLibPIXCD = class (TACBrLib)
    private
      FPIXCDDM: TLibPIXCDDM;

    protected
      procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      procedure Executar; Override;

    public
      constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
      destructor Destroy; override;

      property PIXCDDM: TLibPIXCDDM read FPIXCDDM;

      function GerarQRCodeEstatico(AValor: Currency; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibPIXCDConfig;

{ TACBrLibPIXCD }

constructor TACBrLibPIXCD.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);

  FPIXCDDM := TLibPIXCDDM.Create(Nil);
  FPIXCDDM.Lib := Self;
end;

destructor TACBrLibPIXCD.Destroy;
begin
  FPIXCDDM.Free;

  inherited Destroy;
end;

procedure TACBrLibPIXCD.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibPIXCDConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibPIXCD.Executar;
begin
  inherited Executar;
  FPIXCDDM.AplicarConfiguracoes;
end;

function TACBrLibPIXCD.GerarQRCodeEstatico(AValor: Currency; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  InfoAdicional, TxId, Resposta: String;
begin
  try
    InfoAdicional:= ConverterAnsiParaUTF8(AinfoAdicional);
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_GerarQRCodeEstatico(' + CurrToStr(AValor) + ',' + InfoAdicional + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_GerarQRCodeEstatico', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= PIXCDDM.ACBrPixCD1.GerarQRCodeEstatico(AValor, InfoAdicional, TxId);

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPIXCD.ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  e2id, Resposta: String;
begin
  try
    e2id:= ConverterAnsiParaUTF8(Ae2eid);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarPIX(' + e2id + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarPIX', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= BoolToStr(PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarPix(e2id));

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result:= SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPIXCD.ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId, CpfCnpj, Resposta: String;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);
    CpfCnpj:= ConverterAnsiParaUTF8(ACpfCnpj);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarPixRecebidos(' + DateToStr(ADataInicio) + ',' + DateToStr(ADataFim) + ',' + TxId + ',' + CpfCnpj + ',' + IntToStr(PagAtual) + ',' + IntToStr(ItensPorPagina) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarPixRecebidos', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= BoolToStr(PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos(ADataInicio, ADataFim, TxId, CpfCnpj, PagAtual, ItensPorPagina));

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result:= SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPIXCD.ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  e2eid, idDevolucao, Resposta: String;
begin
  try
    e2eid:= ConverterAnsiParaUTF8(Ae2eid);
    idDevolucao:= ConverterAnsiParaUTF8(AidDevolucao);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarDevolucaoPix(' + e2eid + ',' + idDevolucao + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarDevolucaoPix', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= BoolToStr(PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarDevolucaoPix(e2eid, idDevolucao));

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result:= SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPIXCD.ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId, Resposta: String;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobrancaImediata(' + TxId + ',' + IntToStr(Revisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= BoolToStr(PIXCDDM.ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(TxId, Revisao));

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result:= SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibPIXCD.ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId, Resposta: String;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobranca(' + TxId + ',' + IntToStr(Revisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= BoolToStr(PIXCDDM.ACBrPixCD1.PSP.epCobV.ConsultarCobranca(TxId, Revisao));

      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result:= SetRetorno(ErrOK, Resposta);
    finally
      PIXCDDM.Destravar;
    end;

  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

