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
  Classes, SysUtils, ACBrLibComum, ACBrLibPIXCDDataModule;

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

      function GerarQRCodeEstatico(AValor: Double; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function SolicitarDevolucaoPix(AInfDevolucao: PChar; const Ae2eid: PChar; AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CriarCobrancaImediata(AInfCobSolicitada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobrancaImediata(const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function RevisarCobrancaImediata(AInfCobRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CancelarCobrancaImediata(ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CriarCobranca(AinfCobVSolicitada: PChar; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobranca(const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function RevisarCobranca(AInfCobVRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CancelarCobranca(ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibPIXCDConfig, ACBrPIXCD, ACBrPIXBase, ACBrLibPIXCDRespostas, ACBrLibHelpers,
  ACBrUtil.Strings, ACBrUtil.Base, ACBrPIXSchemasCob, ACBrPIXSchemasCobV;

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

function TACBrLibPIXCD.GerarQRCodeEstatico(AValor: Double; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: AnsiString;
  ValorCurrency: Currency;
begin
  try
    //Conversão realizada por conta de algumas linguagens não suportar Currency - C#.
    ValorCurrency:= AValor;

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_GerarQRCodeEstatico(' + CurrToStr(ValorCurrency) + ',' + AinfoAdicional + ',' + ATxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_GerarQRCodeEstatico', logNormal);

    PIXCDDM.Travar;
    try
      Resposta:= PIXCDDM.ACBrPixCD1.GerarQRCodeEstatico(ValorCurrency, AinfoAdicional, ATxId);

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

function TACBrLibPIXCD.ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  e2id: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    e2id:= ConverterAnsiParaUTF8(Ae2eid);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarPIX(' + e2id + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarPIX', logNormal);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarPix(e2id);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epPix.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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
  TxId, CpfCnpj: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
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
      PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarPixRecebidos(ADataInicio, ADataFim, TxId, CpfCnpj, PagAtual, ItensPorPagina);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epPix.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.SolicitarDevolucaoPix(AInfDevolucao: PChar; const Ae2eid: PChar; AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  e2eid, idDevolucao: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    e2eid:= ConverterAnsiParaUTF8(Ae2eid);
    idDevolucao:= ConverterAnsiParaUTF8(AidDevolucao);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_SolicitarDevolucaoPix(' + AInfDevolucao + ',' + e2eid + ',' + idDevolucao + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_SolicitarDevolucaoPix', logNormal);

    if StringEhArquivo(AInfDevolucao) then
    VerificarArquivoExiste(AInfDevolucao);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epPix.DevolucaoSolicitada.LoadFromIni(AInfDevolucao);
      PIXCDDM.ACBrPixCD1.PSP.epPix.SolicitarDevolucaoPix(e2eid, idDevolucao);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epPix.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
      Resp.Free;
      end;
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
  e2eid, idDevolucao: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
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
      PIXCDDM.ACBrPixCD1.PSP.epPix.ConsultarDevolucaoPix(e2eid, idDevolucao);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epPix.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.CriarCobrancaImediata(AInfCobSolicitada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
  TxId: String;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CriarCobrancaImediata(' + AInfCobSolicitada + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_CriarCobrancaImediata', logNormal);

    if StringEhArquivo(AInfCobSolicitada) then
    VerificarArquivoExiste(AInfCobSolicitada);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCob.CobSolicitada.LoadFromIni(AInfCobSolicitada);
      PIXCDDM.ACBrPixCD1.PSP.epCob.CriarCobrancaImediata(TxId);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCob.Problema);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.ConsultarCobrancaImediata(const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobrancaImediata(' + TxId + ',' + IntToStr(ARevisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(TxId, ARevisao);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCob.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.RevisarCobrancaImediata(AInfCobRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint):longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_RevisarCobrancaImediata(' + AInfCobRevisada + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_RevisarCobrancaImediata', logNormal);

    if StringEhArquivo(AInfCobRevisada) then
    VerificarArquivoExiste(AInfCobRevisada);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCob.CobRevisada.LoadFromIni(AInfCobRevisada);
      PIXCDDM.ACBrPixCD1.PSP.epCob.RevisarCobrancaImediata(TxId);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCob.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.CancelarCobrancaImediata(ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId := ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CancelarCobrancaImediata(' + TxId + ' )', logCompleto, True)
       else
       GravarLog('PIXCD_CancelarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCob.CobRevisada.Clear;
        PSP.epCob.CobRevisada.status:= stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
      end;

      PIXCDDM.ACBrPixCD1.PSP.epCob.RevisarCobrancaImediata(TxId);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCob.Problema);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.CriarCobranca(AinfCobVSolicitada: PChar; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CriarCobranca(' + AinfCobVSolicitada + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_CriarCobranca', logNormal);

    if StringEhArquivo(AinfCobVSolicitada) then
    VerificarArquivoExiste(AinfCobVSolicitada);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCobV.CobVSolicitada.LoadFromIni(AInfCobVSolicitada);
      PIXCDDM.ACBrPixCD1.PSP.epCobV.CriarCobranca(TxId);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCobV.Problema);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.ConsultarCobranca(const ATxId: PChar; ARevisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobranca(' + TxId + ',' + IntToStr(ARevisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCobV.ConsultarCobranca(TxId, ARevisao);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCobV.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.RevisarCobranca(AInfCobVRevisada: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_RevisarCobranca(' + AInfCobVRevisada + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_RevisarCobranca', logNormal);

    if StringEhArquivo(AInfCobVRevisada) then
    VerificarArquivoExiste(AInfCobVRevisada);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCobV.CobVRevisada.LoadFromIni(AInfCobVRevisada);
      PIXCDDM.ACBrPixCD1.PSP.epCobV.RevisarCobranca(TxId);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCobV.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

function TACBrLibPIXCD.CancelarCobranca(ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId := ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CancelarCobranca(' + TxId + ' )', logCompleto, True)
       else
       GravarLog('PIXCD_CancelarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCobV.CobVRevisada.Clear;
        PSP.epCobV.CobVRevisada.status:= stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
      end;

      PIXCDDM.ACBrPixCD1.PSP.epCobV.RevisarCobranca(TxId);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCobV.Problema);

        Resposta := Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result := SetRetorno(ErrOK, Resposta);
      finally
        Resp.Free;
      end;
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

