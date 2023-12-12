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

      function GerarQRCodeEstatico(AValor: Currency; const AinfoAdicional: PChar; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPix(const Ae2eid: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarPixRecebidos(ADataInicio: TDateTime; ADataFim: TDateTime; const ATxId: PChar; const ACpfCnpj: PChar; PagAtual: longint; ItensPorPagina: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function SolicitarDevolucaoPix(const Ae2eid: PChar; AidDevolucao: PChar; AValor: Currency; ANaturezaDevolucao: longint; ADescricao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarDevolucaoPix(const Ae2eid, AidDevolucao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CriarCobrancaImediata(AChavePIX: PChar; ACobrancaExpiracao: longint; ASolicitacaoPagador: PChar; ANomeDevedor: PChar; ACPFCNPJDevedor: PChar; AValor: Currency; APermitirAlterarValor: Boolean; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
      function RevisarCobrancaImediata(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function CriarCobranca(AChavePIX: PChar; ADataVencimento: TDateTime; AValidadeAposVencimento: longint; ANomeDevedor:PChar; ACPFCNPJDevedor: PChar; AValorOriginal: Currency; AMultaModalidade: longint; AMultaValorPercentual: Currency; AJurosModalidade: longint; AJurosValorPercentual: Currency; ADescontoModalidade: longint; ADescontoValorPercentual: Currency; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function RevisarCobranca(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
      function ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
  end;

implementation

Uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibPIXCDConfig, ACBrPIXCD, ACBrPIXBase, ACBrLibPIXCDRespostas, ACBrLibHelpers, ACBrUtil.Strings, ACBrUtil.Base, ACBrPIXSchemasCobV;

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
  InfoAdicional, TxId: String;
  Resposta: AnsiString;
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

function TACBrLibPIXCD.SolicitarDevolucaoPix(const Ae2eid: PChar; AidDevolucao: PChar; AValor: Currency; ANaturezaDevolucao: longint; ADescricao: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  e2eid, idDevolucao, descricao: String;
  NaturezaDevolucao: TACBrPIXNaturezaDevolucao;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    e2eid:= ConverterAnsiParaUTF8(Ae2eid);
    idDevolucao:= ConverterAnsiParaUTF8(AidDevolucao);
    descricao:= ConverterAnsiParaUTF8(ADescricao);

    if not (TEnum.TryParse<TACBrPIXNaturezaDevolucao>(ANaturezaDevolucao, NaturezaDevolucao)) then
       NaturezaDevolucao:= TACBrPIXNaturezaDevolucao.ndNENHUMA;

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_SolicitarDevolucaoPix(' + e2eid + ',' + idDevolucao + ',' + CurrToStr(AValor) + ',' + PIXNaturezaDevolucaoToString(NaturezaDevolucao) + ',' + descricao + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_SolicitarDevolucaoPix', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epPix.DevolucaoSolicitada.Clear;

        PSP.epPix.DevolucaoSolicitada.valor:= AValor;
        PSP.epPix.DevolucaoSolicitada.natureza:= NaturezaDevolucao;
        PSP.epPix.DevolucaoSolicitada.descricao:= descricao;
      end;
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

function TACBrLibPIXCD.CriarCobrancaImediata(AChavePIX: PChar; ACobrancaExpiracao: longint; ASolicitacaoPagador: PChar; ANomeDevedor: PChar; ACPFCNPJDevedor: PChar; AValor: Currency; APermitirAlterarValor: Boolean; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  s, ChavePIX, SolicitacaoPagador, NomeDevedor, CPFCNPJDevedor, TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    ChavePIX:= ConverterAnsiParaUTF8(AChavePIX);
    SolicitacaoPagador:= ConverterAnsiParaUTF8(ASolicitacaoPagador);
    NomeDevedor:= ConverterAnsiParaUTF8(ANomeDevedor);
    CPFCNPJDevedor:= ConverterAnsiParaUTF8(ACPFCNPJDevedor);
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CriarCobrancaImediata(' + ChavePIX + ',' + IntToStr(ACobrancaExpiracao) + ',' + SolicitacaoPagador + ',' + NomeDevedor + ',' + CPFCNPJDevedor + ',' + CurrToStr(AValor) + ',' + BoolToStr(APermitirAlterarValor) + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_CriarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCob.CobSolicitada.Clear;
        s:= ChavePIX;
        PSP.epCob.CobSolicitada.chave:= ChavePIX;
        PSP.epCob.CobSolicitada.calendario.expiracao:= ACobrancaExpiracao;
        PSP.epCob.CobSolicitada.solicitacaoPagador:= SolicitacaoPagador;

        s:= Trim(NomeDevedor);
        if (s <> '') then
        begin
          PSP.epCob.CobSolicitada.devedor.nome:= s;

          s:= OnlyNumber(CPFCNPJDevedor);
          if (s = '') then
             raise Exception.Create('Caso o Nome do Devedor seja Informado, e necessário informar CPF ou CNPJ')
          else if (Length(s) > 11) then
          PSP.epCob.CobSolicitada.devedor.cnpj:= s
          else
          PSP.epCob.CobSolicitada.devedor.cpf:= s;
        end;

        PSP.epCob.CobSolicitada.valor.original:= AValor;
        PSP.epCob.CobSolicitada.valor.modalidadeAlteracao:= APermitirAlterarValor;
      end;

      PIXCDDM.ACBrPixCD1.PSP.epCob.CriarCobrancaImediata(TxId);
      Resp := TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
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

function TACBrLibPIXCD.ConsultarCobrancaImediata(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobrancaImediata(' + TxId + ',' + IntToStr(Revisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCob.ConsultarCobrancaImediata(TxId, Revisao);
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

function TACBrLibPIXCD.RevisarCobrancaImediata(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint):longint;
var
  Status: TACBrPIXStatusCobranca;
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if not (TEnum.TryParse<TACBrPIXStatusCobranca>(AStatus, Status)) then
       Status:= TACBrPIXStatusCobranca.stcNENHUM;

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_RevisarCobrancaImediata(' + TxId + ',' + PIXStatusCobrancaToString(Status) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_RevisarCobrancaImediata', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCob.CobRevisada.Clear;
        PSP.epCob.CobRevisada.status:= Status;
      end;

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

function TACBrLibPIXCD.CriarCobranca(AChavePIX: PChar; ADataVencimento: TDateTime; AValidadeAposVencimento: longint; ANomeDevedor:PChar; ACPFCNPJDevedor: PChar; AValorOriginal: Currency; AMultaModalidade: longint; AMultaValorPercentual: Currency; AJurosModalidade: longint; AJurosValorPercentual: Currency; ADescontoModalidade: longint; ADescontoValorPercentual: Currency; ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  s, ChavePIX, NomeDevedor, CPFCNPJDevedor, TxId: String;
  MultaModalidade: TACBrPIXValoresModalidade;
  JurosModalidade: TACBrPIXJurosModalidade;
  DescontoModalidade: TACBrPIXDescontoModalidade;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    ChavePIX:= ConverterAnsiParaUTF8(AChavePIX);
    NomeDevedor:= ConverterAnsiParaUTF8(ANomeDevedor);
    CPFCNPJDevedor:= ConverterAnsiParaUTF8(ACPFCNPJDevedor);
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if not (TEnum.TryParse<TACBrPIXValoresModalidade>(AMultaModalidade, MultaModalidade)) then
       MultaModalidade:= TACBrPIXValoresModalidade.pvmNenhum;

    if not (TEnum.TryParse<TACBrPIXJurosModalidade>(AJurosModalidade, JurosModalidade)) then
       JurosModalidade:= TACBrPIXJurosModalidade.pjmNenhum;

    if not (TEnum.TryParse<TACBrPIXDescontoModalidade>(ADescontoModalidade, DescontoModalidade)) then
       DescontoModalidade:= TACBrPIXDescontoModalidade.pdmNenhum;

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_CriarCobranca(' + ChavePIX + ',' + DateToStr(ADataVencimento) + ',' + IntToStr(AValidadeAposVencimento) + ',' + NomeDevedor + ',' + CPFCNPJDevedor + ',' + CurrToStr(AValorOriginal) + ',' + ValoresModalidadeToString(MultaModalidade) + ',' + CurrToStr(AMultaValorPercentual) + ',' + JurosModalidadeToString(JurosModalidade) + ',' + CurrToStr(AJurosValorPercentual) + ',' + DescontoModalidadeToString(DescontoModalidade) + ',' + CurrToStr(ADescontoValorPercentual) + ',' + TxId + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_CriarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCobV.CobVSolicitada.Clear;
        PSP.epCobV.CobVSolicitada.chave:= ChavePIX;
        PSP.epCobV.CobVSolicitada.calendario.dataDeVencimento:= ADataVencimento;
        PSP.epCobV.CobVSolicitada.calendario.validadeAposVencimento:= AValidadeAposVencimento;

        with PSP.epCobV.CobVSolicitada.devedor do
        begin
          s:= Trim(NomeDevedor);
          if NaoEstaVazio(s) then
          begin
            nome:= s;
            s:= OnlyNumber(CPFCNPJDevedor);
            if EstaVazio(s) then
            raise Exception.Create('Caso o Nome do Devedor seja Informado, é necessário informar CPF ou CNPJ')
            else if (Length(s) > 11) then
               cnpj := s
            else
               cpf := s;
          end;
        end;

        PSP.epCobV.CobVSolicitada.valor.original:= AValorOriginal;

        PSP.epCobV.CobVSolicitada.valor.multa.modalidade:= MultaModalidade;
        PSP.epCobV.CobVSolicitada.valor.multa.valorPerc:= AMultaValorPercentual;

        PSP.epCobV.CobVSolicitada.valor.juros.modalidade:= JurosModalidade;
        PSP.epCobV.CobVSolicitada.valor.juros.valorPerc:= AJurosValorPercentual;

        PSP.epCobV.CobVSolicitada.valor.desconto.modalidade:= DescontoModalidade;

        if PSP.epCobV.CobVSolicitada.valor.desconto.modalidade in [pdmValorFixo, pdmPercentual] then
        begin
          with PSP.epCobV.CobVSolicitada.valor.desconto.descontosDataFixa.New do
          begin
            data:= ADataVencimento;
            valorPerc:= ADescontoValorPercentual;
          end;
        end
        else
        PSP.epCobV.CobVSolicitada.valor.desconto.valorPerc:= ADescontoValorPercentual;
      end;

      PIXCDDM.ACBrPixCD1.PSP.epCobV.CriarCobranca(TxId);
      Resp:= TLibPIXCDResposta.Create(Config.TipoResposta, Config.CodResposta);
      try
        Resp.Processar(PIXCDDM.ACBrPixCD1.PSP.epCobV.Problema);

        Resposta:= Resp.Gerar;
        MoverStringParaPChar(Resposta, sResposta, esTamanho);
        Result:= SetRetorno(ErrOk, Resposta);
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

function TACBrLibPIXCD.RevisarCobranca(AStatus: longint; const ATxId: PChar; const sResposta: PChar; var esTamanho: longint): longint;
var
  Status: TACBrPIXStatusCobranca;
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if not (TEnum.TryParse<TACBrPIXStatusCobranca>(AStatus, Status)) then
       Status:= TACBrPIXStatusCobranca.stcNENHUM;

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_RevisarCobranca(' + TxId + ',' + PIXStatusCobrancaToString(Status) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_RevisarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      with PIXCDDM.ACBrPixCD1 do
      begin
        PSP.epCobV.CobVRevisada.Clear;
        PSP.epCobV.CobVRevisada.status:= Status;
      end;

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

function TACBrLibPIXCD.ConsultarCobranca(const ATxId: PChar; Revisao: longint; const sResposta: PChar; var esTamanho: longint): longint;
var
  TxId: String;
  Resposta: AnsiString;
  Resp: TLibPIXCDResposta;
begin
  try
    TxId:= ConverterAnsiParaUTF8(ATxId);

    if Config.Log.Nivel > logNormal then
       GravarLog('PIXCD_ConsultarCobranca(' + TxId + ',' + IntToStr(Revisao) + ' )', logCompleto, True)
    else
       GravarLog('PIXCD_ConsultarCobranca', logNormal);

    PIXCDDM.Travar;
    try
      PIXCDDM.ACBrPixCD1.PSP.epCobV.ConsultarCobranca(TxId, Revisao);
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

end.

