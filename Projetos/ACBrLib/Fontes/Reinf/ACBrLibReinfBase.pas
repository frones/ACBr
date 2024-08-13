{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Rafael Teno Dias                                }

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

unit ACBrLibReinfBase;

interface

uses
  Classes, SysUtils, Forms,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrReinf, pcnConversaoReinf, ACBrLibReinfRespostas,
  ACBrLibComum, ACBrLibReinfDataModule;

type

  { TACBrLibReinf }

  TACBrLibReinf = class(TACBrLib)
  private
    FReinfDM: TLibReinfDM;

    function SetRetornoEventoCarregados(const NumEventos: integer): integer;
    function SetRetornoReinfCarregadas(const NumReinf: integer): integer;

  protected
    procedure CriarConfiguracao (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;

  public
    constructor Create (ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    function CriarEventoReinf(eArqIni: PAnsiChar):integer;
    function EnviarReinf(const sResposta: PAnsiChar; var esTamanho: integer): integer;
    function ConsultarReinf(eProtocolo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
    function ConsultarReciboReinf(ePerApur: PAnsiChar; aTipoEvento: Integer; eNrInscEstab: PAnsiChar;
      eCnpjPrestador: PAnsiChar; eNrInscTomador: PAnsiChar; eDtApur: PAnsiChar; eCpfCnpjBenef: PAnsiChar;
      eCnpjFonte: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
    function CriarEnviarReinf(const eArqIni: PAnsiChar; const sResposta: PAnsiChar;
      var esTamanho: integer): integer;
    function LimparReinf: integer;
    function CarregarXMLEventoReinf(const eArquivoOuXML: PAnsiChar): integer;
    function SetIDContribuinte(const aIdContribuinte: PAnsiChar): integer;
    function SetIDTransmissor(const aIdTransmissor: PAnsiChar): integer;
    function SetTipoContribuinte(aTipoContribuinte: integer):integer;
    function SetVersaoDF(const sVersao: PAnsiChar):integer;
    function ObterCertificados(const sResposta: PAnsiChar; var esTamanho: integer): integer;
    function Validar: integer;

    property ReinfDM: TLibReinfDM read FReinfDM;

  end;

implementation

Uses
  ACBrLibConsts, ACBrLibReinfConsts, ACBrLibConfig, ACBrLibReinfConfig,
  ACBrLibResposta, ACBrLibCertUtils, StrUtils;

{ TACBrLibReinf }

constructor TACBrLibReinf.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited
  Create(ArqConfig, ChaveCrypt);
  FReinfDM := TLibReinfDM.Create(Nil);
  FReinfDM.Lib := Self;
end;

destructor TACBrLibReinf.Destroy;
begin
  FReinfDM.Free;

  inherited Destroy;
end;

procedure TACBrLibReinf.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibReinfConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibReinf.Executar;
begin
  inherited Executar;
  FReinfDM.AplicarConfiguracoes;
end;

function TACBrLibReinf.SetRetornoEventoCarregados(const NumEventos: integer): integer;
begin
  Result := SetRetorno(0, Format(SInfEventosCarregados, [NumEventos]));
end;

function TACBrLibReinf.SetRetornoReinfCarregadas(const NumReinf: integer):integer;
begin
  Result := SetRetorno(0, Format(SInfReinfCarregadas, [NumReinf]));
end;

function TACBrLibReinf.CriarEventoReinf(eArqIni: PAnsiChar): integer;
var
  AArqIni: String;
begin
  try
    AArqIni:= AnsiString(eArqIni);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_CriarEvento (' + AArqIni + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_CriarEvento', logNormal);

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Eventos.LoadFromFile(AArqIni, False);
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E:EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.EnviarReinf(const sResposta: PAnsiChar; var esTamanho: integer): integer;
var
  AResposta: string;
  Resp: TRespostas;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_EnviarReinf', logCompleto, True)
    else
      GravarLog('Reinf_EnviarReinf', logNormal);

    ReinfDM.Travar;
    try
      AResposta := '';

      if ReinfDM.ACBrReinf1.Enviar then
      begin
        Resp := TRespostas.Create(ReinfDM.ACBrReinf1, Config.TipoResposta, Config.CodResposta);
        try
          Resp.RespostaEnvio;
          AResposta := Resp.Resposta;
        finally
          Resp.Free;
        end;
      end;

      ReinfDM.ACBrReinf1.Eventos.Clear;

      MoverStringParaPChar (AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.ConsultarReinf(eProtocolo: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
var
  AProtocolo, AResposta: String;
  Resp: TRespostas;
begin
  try
    AProtocolo := AnsiString(eProtocolo);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_ConsultarReinf (' + AProtocolo + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_ConsultarReinf', logNormal);

    ReinfDM.Travar;
    try
      AResposta := '';
      ReinfDM.ACBrReinf1.Eventos.Clear;

      if ReinfDM.ACBrReinf1.Consultar(AProtocolo) then
      begin
        Resp := TRespostas.Create(ReinfDM.ACBrReinf1, Config.TipoResposta, Config.CodResposta);
        try
          Resp.RespostaConsultaRetorno;
          AResposta := Resp.Resposta;
        finally
          Resp.Free;
        end;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.ConsultarReciboReinf(ePerApur: PAnsiChar;
  aTipoEvento: Integer; eNrInscEstab: PAnsiChar; eCnpjPrestador: PAnsiChar;
  eNrInscTomador: PAnsiChar; eDtApur: PAnsiChar; eCpfCnpjBenef: PAnsiChar;
  eCnpjFonte: PAnsiChar; const sResposta: PAnsiChar; var esTamanho: integer): integer;
var
  AResposta: String;
  PerApur: String;
  TipoEvento: Integer;
  Inscricaoestabelecimento: String;
  CnpjPrestador: String;
  InscricaoTomador: String;
  DataApur: String;
  CpfCnpjBenef: String;
  CnpjFonte: String;
  Ok: Boolean;
  Resp: TRespostas;
begin
  try
    PerApur := AnsiString(ePerApur);
    TipoEvento := aTipoEvento;
    InscricaoEstabelecimento := AnsiString(eNrInscEstab);
    CnpjPrestador := AnsiString(eCnpjPrestador);
    InscricaoTomador := AnsiString(eNrInscTomador);
    DataApur := AnsiString(eDtApur);
    CpfCnpjBenef := AnsiString(eCpfCnpjBenef);
    CnpjFonte := AnsiString(eCnpjFonte);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_ConsultarReciboReinf (' + PerApur + ', ' +
        IntToStr(TipoEvento) + ', ' + InscricaoEstabelecimento + ', ' +
        CnpjPrestador + ', ' + InscricaoTomador + ', ' +
        DataApur + ', ' + CpfCnpjBenef + ', ' +
        CnpjFonte + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_ConsultarReciboReinf', logNormal);

    ReinfDM.Travar;
    try
      AResposta := '';
      ReinfDM.ACBrReinf1.Eventos.Clear;

      if ReinfDM.ACBrReinf1.ConsultaReciboEvento(PerApur, StrToTipoEvento(Ok, 'R-' + IntToStr(TipoEvento)),
           InscricaoEstabelecimento, CnpjPrestador, InscricaoTomador, StrToDateTimeDef(DataApur,0),
           CpfCnpjBenef, eCnpjFonte) then
      begin
        Resp := TRespostas.Create(ReinfDM.ACBrReinf1, Config.TipoResposta, Config.CodResposta);
        try
          Resp.RespostaConsultaRecibo;
          AResposta := Resp.Resposta;
        finally
          Resp.Free;
        end;
      end;

      MoverStringParaPChar(AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.CriarEnviarReinf(const eArqIni: PAnsiChar; const sResposta: PAnsiChar;
  var esTamanho: integer): integer;
var
  AIniFile, ArqReinf : String;
  ASalvar : Boolean;
  iEvento : Integer;
  AResposta: String;
  Resp : TRespostas;
begin
  AIniFile:= AnsiString(eArqIni);
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_CriarEnviarReinf(' + AIniFile + ')', logCompleto, True)
    else
      GravarLog('Reinf_CriarEnviarReinf', logNormal);

    if not FileExists(AIniFile) then
      raise EACBrLibException.Create(ErrArquivoNaoExiste, ACBrStr(Format(SErroReinfAbrir, [AIniFile])));

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Eventos.LoadFromIni(AIniFile);
      ASalvar:= ReinfDM.ACBrReinf1.Configuracoes.Geral.Salvar;
      AResposta:= '';

      if not ASalvar then
      begin
        ForceDirectories(PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath);
        ReinfDM.ACBrReinf1.Configuracoes.Arquivos.PathSalvar:= PathWithDelim(ExtractFilePath(Application.ExeName)) + CChaveLogPath;
      end;

      iEvento:= ReinfDM.ACBrReinf1.Eventos.Gerados.Count - 1;
      ArqReinf:= ReinfDM.ACBrReinf1.Eventos.Gerados.Items[iEvento].PathNome + '.xml';

      if not FileExists(ArqReinf) then
        raise EACBrLibException.Create(ErrArquivoNaoExiste, ACBrStr(Format(SErroReinfAbrir, [ArqReinf]) ));

      AResposta := ArqReinf + sLineBreak + ACBrStr(Format(SMsgReinfEventoAdicionado, [TipoEventoToStr(ReinfDM.ACBrReinf1.Eventos.Gerados.Items[iEvento].TipoEvento)]) ) + sLineBreak;

      if ReinfDM.ACBrReinf1.Enviar then
      begin
        Resp := TRespostas.Create(ReinfDM.ACBrReinf1, Config.TipoResposta, Config.CodResposta);
        try
          Resp.RespostaEnvio;
          AResposta := AResposta + sLineBreak + Resp.Resposta;
        finally
          Resp.Free;
        end;
      end;

      ReinfDM.ACBrReinf1.Eventos.Clear;

      MoverStringParaPChar (AResposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, AResposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.LimparReinf: integer;
begin
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_Limpar', logCompleto, True)
    else
      GravarLog('Reinf_Limpar', logNormal);

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Eventos.Clear;
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.CarregarXMLEventoReinf(const eArquivoOuXML: PAnsiChar): integer;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterStringEntrada(eArquivoOuXML);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_CarregarXMLEventoReinf(' + ArquivoOuXml + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_CarregarXMLEventoReinf', logNormal);

    EhArquivo := StringEhArquivo(ArquivoOuXml);
    if EhArquivo then
      VerificarArquivoExiste(ArquivoOuXml);

    ReinfDM.Travar;

    try
      if EhArquivo then
        ReinfDM.ACBrReinf1.Eventos.LoadFromFile(ArquivoOuXml)
      else
        ReinfDM.ACBrReinf1.Eventos.LoadFromString(ArquivoOuXml);

      Result := SetRetornoEventoCarregados(ReinfDM.ACBrReinf1.Eventos.Count);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.SetIDContribuinte(const aIdContribuinte: PAnsiChar): integer;
var
  IdContribuinte: AnsiString;
begin
  try
    IdContribuinte:= ConverterStringEntrada(aIdContribuinte);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_SetIDContribuinte (' + IdContribuinte + ')', logCompleto, True)
    else
      GravarLog('Reinf_SetIDContribuinte', logNormal);

    if EstaVazio(IdContribuinte)then
      raise EACBrLibException.Create(ErrParametroInvalido, 'Valor Nulo');

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Configuracoes.Geral.IdContribuinte := IdContribuinte;
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.SetIDTransmissor (const aIdTransmissor: PAnsiChar): integer;
var
  idTransmissor: AnsiString;
begin
  try
    idTransmissor := ConverterStringEntrada(aIdTransmissor);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_SetIDTransmissor(' + idTransmissor + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_SetIDTransmissor', logNormal);

    if EstaVazio(idTransmissor)then
      raise EACBrLibException.Create(ErrParametroInvalido, 'Valor Nulo');

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Configuracoes.Geral.IdTransmissor := idTransmissor;
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.SetTipoContribuinte (aTipoContribuinte: integer):integer;
var
  TipoContribuinte: Integer;
begin
  TipoContribuinte:= aTipoContribuinte;
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_SetTipoContribuinte(' + IntToStr(aTipoContribuinte) + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_SetTipoContribuinte', logNormal);

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Configuracoes.Geral.TipoContribuinte := TContribuinte( TipoContribuinte );
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.SetVersaoDF (const sVersao: PAnsiChar):integer;
var
  versao: AnsiString;
  ok: Boolean;
begin
  try
    versao := ConverterStringEntrada(sVersao);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_SetVersaoDF(' + versao + ' ) ', logCompleto, True)
    else
      GravarLog('Reinf_SetVersaoDF', logNormal);

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Configuracoes.Geral.VersaoDF := StrToVersaoReinf(ok, versao);
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.ObterCertificados(const sResposta: PAnsiChar; var esTamanho: integer): integer;
var
  Resposta: Ansistring;
begin
  try
    GravarLog('Reinf_ObterCertificados', logNormal);

    ReinfDM.Travar;

    try
      Resposta := '';
      Resposta := ConverterStringSaida( ObterCerticados(ReinfDM.ACBrReinf1.SSL) );
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

function TACBrLibReinf.Validar():integer;
begin
  try
    GravarLog('Reinf_Validar', logNormal);

    ReinfDM.Travar;
    try
      try
        ReinfDM.ACBrReinf1.Eventos.Validar;
        Result := SetRetornoReinfCarregadas(ReinfDM.ACBrReinf1.Eventos.Count);
      except
        on E: EACBrReinfException do
          Result := SetRetorno(ErrValidacaoReinf, ConverterStringSaida(E.Message));
      end;
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterStringSaida(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterStringSaida(E.Message));
  end;
end;

end.

