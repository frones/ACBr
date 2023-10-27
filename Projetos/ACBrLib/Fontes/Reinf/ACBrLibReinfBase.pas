{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }

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
  Classes, SysUtils, Forms, ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings,
  ACBrLibComum, ACBrLibReinfDataModule, ACBrReinf, pcnConversaoReinf, ACBrLibReinfRespostas;

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

    function CriarEventoReinf(eArqIni: PChar):longint;
    function EnviarReinf(aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarReinf(eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function ConsultarReciboReinf(ePerApur: PChar; aTipoEvento: Integer; eNrInscEstab: PChar;
      eCnpjPrestador: PChar; eNrInscTomador: PChar; eDtApur: PChar; eCpfCnpjBenef: PChar;
      eCnpjFonte: PChar; const sResposta: PChar; var esTamanho: longint): longint;
    function CriarEnviarReinf(const eArqIni: PChar; aGrupo:integer): longint;
    function LimparReinf: Longint;
    function CarregarXMLEventoReinf(const eArquivoOuXML: PChar): longint;
    function SetIdContribuinte(const aIdContribuinte: PChar): longint;
    function SetIDTransmissor(const aIdTransmissor: PChar): longint;
    function SetTipoContribuinte(aTipoContribuinte: integer):longint;
    function SetVersaoDF(const sVersao: PChar):longint;
    function ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
    function Validar: longint;

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

function TACBrLibReinf.CriarEventoReinf(eArqIni: PChar): longint;
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.EnviarReinf(aGrupo: integer; const sResposta: PChar; var esTamanho: longint): longint;
var
  AResposta: string;
  Resp: TRespostas;
  grupo: Integer;
begin
  grupo := aGrupo;

  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_EnviarReinf (' + IntToStr(Grupo) + ' ) ', logCompleto, True)
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.ConsultarReinf(eProtocolo: PChar; const sResposta: PChar; var esTamanho: longint): longint;
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.ConsultarReciboReinf(ePerApur: PChar;
  aTipoEvento: Integer; eNrInscEstab: PChar; eCnpjPrestador: PChar;
  eNrInscTomador: PChar; eDtApur: PChar; eCpfCnpjBenef: PChar;
  eCnpjFonte: PChar; const sResposta: PChar; var esTamanho: longint): longint;
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.CriarEnviarReinf(const eArqIni: PChar; aGrupo:integer): longint;
var
  AIniFile, ArqReinf : String;
  ASalvar : Boolean;
  iEvento : Integer;
  grupo : Integer;
  AResposta: String;
  Resp : TRespostas;
begin
  AIniFile:= AnsiString(eArqIni);
  Grupo:= aGrupo;
  try
    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_CriarEnviarReinf(' + AIniFile + ', ' + IntToStr(Grupo) + ' )', logCompleto, True)
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

      Result := SetRetorno(ErrOK, AResposta);

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
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.LimparReinf: Longint;
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.CarregarXMLEventoReinf(const eArquivoOuXML: PChar): longint;
var
  EhArquivo: boolean;
  ArquivoOuXml: string;
begin
  try
    ArquivoOuXml := ConverterAnsiParaUTF8(eArquivoOuXML);

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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.SetIdContribuinte(const aIdContribuinte: PChar): longint;
var
  IdContribuinte: AnsiString;
begin
  try
    IdContribuinte:= ConverterAnsiParaUTF8(aIdContribuinte);

    if Config.Log.Nivel > logNormal then
      GravarLog('Reinf_IdContribuinte (' + IdContribuinte + ')', logCompleto, True)
    else
      GravarLog('Reinf_SetIdContribuinte', logNormal);

    if DirectoryExists(IdContribuinte) then
      raise EACBrLibException.Create(ErrDiretorioNaoExiste, 'Diretorio não existe');

    ReinfDM.Travar;
    try
      ReinfDM.ACBrReinf1.Configuracoes.Geral.IdContribuinte := IdContribuinte;
      Result := SetRetorno(ErrOK);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.SetIDTransmissor (const aIdTransmissor: PChar): longint;
var
  idTransmissor: AnsiString;
begin
  try
    idTransmissor := ConverterAnsiParaUTF8(aIdTransmissor);

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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.SetTipoContribuinte (aTipoContribuinte: integer):longint;
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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.SetVersaoDF (const sVersao: PChar):longint;
var
  versao: AnsiString;
  ok: Boolean;
begin
  try
    versao := ConverterAnsiParaUTF8(sVersao);

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
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.ObterCertificados(const sResposta: PChar; var esTamanho: longint): longint;
var
  Resposta: Ansistring;
begin
  try
    GravarLog('Reinf_ObterCertificados', logNormal);

    ReinfDM.Travar;

    try
      Resposta := '';
      Resposta := ObterCerticados(ReinfDM.ACBrReinf1.SSL);
      Resposta := IfThen(Config.CodResposta = codAnsi, ACBrUTF8ToAnsi(Resposta), Resposta);
      MoverStringParaPChar(Resposta, sResposta, esTamanho);
      Result := SetRetorno(ErrOK, Resposta);
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibReinf.Validar():longint;
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
          Result := SetRetorno(ErrValidacaoReinf, ConverterUTF8ParaAnsi(E.Message));
      end;
    finally
      ReinfDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

