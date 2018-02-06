{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2017 Leivio Ramos de Fontenele              }
{                                                                              }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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
{                                                                              }
{ Leivio Ramos de Fontenele  -  leivio@yahoo.com.br                            }
{******************************************************************************}
{******************************************************************************
|* Historico
|*
|* 24/10/2017: Renato Rubinho
|*  - Compatibilizado Fonte com Delphi 7
*******************************************************************************}

{$I ACBr.inc}

unit ACBrReinfWebServices;

interface

uses
  Classes, SysUtils, ACBrDFe, ACBrDFeWebService, pcnLeitor, ACBrUtil, pcnConversaoReinf, ACBrReinfRetEventos, pcnGerador;

type

  EACBReinfWebService = class(Exception);

  TReinfWebService = class(TDFeWebService)
  private
    FPLayout: TLayReinf;
    FXMLEnvio: string;
    procedure ConfigurarSoapDEPC;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: string; override;
  public
    property Layout: TLayReinf read FPLayout;
    property XMLEnvio: string read FXMLEnvio write FXMLEnvio;
    constructor Create(AOwner: TACBrDFe); override;
  end;

  TEnvioLote = class(TReinfWebService)
  private
    FVersao : String;
  protected
    procedure DefinirServicoEAction; override;
    procedure SalvarEnvio; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirDadosMsg; override;
  public
    constructor Create(AOwner: TACBrDFe); reintroduce;
  end;

  { Não Liberado }
  {
  TConsultarLote = class(TReinfWebService)
  private
    FVersao : string;
    FGrupo : Integer;
    FPURLEnvio : string;
  protected
    procedure DefinirServicoEAction; override;
    procedure SalvarEnvio; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirDadosMsg; override;
  public
    constructor Create(AOwner: TACBrDFe);
  end;
  }
  TWebServices = class
  private
    FACBrReinf: TACBrDFe;
    FEnvioLote : TEnvioLote;
    //FConsultar: TConsultarLote;
    FRetEventos: TRetornoLoteEventos;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;
    function Enviar(const AXML: string): Boolean;
    //function Consultar(const AXML: string): Boolean;
    property ACBrReinf: TACBrDFe read FACBrReinf write FACBrReinf;
    property EnvioLote : TEnvioLote read FEnvioLote write FEnvioLote;
    property RetEventos: TRetornoLoteEventos read FRetEventos;
  end;

implementation

{ TReinfWebService }

uses blcksock, ACBrReinf, DateUtils, pcnConversao, ACBrReinfClasses;

procedure TReinfWebService.ConfigurarSoapDEPC;
begin
  FPSoapVersion := 'soap';
  FPHeaderElement := 'Header';
  FPSoapEnvelopeAtributtes :='xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:v1="http://sped.fazenda.gov.br/"';
  FPBodyElement := 'Body';
end;

constructor TReinfWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPSoapVersion := 'soap';
  FPMimeType := 'text/xml';
  FPHeaderElement := '';
  FPBodyElement := '';
  FPCabMsg := '';
  FPURL := '';
  FPVersaoServico := '';
  FPArqEnv := '';
  FPArqResp := '';
  FPServico := '';
  FPSoapAction := '';
end;

procedure TReinfWebService.DefinirURL;
var
  Versao: Double;
begin
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  TACBrReinf(FPDFeOwner).LerServicoDeParams(FPLayout, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TReinfWebService.GerarVersaoDadosSoap: String;
begin
  Result := '';
end;

procedure TReinfWebService.InicializarServico;
begin
  inherited InicializarServico;
  FPDFeOwner.SSL.SSLType := LT_TLSv1;
end;

{ TEnvioLote }

constructor TEnvioLote.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
  FPLayout := orLayENVIO;
  //FRetProcLote := TRetProcLote.Create(AOwner);
  FVersao := '';
  ConfigurarSoapDEPC;
end;

procedure TEnvioLote.DefinirDadosMsg;
begin
  FPDadosMsg := FXMLEnvio;
end;

procedure TEnvioLote.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}
  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
  Texto := Texto + '<' + 'v1:ReceberLoteEventos>';
  Texto := Texto + '<' +  'v1:loteEventos>';
  Texto := Texto + DadosMsg;
  Texto := Texto + '<' +  '/v1:loteEventos>';
  Texto := Texto + '<' +  '/v1:ReceberLoteEventos>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  Texto := '<?xml version="1.0" encoding="utf-8"?>' + Texto;
  FPEnvelopeSoap := Texto;
end;

procedure TEnvioLote.DefinirServicoEAction;
begin
  FPServico := FPDFeOwner.GetNameSpaceURI + '/ReceberLoteEventos';
  FPSoapAction := Trim(FPServico);
end;

procedure TEnvioLote.SalvarEnvio;
var
  Path: string;
begin
  if TACBrReinf(Self.FPDFeOwner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBrReinf(Self.FPDFeOwner).Configuracoes.Arquivos.PathSalvar;
    with TStringList.Create do
    try
      Text := FPEnvelopeSoap;
      SaveToFile(Path+'\'+'E_Reinf_Soap'+'-'+ IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
  end;
end;

procedure TEnvioLote.SalvarResposta;
var
  Path: string;
begin
  if TACBrReinf(Self.FPDFeOwner).Configuracoes.Geral.Salvar then
  begin
    Path := TACBrReinf(Self.FPDFeOwner).Configuracoes.Arquivos.PathSalvar;
    with TStringList.Create do
    try
      Text := FPRetornoWS;
      SaveToFile(Path+'\'+'R_Reinf_Soap'+'-' + IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
      Text := FPRetWS;
      SaveToFile(Path+'\'+'RReinf'+'-'+ IntTostr(HourOf(Now))+ IntTostr(MinuteOf(Now))+IntTostr(SecondOf(Now)) + '_' +IntTostr(MilliSecondOf(Now)) + '.xml');
    finally
      Free;
    end;
  end;end;

function TEnvioLote.TratarResposta: Boolean;
var
  Leitor, Reader: TLeitor;
  i, j, k: integer;
  RetEventos: TRetornoLoteEventos;
  Evento: TEvento;
  Ocorrencia: TOcorrencia;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'ReceberLoteEventosResult');

  Result := FPRetWS <> EmptyStr;
  RetEventos := TACBrReinf(FPDFeOwner).WebServices.RetEventos;
  Leitor := TLeitor.Create;
  try
    Leitor.Arquivo := AnsiString(FPRetWS);
    Leitor.Grupo := AnsiString(FPRetWS);
    RetEventos.IdeTransmissor.IdTransmissor := Leitor.rCampo(tcStr, 'IdTransmissor');
    Leitor.Grupo := Leitor.rExtrai(1, 'status');
    RetEventos.Status.cdStatus := Leitor.rCampo(tcInt, 'cdStatus');
    RetEventos.Status.descRetorno := Leitor.rCampo(tcStr,'descRetorno');
    // Eventos
    Leitor.Arquivo := Ansistring(FPRetWS);
    i := 0;
    while Leitor.rExtrai(1, 'retornoEventos', '', i + 1) <> '' do
    begin
      //recepcao
      Reader := TLeitor.Create;
      try
        Reader.Arquivo := Leitor.Grupo;
        Reader.Grupo := Reader.rExtrai(1, 'retornoEvento');
        Evento := RetEventos.Eventos.Items[RetEventos.Eventos.Add(TEvento.Create)];
        Evento.id := Reader.rAtributo('Id', 'evento');
        Evento.ideContrib.TpInsc := tpTpInsc(Reader.rCampo(tcInt, 'tpInsc'));
        Evento.ideContrib.NrInsc := Reader.rCampo(tcStr, 'NrInsc');

        Evento.dadosRecepcaoEvento.dhProcessamento := Reader.rCampo(tcDatHor, 'dhProcessamento','');

        Evento.dadosRecepcaoEvento.tipoEvento := Reader.rCampo(tcStr, 'tipoEvento');
        Evento.dadosRecepcaoEvento.IDEvento := Reader.rCampo(tcStr, 'idEvento');
        Evento.dadosRecepcaoEvento.Hash := Reader.rCampo(tcStr, 'hash');

        Reader.Arquivo := Reader.rExtrai(1, 'retornoEvento');
        Reader.Grupo := Reader.rExtrai(1, 'status');
        Evento.Status.cdRetorno := Reader.rCampo(tcInt, 'cdRetorno');
        Evento.Status.descRetorno := UTF8ToNativeString((Reader.rCampo(tcStr, 'descRetorno')));

        if Evento.Status.cdRetorno = 0 then
        begin
          Reader.Arquivo := Reader.rExtrai(1, 'retornoEvento');
          Reader.Grupo := Reader.rExtrai(1, 'dadosReciboEntrega');
          Evento.dadosReciboEntrega.numeroRecibo := Reader.rCampo(tcStr, 'numeroRecibo');
        end
        else
        begin
          j := 0;
          while Reader.rExtrai(1, 'dadosRegistroOcorrenciaEvento', '', j + 1) <> '' do
          begin
            k := 0;
            while Reader.rExtrai(1, 'ocorrencias', '', k + 1) <> '' do
            begin
              Ocorrencia := Evento.Status.Ocorrencias.Items[Evento.Status.Ocorrencias.Add(TOcorrencia.Create)];
              Ocorrencia.tipo := Reader.rCampo(tcInt, 'tipo');
              Ocorrencia.localizacaoErroAviso := Reader.rCampo(tcStr, 'localizacaoErroAviso');
              Ocorrencia.codigo := Reader.rCampo(tcStr, 'codigo');
              Ocorrencia.descricao := UTF8ToNativeString(Reader.rCampo(tcStr, 'descricao'));

              inc(k);
            end;

            inc(j);
          end;
        end;
      finally
        Reader.Free;
      end;
      inc(i);
    end;
  finally
    Leitor.Free;
  end;
end;

{ TWebServices }

{
function TWebServices.Consultar(const AXML: string): Boolean;
begin
  raise Exception.Create('Consulta ainda não Liberado');
end;
}
constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrReinf := TACBrDFe(AOwner);
  FRetEventos := TRetornoLoteEventos.Create(AOwner);
  FEnvioLote := TEnvioLote.Create(FACBrReinf);
  //FConsultar := TConsultarLote.Create(FACBrReinf);
end;

destructor TWebServices.Destroy;
begin
  FRetEventos.Free;
  FEnvioLote.Free;
  inherited;
end;

function TWebServices.Enviar(const AXML: string): Boolean;
begin
  try
    EnvioLote.XMLEnvio := Axml;
    Result := EnvioLote.Executar;
    if not Result then
      EnvioLote.GerarException(EnvioLote.Msg);
  except
    on E: Exception do
      raise EACBReinfWebService.Create(e.Message);
  end;
end;

{ TConsultarLote }
(*
constructor TConsultarLote.Create(AOwner: TACBrDFe);
begin

end;

procedure TConsultarLote.DefinirDadosMsg;
begin
  inherited;

end;

procedure TConsultarLote.DefinirEnvelopeSoap;
begin
  inherited;

end;

procedure TConsultarLote.DefinirServicoEAction;
begin
  inherited;
  FPServico := FPDFeOwner.GetNameSpaceURI + '/';
  FPSoapAction := Trim(FPServico);
end;

procedure TConsultarLote.SalvarEnvio;
begin
  inherited;

end;

procedure TConsultarLote.SalvarResposta;
begin
  inherited;

end;

function TConsultarLote.TratarResposta: Boolean;
begin

end;
*)
end.
