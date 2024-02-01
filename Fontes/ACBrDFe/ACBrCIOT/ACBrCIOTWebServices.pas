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

unit ACBrCIOTWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  ACBrCIOTContratos, ACBrCIOTConfiguracoes,
  pcnAuxiliar, pcnConversao, pcnConversaoCIOT, pcnCIOT, pcnRetEnvCIOT;

const
  CURL_WSDL = 'http://schemas.ipc.adm.br/efrete/pef/';

type

  { TCIOTWebService }

  TCIOTWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrCIOT;
    FPLayout: TLayOutCIOT;
    FPConfiguracoesCIOT: TConfiguracoesCIOT;

  protected
    procedure InicializarServico; override;
    procedure DefinirEnvelopeSoap; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure EnviarDados; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrCIOT read FPStatus;
    property Layout: TLayOutCIOT     read FPLayout;
  end;

  { TCIOTEnviar }

  TCIOTEnviar = class(TCIOTWebService)
  private
    FContratos: TContratos;
    FRetornoEnvio: TRetornoEnvio;
    FCodRetorno: Integer;
    FNomePDF: string;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure DefinirURL; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
    function GerarPathDownload: String;
  public
    constructor Create(AOwner: TACBrDFe; AContratos: TContratos);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property RetornoEnvio: TRetornoEnvio read FRetornoEnvio;
    property CodRetorno: Integer         read FCodRetorno     write FCodRetorno;
    property NomePDF: string             read FNomePDF        write FNomePDF;
  end;

  { TCIOTEnvioWebService }

  TCIOTEnvioWebService = class(TCIOTWebService)
  private
    FXMLEnvio: String;
    FPURLEnvio: String;
    FVersao: String;
    FSoapActionEnvio: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    function Executar: Boolean; override;
    procedure Clear; override;

    property XMLEnvio: String        read FXMLEnvio        write FXMLEnvio;
    property URLEnvio: String        read FPURLEnvio       write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrCIOT: TACBrDFe;
    FCIOTEnviar: TCIOTEnviar;
    FEnvioWebService: TCIOTEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(const ANomePDF: String = ''): Boolean;

    property ACBrCIOT: TACBrDFe                   read FACBrCIOT         write FACBrCIOT;
    property CIOTEnviar: TCIOTEnviar              read FCIOTEnviar       write FCIOTEnviar;
    property EnvioWebService: TCIOTEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  ACBrDFeConsts,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  ACBrCIOT,
  pcnGerador, pcnLeitor;

{ TCIOTWebService }

constructor TCIOTWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesCIOT := TConfiguracoesCIOT(FPConfiguracoes);

  FPLayout := LayeFreteOperacaoTransporte;
  FPStatus := stCIOTIdle;

  FPHeaderElement := '';
  FPBodyElement   := '';
  FPMimeType      := 'text/xml';
  FPSoapVersion   := 'soap';
end;

procedure TCIOTWebService.Clear;
begin
  inherited Clear;

  FPStatus := stCIOTIdle;
end;

procedure TCIOTWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrCIOT(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TCIOTWebService.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  FPDadosMsg := RemoverDeclaracaoXML(FPDadosMsg);

  Texto := Texto + '<' + FPSoapVersion + ':Envelope ' + FPSoapEnvelopeAtributtes + '>';
  Texto := Texto + '<' + FPSoapVersion + ':Header/>';
  Texto := Texto + '<' + FPSoapVersion + ':Body>';
//  Texto := Texto + '<' + FPBodyElement + '>';
  Texto := Texto + FPDadosMsg;
//  Texto := Texto + '</' + FPBodyElement + '>';
  Texto := Texto + '</' + FPSoapVersion + ':Body>';
  Texto := Texto + '</' + FPSoapVersion + ':Envelope>';

  FPEnvelopeSoap := Texto;
end;

procedure TCIOTWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrCIOT(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TCIOTWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrCIOT(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TCIOTWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrCIOT(FPDFeOwner).SetStatus(stCIOTIdle);
end;

procedure TCIOTWebService.EnviarDados;
Var
  Tentar, Tratado: Boolean;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  FPRetWS := '';
  FPRetornoWS := '';
  Tentar := True;

  FPEnvelopeSoap := UTF8ToNativeString(FPEnvelopeSoap);

  while Tentar do
  begin
    Tentar := False;
    Tratado := False;

    // Tem Certificado carregado ?
    if (FPConfiguracoes.Certificados.NumeroSerie <> '') then
      if FPConfiguracoes.Certificados.VerificarValidade then
        if (FPDFeOwner.SSL.CertDataVenc < Now) then
          GerarException(ACBrStr('Data de Validade do Certificado já expirou: ' +
                                 FormatDateBr(FPDFeOwner.SSL.CertDataVenc)));

    try
      FPRetornoWS := FPDFeOwner.SSL.Enviar(FPEnvelopeSoap, FPURL, FPSoapAction, FPMimeType);
    except
      if Assigned(FPDFeOwner.OnTransmitError) then
        FPDFeOwner.OnTransmitError(FPDFeOwner.SSL.HTTPResultCode,
                                   FPDFeOwner.SSL.InternalErrorCode,
                                   FPURL, FPEnvelopeSoap, FPSoapAction,
                                   Tentar, Tratado);

      if not (Tentar or Tratado) then
        raise;
    end;
  end;
end;

{ TCIOTEnvioWebService }

constructor TCIOTEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stCIOTEnvioWebService;
end;

destructor TCIOTEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

function TCIOTEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TCIOTEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

procedure TCIOTEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TCIOTEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TCIOTEnvioWebService.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
  LeitorXML := TLeitor.Create;

  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;

  FPDadosMsg := FXMLEnvio;
end;

function TCIOTEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, FPSoapVersion + ':Body');
  Result := True;
end;

function TCIOTEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TCIOTEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrCIOT := TACBrCIOT(AOwner);

  FCIOTEnviar := TCIOTEnviar.Create(FACBrCIOT, TACBrCIOT(FACBrCIOT).Contratos);

  FEnvioWebService := TCIOTEnvioWebService.Create(FACBrCIOT);
end;

destructor TWebServices.Destroy;
begin
  FCIOTEnviar.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia(const ANomePDF: String = ''): Boolean;
begin
  CIOTEnviar.NomePDF := ANomePDF;

  if not CIOTEnviar.Executar then
    CIOTEnviar.GerarException( CIOTEnviar.Msg );

  Result := True;
end;

{ TCIOTEnviar }

procedure TCIOTEnviar.Clear;
begin
  inherited Clear;

  FPStatus := stCIOTEnviar;

  FCodRetorno := 0;

  if Assigned(FRetornoEnvio) then
    FRetornoEnvio.Free;

  FRetornoEnvio := TRetornoEnvio.Create;
end;

constructor TCIOTEnviar.Create(AOwner: TACBrDFe; AContratos: TContratos);
begin
  inherited Create(AOwner);

  FContratos := AContratos;
end;

procedure TCIOTEnviar.DefinirDadosMsg;
begin
  FPDadosMsg := FContratos.Items[0].XMLAssinado;
end;

procedure TCIOTEnviar.DefinirServicoEAction;
var
  Servico, Acao: String;
begin
  FPSoapEnvelopeAtributtes := 'xmlns:' + FPSoapVersion +
                                '="http://schemas.xmlsoap.org/soap/envelope/" ';

  case FContratos.Items[0].CIOT.Integradora.Operacao of
    opLogin,
    opLogout:
      begin
        Servico := 'http://schemas.ipc.adm.br/efrete/logon/';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
                                    'xmlns:log="http://schemas.ipc.adm.br/efrete/logon" '+
                                    'xmlns:obj="http://schemas.ipc.adm.br/efrete/logon/objects"';
      end;

    opGravarProprietario:
      begin
        Servico := 'http://schemas.ipc.adm.br/efrete/proprietarios/';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
                                    'xmlns:prop="http://schemas.ipc.adm.br/efrete/proprietarios" '+
                                    'xmlns:obj="http://schemas.ipc.adm.br/efrete/proprietarios/objects" ' +
                                    'xmlns:obj1="http://schemas.ipc.adm.br/efrete/objects"';
      end;

    opGravarVeiculo:
      begin
        Servico := 'http://schemas.ipc.adm.br/efrete/veiculos/';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
                                    'xmlns:veic="http://schemas.ipc.adm.br/efrete/veiculos" '+
                                    'xmlns:obj="http://schemas.ipc.adm.br/efrete/veiculos/objects" ' +
                                    'xmlns:obj1="http://schemas.ipc.adm.br/efrete/objects"';
      end;

    opGravarMotorista:
      begin
        Servico := 'http://schemas.ipc.adm.br/efrete/motoristas/';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
                                    'xmlns:mot="http://schemas.ipc.adm.br/efrete/motoristas" '+
                                    'xmlns:obj="http://schemas.ipc.adm.br/efrete/motoristas/objects" ' +
                                    'xmlns:obj1="http://schemas.ipc.adm.br/efrete/objects"';
      end;

  else
    begin
      Servico  := 'http://schemas.ipc.adm.br/efrete/pef/';

      FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
                                  'xmlns:pef="http://schemas.ipc.adm.br/efrete/pef" ' +
                                  'xmlns:obj="http://schemas.ipc.adm.br/efrete/pef/objects" ' +
                                  'xmlns:obj1="http://schemas.ipc.adm.br/efrete/objects"';
    end;
  end;

  case FContratos.Items[0].CIOT.Integradora.Operacao of
    opLogin:
      begin
        FPArqEnv  := 'ped-Login';
        FPArqResp := 'res-Login';
        Acao      := 'Login';
      end;

    opLogout:
      begin
        FPArqEnv  := 'ped-Logout';
        FPArqResp := 'res-Logout';
        Acao      := 'Logout';
        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:obj1="http://schemas.ipc.adm.br/efrete/objects"';
      end;

    opGravarProprietario:
      begin
        FPArqEnv  := 'ped-GravarProp';
        FPArqResp := 'res-GravarProp';
        Acao      := 'Gravar';
      end;

    opGravarVeiculo:
      begin
        FPArqEnv  := 'ped-GravarVeic';
        FPArqResp := 'res-GravarVeic';
        Acao      := 'Gravar';
      end;

    opGravarMotorista:
      begin
        FPArqEnv  := 'ped-GravarMot';
        FPArqResp := 'res-GravarMot';
        Acao      := 'Gravar';
      end;

    opObterCodigoIOT:
      begin
        FPArqEnv  := 'ped-ObterCodigoIOT';
        FPArqResp := 'res-ObterCodigoIOT';
        Acao      := 'ObterCodigoIdentificacaoOperacaoTransportePorIdOperacaoCliente';
      end;

    opObterPdf:
      begin
        FPArqEnv  := 'ped-ObterOperTranspPDF';
        FPArqResp := 'res-ObterOperTranspPDF';
        Acao      := 'ObterOperacaoTransportePdf';
      end;

    opAdicionar:
      begin
        FPArqEnv  := 'ped-AdicOperTransp';
        FPArqResp := 'res-AdicOperTransp';
        Acao      := 'AdicionarOperacaoTransporte';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:adic="http://schemas.ipc.adm.br/efrete/pef/' + Acao + '"';
      end;

    opRetificar:
      begin
        FPArqEnv  := 'ped-RetifOperTransp';
        FPArqResp := 'res-RetifOperTransp';
        Acao      := 'RetificarOperacaoTransporte';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:ret="http://schemas.ipc.adm.br/efrete/pef/' + Acao + '"';
      end;

    opCancelar:
      begin
        FPArqEnv  := 'ped-CancOperTransp';
        FPArqResp := 'res-CancOperTransp';
        Acao      := 'CancelarOperacaoTransporte';
      end;

    opAdicionarViagem:
      begin
        FPArqEnv  := 'ped-AdicViagem';
        FPArqResp := 'res-AdicViagem';
        Acao      := 'AdicionarViagem';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:adic="http://schemas.ipc.adm.br/efrete/pef/' + Acao + '"';
      end;

    opAdicionarPagamento:
      begin
        FPArqEnv  := 'ped-AdicPag';
        FPArqResp := 'res-AdicPag';
        Acao      := 'AdicionarPagamento';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:adic="http://schemas.ipc.adm.br/efrete/pef/' + Acao + '"';
      end;

    opCancelarPagamento:
      begin
        FPArqEnv  := 'ped-CancPag';
        FPArqResp := 'res-CancPag';
        Acao      := 'CancelarPagamento';
      end;

    opEncerrar:
      begin
        FPArqEnv  := 'ped-EncerOperTransp';
        FPArqResp := 'res-EncerOperTransp';
        Acao      := 'EncerrarOperacaoTransporte';

        FPSoapEnvelopeAtributtes := FPSoapEnvelopeAtributtes +
              ' xmlns:enc="http://schemas.ipc.adm.br/efrete/pef/' + Acao + '"';
      end;

    opConsultarTipoCarga:
      begin
        FPArqEnv  := 'ped-ConsultarTipoCarga';
        FPArqResp := 'res-ConsultarTipoCarga';
        Acao      := 'ConsultarTipoCarga';
      end;

    opAlterarDataLiberacaoPagamento:
      begin
        FPArqEnv  := 'ped-AlterarDataLiberacaoPagamento';
        FPArqResp := 'res-AlterarDataLiberacaoPagamento';
        Acao      := 'AlterarDataLiberacaoPagamento';
      end;
  end;

  FPServico := CURL_WSDL + Acao;
  FPSoapAction := Servico + Acao;
end;

procedure TCIOTEnviar.DefinirURL;
begin
  case FContratos.Items[0].CIOT.Integradora.Operacao of
    opLogin,
    opLogout:
      FPLayout := LayeFreteLogon;

    opGravarProprietario:
      FPLayout := layeFreteProprietarios;

    opGravarVeiculo:
      FPLayout := LayeFreteVeiculos;

    opGravarMotorista:
      FPLayout := LayeFreteMotoristas;
(*
    LayeFreteFaturamentoTransportadora
*)
  else
    FPLayout := LayeFreteOperacaoTransporte;
  end;

  inherited DefinirURL;
end;

destructor TCIOTEnviar.Destroy;
begin
  FRetornoEnvio.Free;

  inherited Destroy;
end;

function TCIOTEnviar.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Enviar Documento:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TCIOTEnviar.GerarMsgLog: String;
var
  xMsg: String;
begin
  xMsg := '';

  if CodRetorno <> 0 then
  begin
    xMsg := Format(ACBrStr('Controle Negocial:' + LineBreak +
                           ' Codigo Retorno: %s ' + LineBreak +
                           ' Mensagem: %s ' + LineBreak),
                 [CodRetorno,
                  FPMsg]);
  end;

  Result := xMsg;
end;

function TCIOTEnviar.TratarResposta: Boolean;
var
  NomeArq: string;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');

  FRetornoEnvio.Integradora := FPConfiguracoesCIOT.Geral.Integradora;
  FRetornoEnvio.Leitor.Arquivo := ParseText(FPRetWS);
  FRetornoEnvio.LerXml;

  if FRetornoEnvio.RetEnvio.PDF <> '' then
  begin
    if FNomePDF = '' then
      NomeArq := PathWithDelim(GerarPathDownload) + FRetornoEnvio.RetEnvio.ProtocoloServico + '.pdf'
    else
      NomeArq := PathWithDelim(GerarPathDownload) + FNomePDF + '.pdf';

    WriteToTXT(NomeArq, FRetornoEnvio.RetEnvio.PDF, False, False, True);

    FRetornoEnvio.RetEnvio.PDFNomeArquivo := NomeArq;
  end;

  FPMsg := '';

  if FRetornoEnvio.RetEnvio.Mensagem <> '' then
    FPMsg := FRetornoEnvio.RetEnvio.Mensagem + LineBreak +
             FRetornoEnvio.RetEnvio.Codigo
  else
    FPMsg := FRetornoEnvio.RetEnvio.Sucesso + LineBreak +
             FRetornoEnvio.RetEnvio.ProtocoloServico;

  Result := (FRetornoEnvio.RetEnvio.Sucesso = 'true');
end;

function TCIOTEnviar.GerarPathDownload: String;
var
  Data: TDateTime;
begin
  Data := Now;

  Result := FPConfiguracoesCIOT.Arquivos.GetPathDownload(
                                         FPConfiguracoesCIOT.Geral.Usuario,
                                         FPConfiguracoesCIOT.Geral.CNPJEmitente,
                                         '',
                                         Data);
end;

end.
