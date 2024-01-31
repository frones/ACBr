{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Daniel Simoes de Almeida                       }
{                               André Ferreira de Moraes                       }
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

unit ACBrDFe;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrBase, ACBrDFeConfiguracoes, ACBrMail, ACBrIntegrador, ACBrDFeSSL,
  pcnConversao;

const
  CSCHEMA_EXT = '.xsd';
  CSCHEMA_SeparadorVersao = '_v';

type
  TACBrDFeOnTransmit = procedure(const Dados, URL, SoapAction, MimeType: String;
    var Resposta: String; var HTTPResultCode: Integer; var InternalErrorCode: Integer) of object ;

  TACBrDFeOnTransmitError = procedure(const HttpError, InternalError: Integer;
    const URL, DadosEnviados, SoapAction: String; var Retentar: Boolean; var Tratado: Boolean) of object ;

  { TACBrDFe }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrDFe = class(TACBrComponent)
  private
    FMAIL: TACBrMail;
    FIntegrador: TACBrIntegrador;
    FOnTransmit: TACBrDFeOnTransmit;
    FOnTransmitError: TACBrDFeOnTransmitError;
    FSSL: TDFeSSL;
    FListaDeSchemas: TStringList;
    FPathSchemas: String;
    FOnStatusChange: TNotifyEvent;
    FOnGerarLog: TACBrGravarLog;
    function GetOnAntesDeAssinar: TDFeSSLAntesDeAssinar;
    procedure SetOnAntesDeAssinar(AValue: TDFeSSLAntesDeAssinar);
    procedure SetMAIL(AValue: TACBrMail);
    procedure SetIntegrador(AValue: TACBrIntegrador);
  protected
    FPConfiguracoes: TConfiguracoes;
    FPIniParams: TMemIniFile;
    FPIniParamsCarregado: Boolean;
    FPSeparadorVersaoSchema: String;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function CreateConfiguracoes: TConfiguracoes; virtual;

    procedure LerParamsIni( ApenasSeNaoLido: Boolean = False); virtual;

    function NomeServicoToNomeSchema(const NomeServico: String): String; virtual;
    function ExtrairVersaoNomeArquivoSchema(const ArqSchema: String): String; virtual;
    function VersaoSchemaDoubleToString(AVersao: Double): String; virtual;
    function VersaoSchemaStringToDouble(const AVersao: String): Double; virtual;
    procedure AchaArquivoSchema(NomeSchema: String; var AVersao: Double; var ArqSchema: String); virtual;

  public
    property SSL: TDFeSSL read FSSL;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetNomeModeloDFe: String; virtual;
    function GetNameSpaceURI: String; virtual;

    function Gravar(NomeArquivo: String; ConteudoXML: String; const aPath: String = '';
      ConteudoEhUTF8: Boolean = True): Boolean;
    procedure EnviarEmail(const sPara, sAssunto: String;
      sMensagem: TStrings = nil; sCC: TStrings = nil; Anexos: TStrings = nil;
      StreamNFe: TStream = nil; const NomeArq: String = ''; sReplyTo: TStrings = nil; sBCC: TStrings = nil); virtual;

    procedure LerServicoChaveDeParams(const NomeSessao, NomeServico: String;
      var Versao: Double; var URL: String);
    procedure LerDeParams(const NomeSessao, NomeServico: String;
      var Versao: Double; var Servico: String);
    procedure LerServicoDeParams(const ModeloDFe, UF: String;
      const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
      var Versao: Double; var URL: String); overload;
    procedure LerServicoDeParams(const ModeloDFe, UF: String;
      const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
      var Versao: Double; var URL: String; var Servico: String; var SoapAction: String);overload;
    function LerVersaoDeParams(const ModeloDFe, UF: String;
      const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
      VersaoBase: Double): Double; virtual;
    function LerURLDeParams(const ModeloDFe, UF: String;
      const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
      VersaoBase: Double): String; virtual;

    procedure FazerLog(const Msg: String; out Tratado: Boolean);
    procedure GerarException(const Msg: String; E: Exception = nil);
    property Configuracoes: TConfiguracoes read FPConfiguracoes write FPConfiguracoes;

  published
    property MAIL: TACBrMail read FMAIL write SetMAIL;
    property Integrador: TACBrIntegrador read FIntegrador write SetIntegrador;
    property OnTransmit : TACBrDFeOnTransmit read FOnTransmit write FOnTransmit;
    property OnTransmitError : TACBrDFeOnTransmitError read FOnTransmitError
       write FOnTransmitError;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;

    property OnGerarLog: TACBrGravarLog read FOnGerarLog write FOnGerarLog;
    property OnAntesDeAssinar: TDFeSSLAntesDeAssinar read GetOnAntesDeAssinar
       write SetOnAntesDeAssinar;
  end;

implementation

uses
  strutils,
  ACBrDFeConsts,
  ACBrDFeException,
  ACBrUtil.Base,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML;

{ TACBrDFe }

constructor TACBrDFe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPSeparadorVersaoSchema := CSCHEMA_SeparadorVersao;
  FPConfiguracoes := CreateConfiguracoes;
  FPConfiguracoes.Name := 'Configuracoes';
  {$IFDEF COMPILER6_UP}
  FPConfiguracoes.SetSubComponent(True);{ para gravar no DFM/XFM }
  {$ENDIF}

  FMAIL := Nil;
  FIntegrador := Nil;

  // Criando uma instância de FSSL e atribuindo valores de "Configuracoes" a ela;
  FSSL := TDFeSSL.Create;
  FListaDeSchemas := TStringList.Create;
  FPathSchemas := '';

  with FSSL do
  begin
    URLPFX := Configuracoes.Certificados.URLPFX;
    ArquivoPFX := Configuracoes.Certificados.ArquivoPFX;
    DadosPFX := Configuracoes.Certificados.DadosPFX;
    NameSpaceURI := GetNameSpaceURI;
    NumeroSerie := Configuracoes.Certificados.NumeroSerie;
    Senha := Configuracoes.Certificados.Senha;

    ProxyHost := Configuracoes.WebServices.ProxyHost;
    ProxyPass := Configuracoes.WebServices.ProxyPass;
    ProxyPort := Configuracoes.WebServices.ProxyPort;
    ProxyUser := Configuracoes.WebServices.ProxyUser;
    TimeOut := Configuracoes.WebServices.TimeOut;
    TimeOutPorThread := Configuracoes.WebServices.TimeOutPorThread;

    SSLCryptLib := Configuracoes.Geral.SSLCryptLib;
    SSLHttpLib := Configuracoes.Geral.SSLHttpLib;
    SSLXmlSignLib := Configuracoes.Geral.SSLXmlSignLib;
  end;

  FOnGerarLog := nil;
  FOnTransmit := nil;
  FOnTransmitError := nil;

  FPIniParams := TMemIniFile.Create(Configuracoes.Arquivos.IniServicos);
  FPIniParamsCarregado := False;
end;

function TACBrDFe.CreateConfiguracoes: TConfiguracoes;
begin
  Result := TConfiguracoes.Create(self);
end;

destructor TACBrDFe.Destroy;
begin
  FSSL.Free;
  FListaDeSchemas.Free;
  FPConfiguracoes.Free;
  FPIniParams.Free;

  inherited;
end;

function TACBrDFe.GetNomeModeloDFe: String;
begin
  Result := '';
end;

function TACBrDFe.GetNameSpaceURI: String;
begin
  Result := '';
end;

function TACBrDFe.GetOnAntesDeAssinar: TDFeSSLAntesDeAssinar;
begin
  Result := FSSL.AntesDeAssinar;
end;

procedure TACBrDFe.SetOnAntesDeAssinar(AValue: TDFeSSLAntesDeAssinar);
begin
  FSSL.AntesDeAssinar := AValue;
end;


function TACBrDFe.Gravar(NomeArquivo: String; ConteudoXML: String;
  const aPath: String; ConteudoEhUTF8: Boolean): Boolean;
var
  UTF8Str, SoNome, SoPath: String;
begin
  Result := False;
  try
    SoNome := ExtractFileName(NomeArquivo);
    if EstaVazio(SoNome) then
      raise EACBrDFeException.Create('Nome de arquivo não informado');

    SoPath := ExtractFilePath(NomeArquivo);
    if EstaVazio(SoPath) then
      SoPath := aPath;
    if EstaVazio(SoPath) then
      SoPath := FPConfiguracoes.Arquivos.PathSalvar;

    SoPath := PathWithDelim(SoPath);

    ConteudoXML := StringReplace(ConteudoXML, '<-><->', '', [rfReplaceAll]);
    { Sempre salva o Arquivo em UTF8, independente de qual seja a IDE...
      FPC já trabalha com UTF8 de forma nativa }
    if ConteudoEhUTF8 and (not XmlEhUTF8(ConteudoXML)) then
      UTF8Str := '<' + ENCODING_UTF8 + '>' + ConteudoXML
    else
      UTF8Str := ConverteXMLtoUTF8(ConteudoXML);

    if not DirectoryExists(SoPath) then
      ForceDirectories(SoPath);

    NomeArquivo := SoPath + SoNome;

    WriteToTXT(NomeArquivo, UTF8Str, False, False);
    Result := True;
  except
    on E: Exception do
      GerarException('Erro ao salvar.', E);
  end;
end;

procedure TACBrDFe.EnviarEmail(const sPara, sAssunto: String; sMensagem: TStrings;
  sCC: TStrings; Anexos: TStrings; StreamNFe: TStream; const NomeArq: String;
  sReplyTo: TStrings; sBCC: TStrings);
var
  i: Integer;
begin
  if not Assigned(MAIL) then
    raise EACBrDFeException.Create('Componente ACBrMail não associado');

  MAIL.Clear;
  MAIL.AddAddress(sPara);
  MAIL.Subject := sAssunto;

  if Assigned(sMensagem) then
  begin
    MAIL.Body.Text := sMensagem.Text;
    MAIL.AltBody.Text := (StripHTML(sMensagem.Text));
  end;

  MAIL.ClearAttachments;
  if Assigned(StreamNFe) then
    MAIL.AddAttachment(StreamNFe, NomeArq, adAttachment);

  if Assigned(Anexos) then
  begin
    for i := 0 to Anexos.Count - 1 do
      MAIL.AddAttachment(Anexos[i], '', adAttachment);
  end;

  if Assigned(sCC) then
  begin
    for i := 0 to sCC.Count - 1 do
      MAIL.AddCC(sCC[i]);
  end;

  if Assigned(sBCC) then
  begin
    for i := 0 to sBCC.Count - 1 do
      MAIL.AddBCC(sBCC[i]);
  end;

  //ReplyTo
  if Assigned(sReplyTo) then
  begin
    for i := 0 to sReplyTo.Count - 1 do
      MAIL.AddReplyTo(sReplyTo[i]);
  end;

  MAIL.Send;
end;

function TACBrDFe.NomeServicoToNomeSchema(const NomeServico: String): String;
begin
  Result := '';
end;

function TACBrDFe.ExtrairVersaoNomeArquivoSchema(const ArqSchema: String): String;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := pos(FPSeparadorVersaoSchema, ArqSchema );
  if (P1 > 0) then
  begin
    P1 := P1 + Length(FPSeparadorVersaoSchema);
    P2 := PosEx(CSCHEMA_EXT, ArqSchema, P1);
    if (P2 = 0) then
      P2 := Length(ArqSchema);

    Result := copy(ArqSchema, P1, P2-P1);
  end;
end;

function TACBrDFe.VersaoSchemaDoubleToString(AVersao: Double): String;
begin
  if (AVersao > 0) then
    Result := FloatToString(AVersao,'.','0.00')
  else
    Result := '';
end;

function TACBrDFe.VersaoSchemaStringToDouble(const AVersao: String): Double;
begin
  Result := StringToFloatDef(AVersao, 0);
end;

procedure TACBrDFe.AchaArquivoSchema(NomeSchema: String; var AVersao: Double;
  var ArqSchema: String);
Var
  VersaoMaisProxima, VersaoArq: Double;
  ArqAtual: String;
  I, P, LenNome: Integer;
begin
  if NomeSchema = '' then exit;

  VersaoMaisProxima := 0;
  ArqSchema := '';

  if (FPathSchemas <> Configuracoes.Arquivos.PathSchemas) then
  begin
    FListaDeSchemas.Clear;
    FPathSchemas := Configuracoes.Arquivos.PathSchemas;
  end;

  if (FListaDeSchemas.Count = 0) then   // Carrega todos arquivos de Schema
  begin
    FindFiles(FPathSchemas +'*'+CSCHEMA_EXT, FListaDeSchemas, False );

    if FListaDeSchemas.Count = 0 then
      raise EACBrDFeException.Create('Nenhum arquivo de Schema encontrado na pasta: '+sLineBreak+
                                     Configuracoes.Arquivos.PathSchemas );

    FListaDeSchemas.Sort;
  end;

  if (AVersao = 0) then
    ArqAtual := NomeSchema + CSCHEMA_EXT
  else
    ArqAtual := NomeSchema + FPSeparadorVersaoSchema + VersaoSchemaDoubleToString(AVersao) + CSCHEMA_EXT ;

  P := FListaDeSchemas.IndexOf(ArqAtual);

  if P >= 0 then
  begin
    ArqSchema := FListaDeSchemas[P];
    VersaoMaisProxima := AVersao;
  end
  else if AVersao > 0 then
  begin
    NomeSchema := NomeSchema + FPSeparadorVersaoSchema;
    LenNome := Length(NomeSchema);

    For I := 0 to FListaDeSchemas.Count-1 do
    begin
      ArqAtual := FListaDeSchemas[I];

      if (copy(ArqAtual, 1, LenNome) = NomeSchema) then
      begin
        VersaoArq := VersaoSchemaStringToDouble(ExtrairVersaoNomeArquivoSchema(ArqAtual));
        if (VersaoArq > 0) and
           (VersaoArq > VersaoMaisProxima) and
           (VersaoArq <= AVersao) then
        begin
          VersaoMaisProxima := VersaoArq;
          ArqSchema := ArqAtual;
        end;
      end;
    end;
  end;

  AVersao := VersaoMaisProxima;
  if NaoEstaVazio(ArqSchema) then
    ArqSchema := Configuracoes.Arquivos.PathSchemas + ArqSchema;
end;

procedure TACBrDFe.LerParamsIni(ApenasSeNaoLido: Boolean);
begin
  if ApenasSeNaoLido and FPIniParamsCarregado then
    exit;

  if Configuracoes.WebServices.Params.Count = 0 then
    Configuracoes.WebServices.LerParams;

  FPIniParams.SetStrings(Configuracoes.WebServices.Params);
  FPIniParamsCarregado := True;
end;

procedure TACBrDFe.LerServicoChaveDeParams(const NomeSessao, NomeServico: String;
  var Versao: Double; var URL: String);
var
  Chave, ChaveBase, K: String;
  SL: TStringList;
  I: integer;
  VersaoAtual, VersaoAchada: Double;
begin
  VersaoAchada := 0;
  URL := '';
  VersaoAtual := Versao;
  LerParamsIni( True );

  if not FPIniParams.SectionExists(NomeSessao) then
    exit;

  ChaveBase := UpperCase(NomeServico + '_');
  Chave := ChaveBase + FloatToString(VersaoAtual,'.','0.00');

  // Achou com busca exata ? (mesma versao) //
  if NaoEstaVazio(FPIniParams.ReadString(NomeSessao, Chave, '')) then
    VersaoAchada := VersaoAtual;

  if VersaoAchada = 0 then
  begin
    // Procure por serviço com o mesmo nome, mas com versão inferior //
    SL := TStringList.Create;
    try
      FPIniParams.ReadSection(NomeSessao, SL);
      for I := 0 to SL.Count-1 do
      begin
        K := UpperCase(SL[I]);

        if copy(K, 1, Length(ChaveBase)) = ChaveBase then
        begin
          VersaoAtual := StringToFloatDef(copy(K, Length(ChaveBase) + 1, Length(K)), 0);

          if (VersaoAtual > VersaoAchada) and (VersaoAtual <= Versao) then
          begin
            VersaoAchada := VersaoAtual;
            Chave := K;
          end;
        end;
      end;
    finally
      SL.Free;
    end;
  end;

  Versao := VersaoAchada;
  if Versao > 0 then
    URL := FPIniParams.ReadString(NomeSessao, Chave, '')
  else
    URL := FPIniParams.ReadString(NomeSessao, NomeServico, '');
end;

procedure TACBrDFe.LerDeParams(const NomeSessao, NomeServico: String;
  var Versao: Double; var Servico: String);
begin
  LerServicoChaveDeParams(NomeSessao,NomeServico,Versao,Servico);
end;

procedure TACBrDFe.LerServicoDeParams(const ModeloDFe, UF: String;
  const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
  var Versao: Double; var URL: String);
var
  Servico, SoapAction: String;
begin
  LerServicoDeParams(ModeloDFe, UF, TipoAmbiente, NomeServico, Versao, URL, Servico, SoapAction);
end;

procedure TACBrDFe.LerServicoDeParams(const ModeloDFe, UF: String;
  const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
  var Versao: Double; var URL: String; var Servico: String;
  var SoapAction: String);
var
  Sessao, ListaSessoes, SessaoUsar, NomeSchema, ArqSchema: String;
  VersaoAchada, VersaoSchema: Double;
begin
  if EstaVazio(ModeloDFe) then
    raise EACBrDFeException.Create('ModeloDFe não pode ser vazio');

  if EstaVazio(UF) then
    raise EACBrDFeException.Create('UF não pode ser vazia');

  Sessao := ModeloDFe + '_' + UF + '_' + IfThen(TipoAmbiente = taProducao, 'P', 'H');
  VersaoAchada := Versao;

  LerServicoChaveDeParams( Sessao, NomeServico, VersaoAchada, URL );
  LerDeParams( FPIniParams.ReadString(Sessao, 'WSDL', ''), NomeServico, VersaoAchada, Servico );
  LerDeParams( FPIniParams.ReadString(Sessao, 'SoapAction', ''), NomeServico, VersaoAchada, SoapAction );

  // Se não achou, verifique se está fazendo redirecionamento "Usar="
  ListaSessoes := '';  // proteção contra redirecionamento circular
  while EstaVazio(URL) do
  begin
    if NaoEstaVazio(Sessao) then
    begin
      if (Pos(Sessao, ListaSessoes) > 0) then
        raise EACBrDFeException.Create('Redirecionamento circular detectado: Sessão "'+Sessao+'" no arquivo "'+
          Configuracoes.WebServices.ResourceName+'"');

      ListaSessoes := ListaSessoes + ';' + Sessao;
    end;

    if FPIniParams.SectionExists(Sessao) then
    begin
      SessaoUsar := FPIniParams.ReadString(Sessao, 'Usar', '');
      if NaoEstaVazio(SessaoUsar) then
      begin
        Sessao := SessaoUsar;
        VersaoAchada := Versao;
        LerServicoChaveDeParams( Sessao, NomeServico, VersaoAchada, URL );
        LerDeParams(  FPIniParams.ReadString(Sessao, 'WSDL', ''), NomeServico, VersaoAchada, Servico );
        LerDeParams( FPIniParams.ReadString(Sessao, 'SoapAction', ''), NomeServico, VersaoAchada, SoapAction );
      end
      else
        raise EACBrDFeException.Create('URL para o serviço "' + NomeServico + '" não encontrada na sessão "'+Sessao+'" no arquivo "'+
                                       Configuracoes.WebServices.ResourceName+'"');
    end
    else
      raise EACBrDFeException.Create('Sessão "'+Sessao+'", não encontrada no arquivo "'+
                                     Configuracoes.WebServices.ResourceName+'"');
  end;

  // tenta procura por Versão na pasta de Schemas //
  NomeSchema := NomeServicoToNomeSchema(NomeServico);
  if NaoEstaVazio(NomeSchema) then
  begin
    VersaoSchema := Versao;
    ArqSchema := '';
    AchaArquivoSchema( NomeSchema, VersaoSchema, ArqSchema );
    if VersaoSchema > 0 then
      VersaoAchada := VersaoSchema;
  end;

  if VersaoAchada > 0 then
    Versao := VersaoAchada;
end;
function TACBrDFe.LerVersaoDeParams(const ModeloDFe, UF: String;
  const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
  VersaoBase: Double): Double;
var
  Versao: Double;
  URL: String;
begin
  Versao := VersaoBase;
  URL := '';

  LerServicoDeParams(ModeloDFe, UF, TipoAmbiente, NomeServico, Versao, URL);
  Result := Versao;
end;

function TACBrDFe.LerURLDeParams(const ModeloDFe, UF: String;
  const TipoAmbiente: TpcnTipoAmbiente; const NomeServico: String;
  VersaoBase: Double): String;
var
  Versao: Double;
  URL: String;
begin
  Versao := VersaoBase;
  URL := '';

  // TODO: Fazer mecanismo de Cache, pois está lendo muitas vezes para a mesma consulta

  LerServicoDeParams(ModeloDFe, UF, TipoAmbiente, NomeServico, Versao, URL);
  Result := URL;
end;

procedure TACBrDFe.FazerLog(const Msg: String; out Tratado: Boolean);
begin
  Tratado := False;
  if (Msg <> '') then
  begin
    if Assigned(OnGerarLog) then
      OnGerarLog(Msg, Tratado);
  end;
end;

procedure TACBrDFe.GerarException(const Msg: String; E: Exception);
var
  Tratado: Boolean;
  MsgErro: String;
begin
  MsgErro := Msg;
  if Assigned(E) then
    MsgErro := MsgErro + sLineBreak + E.Message;

  Tratado := False;
  FazerLog('ERRO: ' + MsgErro, Tratado);

  // MsgErro já está na String Nativa da IDE... por isso deve usar "CreateDef"
  if not Tratado then
    raise EACBrDFeException.CreateDef(MsgErro);
end;

procedure TACBrDFe.SetMAIL(AValue: TACBrMail);
begin
  if AValue <> FMAIL then
  begin
    if Assigned(FMAIL) then
      FMAIL.RemoveFreeNotification(Self);

    FMAIL := AValue;

    if AValue <> Nil then
      AValue.FreeNotification(Self);
  end;
end;

procedure TACBrDFe.SetIntegrador(AValue: TACBrIntegrador);
begin
  if AValue <> FIntegrador then
  begin
    if Assigned(FIntegrador) then
      FIntegrador.RemoveFreeNotification(Self);

    FIntegrador := AValue;

    if AValue <> Nil then
      AValue.FreeNotification(Self);
  end;
end;

procedure TACBrDFe.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (FMAIL <> Nil) and (AComponent is TACBrMail) then
      FMAIL := Nil

    else if (FIntegrador <> Nil) and (AComponent is TACBrIntegrador) then
      FIntegrador := Nil;
  end;
end;

end.
