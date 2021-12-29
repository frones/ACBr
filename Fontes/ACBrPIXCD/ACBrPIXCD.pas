{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrPIXCD;

interface

uses
  Classes, SysUtils,
  httpsend, ssl_openssl,
  ACBrBase,
  ACBrPIXBase, ACBrPIXQRCodeEstatico,
  ACBrPIXSchemasPixConsultados, ACBrPIXSchemasProblema;

const
  CHttpTimeOutDef = 90000;
  CHttpContentTypeJSon = 'application/json';

resourcestring
  CErroRecebedorNome = 'Nome do Recebedor não informado';
  CErroRecebedorCidade = 'Cidade do Recebedor não informada';
  CErroPSPNaoAtribuido = 'Componente ACBrPSP não atribuido a ACBrPixCD';
  CErroPSPChavePIX = 'Chave Pix não informada';
  CErroPSPTipoChave = 'Chave Pix inválida';

type

  TACBrPixCD = class;
  TACBrPSP = class;

  // Classes com comandos para EndPoints

  { TACBrPixEndPoint }

  TACBrPixEndPoint = class
  private
    fPSP: TACBrPSP;
    fHTTP: THTTPSend;
    fProblema: TACBrPIXProblema;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    property Problema: TACBrPIXProblema read fProblema;
  end;

  { TACBrPixEndPointPix }

  TACBrPixEndPointPix = class( TACBrPixEndPoint)
  private
    fPixConsultados: TACBrPIXConsultados;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    procedure ConsultarPixRecebidos(Inicio: TDateTime; Fim: TDateTime;
      TxId: String = ''; CpfCnpj: String = '';
      PagAtual: Integer = 0; ItensPorPagina: Integer = 100);

    property PixConsultados: TACBrPIXConsultados read fPixConsultados;
  end;

  TACBrQuandoTransmitirHttp = procedure(var AURL: String; const AHttp: THTTPSend) of object ;

  { TACBrPSP - O Componente Base, para os PSPs, deve ser conectado em TACBrPixCD}

  TACBrPSP = class(TACBrComponent)
  private
    fChavePIX: String;
    fQuandoTransmitirHttp: TACBrQuandoTransmitirHttp;
    fURLProd: String;
    fURLTest: String;
    fURL: String;
    fClientID: AnsiString;
    fClientSecret: AnsiString;
    fk1, fk2: String;
    fTipoChave: TACBrPIXTipoChave;

    fepPix: TACBrPixEndPointPix;
    fPixCD: TACBrPixCD;
    fHttpSend: THTTPSend;

    function GetClientID: String;
    procedure SetClientID(AValue: String);
    function GetClientSecret: String;
    procedure SetClientSecret(AValue: String);
    procedure VerificarPIXCDAtribuido;
    procedure SetChavePIX(AValue: String);
    procedure SetACBrPixCD(AValue: TACBrPixCD);
  protected
    fpAutenticado: Boolean;
    fpValidadeToken: TDateTime;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure VerificarValidadeToken; virtual;
    procedure RenovarToken; virtual;
    procedure ConfigurarHTTP; virtual;
    procedure ConfigurarProxy; virtual;
    procedure ConfigurarTimeOut; virtual;
    procedure ConfigurarHeader; virtual;
    procedure ConfigurarChaves; virtual;
    procedure TransmitirHttp; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    procedure VerificarAutenticacao; virtual;
    procedure Autenticar; virtual;
    property Autenticado: Boolean read fpAutenticado;
    property ValidadeToken: TDateTime read fpValidadeToken;

    property epPix: TACBrPixEndPointPix read fepPix;
    property Http: THTTPSend read fHttpSend;

    procedure HTTPGet(const AURL : String); virtual;
    procedure HTTPDelete(const AURL : String); virtual;
    Procedure HTTPPost(const AURL : String; const APostData: AnsiString;
      const ContentType: String = CHttpContentTypeJSon) ; virtual;
    procedure HTTPPut(const AURL: String; const APostData: AnsiString;
      const ContentType: String = CHttpContentTypeJSon) ; virtual;
  published
    property ChavePIX: String read fChavePIX write SetChavePIX;
    property TipoChave: TACBrPIXTipoChave read fTipoChave;

    property ACBrPixCD: TACBrPixCD read fPixCD write SetACBrPixCD;

    property URLProd: String read fURLProd write fURLProd;
    property URLTest: String read fURLTest write fURLTest;

    property ClientID: String read GetClientID write SetClientID;
    property ClientSecret: String read GetClientSecret write SetClientSecret;

    property QuandoTransmitirHttp : TACBrQuandoTransmitirHttp
       read fQuandoTransmitirHttp write fQuandoTransmitirHttp;
  end;

  { TACBrPixRecebedor }

  TACBrPixRecebedor = class
  private
    fCEP: String;
    fCidade: String;
    fCodCategoriaComerciante: Integer;
    fNome: String;

    procedure SetCEP(AValue: String);
    procedure SetCidade(AValue: String);
    procedure SetCodCategoriaComerciante(AValue: Integer);
    procedure SetNome(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPixRecebedor);
  published
    property Nome: String read fNome write SetNome;
    property Cidade: String read fCidade write SetCidade;
    property CEP: String read fCEP write SetCEP;
    property CodCategoriaComerciante: Integer read fCodCategoriaComerciante  // https://classification.codes/classifications/industry/mcc/
      write SetCodCategoriaComerciante;
  end;

  { TACBrHttpProxy }

  TACBrHttpProxy = class
  private
    fHost: String;
    fPass: String;
    fPort: String;
    fUser: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrHttpProxy);
  published
    property Host: String read fHost write fHost;
    property Port: String read fPort write fPort;
    property User: String read fUser write fUser;
    property Pass: String read fPass write fPass;
  end;

  { TACBrPixCD - O Componente em si...}

  TACBrPixCD = class(TACBrComponent)
  private
    fArqLOG: String;
    fProxy: TACBrHttpProxy;
    fPSP: TACBrPSP;
    fQuandoGravarLog: TACBrGravarLog;
    fRecebedor: TACBrPixRecebedor;
    fTimeOut: Integer;

    procedure SetACBrPSP(AValue: TACBrPSP);
    procedure SetProxy(AValue: TACBrHttpProxy);
    procedure SetRecebedor(AValue: TACBrPixRecebedor);

    procedure VerificarPSPAtribuido;
    procedure GravarLogEmArquivo(const ALinha: String) ;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegistrarLog(const ALinha: String);

    function GerarQRCodeEstatico(Valor: Currency; infoAdicional: String = ''; TxId: String = ''): String;
  published
    property Recebedor: TACBrPixRecebedor read fRecebedor write SetRecebedor;
    property Proxy: TACBrHttpProxy read fProxy write SetProxy;
    property TimeOut: Integer read fTimeOut write fTimeOut default CHttpTimeOutDef;

    property ACBrPSP: TACBrPSP read fPSP write SetACBrPSP;

    property ArqLOG: String read fArqLOG write fArqLOG;
    property QuandoGravarLog: TACBrGravarLog read fQuandoGravarLog write fQuandoGravarLog;
  end;

implementation

uses
  ACBrUtil,
  ACBrPIXUtil;

{ TACBrPixEndPoint }

constructor TACBrPixEndPoint.Create(AOwner: TACBrPSP);
begin
  inherited Create;
  fPSP := AOwner;
  fHTTP := fPSP.fpHttpSend;
end;

destructor TACBrPixEndPoint.Destroy;
begin
  fProblema.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPoint.Clear;
begin
  fProblema.Clear;
end;

{ TACBrPixEndPointPix }

constructor TACBrPixEndPointPix.Create(AOwner: TACBrPSP);
begin
  inherited Create(AOwner);
  fPixConsultados := TACBrPIXConsultados.Create;
  Clear;
end;

destructor TACBrPixEndPointPix.Destroy;
begin
  fPixConsultados.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointPix.Clear;
begin
  inherited Clear;
  fPixConsultados.Clear;
end;

procedure TACBrPixEndPointPix.ConsultarPixRecebidos(Inicio: TDateTime;
  Fim: TDateTime; TxId: String; CpfCnpj: String; PagAtual: Integer;
  ItensPorPagina: Integer);
begin

end;

{ TACBrPSP }

constructor TACBrPSP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpAutenticado := False;
  fpValidadeToken := 0;
  fHttpSend := THTTPSend.Create;
  fepPix := TACBrPixEndPointPix.Create(Self);
  fQuandoTransmitirHttp := Nil;
  Clear;
end;

destructor TACBrPSP.Destroy;
begin
  fHttpSend.Free;
  fepPix.Free;
  inherited Destroy;
end;

procedure TACBrPSP.Clear;
begin
  fpHttpSend.Clear;
  fepPix.Clear;
  fChavePIX := '';
  fTipoChave := tchNenhuma;
  fURL := '';
end;

procedure TACBrPSP.SetACBrPixCD(AValue: TACBrPixCD);
var
  va: TACBrPixCD;
begin
  if (AValue = fPixCD) then
    Exit;

  if Assigned(fPixCD) then
    fPixCD.RemoveFreeNotification(Self);

  va := fPixCD;       // Usa outra variavel para evitar Loop Infinito,
  fPixCD := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.ACBrPSP) then
      va.ACBrPSP := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.ACBrPSP := Self;
  end ;
end;

procedure TACBrPSP.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation) ;
  if (Operation = opRemove) and (fPixCD <> Nil) and (AComponent = fPixCD) then
    fPixCD := Nil
end;

procedure TACBrPSP.VerificarPIXCDAtribuido;
begin
  if not Assigned(fPixCD) then
    raise EACBrPixException.Create(ACBrStr(CErroPSPNaoAtribuido));
end;

function TACBrPSP.GetClientID: String;
begin
  Result := StrCrypt(fClientID, fk1);
end;

procedure TACBrPSP.SetClientID(AValue: String);
begin
  if (fk1 <> '') and (fClientID = StrCrypt(AValue, fk1)) then
    Exit;

  fk1 := FormatDateTime('hhnnsszzz',Now);
  fClientID := StrCrypt(AValue, fk1);  // Salva de forma Criptografada, para evitar "Inspect"
end;

function TACBrPSP.GetClientSecret: String;
begin
  Result := StrCrypt(fClientSecret, fk2);
end;

procedure TACBrPSP.SetClientSecret(AValue: String);
begin
  if (fk2 <> '') and (fClientSecret = StrCrypt(AValue, fk2)) then
    Exit;

  fk2 := FormatDateTime('hhnnsszzz',Now);
  fClientSecret := StrCrypt(AValue, fk2);  // Salva de forma Criptografada, para evitar "Inspect"
end;

procedure TACBrPSP.SetChavePIX(AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  if (fChavePix = AValue) then
    Exit;

  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tchNenhuma) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroChaveInvalida), [AValue]);

  fChavePix := Trim(AValue);
  fTipoChave := TipoChave;
end;

procedure TACBrPSP.ConfigurarHTTP;
begin
  Clear;
  ConfigurarProxy;
  ConfigurarTimeOut;
  ConfigurarHTTP;
  ConfigurarChaves;
end;

procedure TACBrPSP.ConfigurarProxy;
begin
  VerificarPIXCDAtribuido;
  fpHttpSend.ProxyHost := fPixCD.Proxy.Host;
  fpHttpSend.ProxyPort := fPixCD.Proxy.Port;
  fpHttpSend.ProxyUser := fPixCD.Proxy.User;
  fpHttpSend.ProxyPass := fPixCD.Proxy.Pass;
end;

procedure TACBrPSP.ConfigurarTimeOut;
begin
  VerificarPIXCDAtribuido;
  if (fPixCD.TimeOut = 0) then
    Exit;

  fpHttpSend.Timeout := fPixCD.TimeOut;
  with fpHttpSend.Sock do
  begin
    ConnectionTimeout := fPixCD.TimeOut;
    InterPacketTimeout := False;
    NonblockSendTimeout := fPixCD.TimeOut;
    SocksTimeout := fPixCD.TimeOut;
    HTTPTunnelTimeout := fPixCD.TimeOut;
  end;
end;

procedure TACBrPSP.ConfigurarHeader;
begin

end;

procedure TACBrPSP.ConfigurarChaves;
begin

end;

procedure TACBrPSP.TransmitirHttp;
begin
  if Assigned(fQuandoTransmitirHttp) then
    fQuandoTransmitirHttp(fURL, fHttpSend);


end;

procedure TACBrPSP.VerificarValidadeToken;
begin
  if (ValidadeToken <> 0) and (ValidadeToken < Now) then
    RenovarToken;
end;

procedure TACBrPSP.RenovarToken;
begin
  { Método Virtual }
end;

procedure TACBrPSP.VerificarAutenticacao;
begin
  if not Autenticado then
    Autenticar;

  VerificarValidadeToken;
end;

procedure TACBrPSP.Autenticar;
begin
  fpAutenticado := True;
end;

procedure TACBrPSP.HTTPGet(const AURL: String);
begin

end;

procedure TACBrPSP.HTTPDelete(const AURL: String);
begin

end;

procedure TACBrPSP.HTTPPost(const AURL: String; const APostData: AnsiString;
  const ContentType: String);
begin

end;

procedure TACBrPSP.HTTPPut(const AURL: String; const APostData: AnsiString;
  const ContentType: String);
begin

end;

{ TACBrPixRecebedor }

constructor TACBrPixRecebedor.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrPixRecebedor.Clear;
begin
  fNome := '';
  fCidade := '';
  fCEP := '';
  fCodCategoriaComerciante := 0;
end;

procedure TACBrPixRecebedor.Assign(Source: TACBrPixRecebedor);
begin
  fNome := Source.Nome;
  fCidade := Source.Cidade;
  fCEP := Source.CEP;
  fCodCategoriaComerciante := Source.CodCategoriaComerciante;
end;

procedure TACBrPixRecebedor.SetCEP(AValue: String);
begin
  if (fCEP = AValue) then
    Exit;

  fCEP := OnlyNumber(AValue);
end;

procedure TACBrPixRecebedor.SetCidade(AValue: String);
begin
  if (fCidade = AValue) then
    Exit;

  fCidade := copy(Trim(AValue),1,15);
end;

procedure TACBrPixRecebedor.SetNome(AValue: String);
begin
  if (fNome = AValue) then
    Exit;

  fNome := copy(Trim(AValue),1,25);
end;

procedure TACBrPixRecebedor.SetCodCategoriaComerciante(AValue: Integer);
begin
  if (fCodCategoriaComerciante = AValue) then
    Exit;

  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(ACBrStr(sErroMCCForaDaFaixa));

  fCodCategoriaComerciante := AValue;
end;

{ TACBrHttpProxy }

constructor TACBrHttpProxy.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrHttpProxy.Clear;
begin
  fHost := '';
  fPass := '';
  fPort := '';
  fUser := '';
end;

procedure TACBrHttpProxy.Assign(Source: TACBrHttpProxy);
begin
  fHost := Source.Host;
  fPass := Source.Pass;
  fPort := Source.Port;
  fUser := Source.User;
end;

{ TACBrPixCD }

constructor TACBrPixCD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRecebedor := TACBrPixRecebedor.Create;
  fProxy := TACBrHttpProxy.Create;
  fTimeOut := CHttpTimeOutDef;
  fArqLOG := '';
  fQuandoGravarLog := Nil;
end;

destructor TACBrPixCD.Destroy;
begin
  fRecebedor.Free;
  fProxy.Free;

  inherited Destroy;
end;

procedure TACBrPixCD.RegistrarLog(const ALinha: String);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned(fQuandoGravarLog) then
    fQuandoGravarLog(ALinha, Tratado);

  if not Tratado then
    GravarLogEmArquivo(AString);
end;

procedure TACBrPixCD.GravarLogEmArquivo(const ALinha: String);
begin
  if (fArqLOG = '') then
    Exit;

  WriteLog( fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + ALinha);
end;

procedure TACBrPixCD.SetACBrPSP(AValue: TACBrPSP);
var
  va: TACBrPSP ;
begin
  if (AValue = fPSP) then
    Exit;

  if Assigned(fPSP) then
    fPSP.RemoveFreeNotification(Self);

  va := fPSP;       // Usa outra variavel para evitar Loop Infinito,
  fPSP := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.ACBrPixCD) then
      va.ACBrPixCD := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.ACBrPixCD := Self;
  end ;
end;

procedure TACBrPixCD.SetProxy(AValue: TACBrHttpProxy);
begin
  if (fProxy <> AValue) then
    fProxy.Assign(AValue);
end;

procedure TACBrPixCD.SetRecebedor(AValue: TACBrPixRecebedor);
begin
  if (fRecebedor <> AValue) then
    fRecebedor.Assign(AValue);
end;

procedure TACBrPixCD.VerificarPSPAtribuido;
begin
  if not Assigned(fPSP) then
    raise EACBrPixException.Create(ACBrStr(CErroPSPNaoAtribuido));
end;

procedure TACBrPixCD.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (fPSP <> nil) and (AComponent = fPSP) then
    fPSP := nil ;
end;

function TACBrPixCD.GerarQRCodeEstatico(Valor: Currency; infoAdicional: String;
  TxId: String): String;
var
  Erros: String;
  QRCodeEstatico: TACBrPIXQRCodeEstatico;
begin
  VerificarPSPAtribuido;

  Erros := '';
  if (fRecebedor.Nome = '') then
    Erros := Erros + CErroRecebedorNome + sLineBreak;

  if (fRecebedor.Cidade = '') then
    Erros := Erros + CErroRecebedorCidade + sLineBreak;

  if (fPSP.ChavePIX = '') then
    Erros := Erros + CErroPSPChavePIX + sLineBreak;

  if (fPSP.TipoChave = tchNenhuma) then
    Erros := Erros + CErroPSPTipoChave + sLineBreak;

  if (Erros <> '') then
    raise EACBrPixException.Create(ACBrStr(Erros));

  QRCodeEstatico := TACBrPIXQRCodeEstatico.Create;
  try
    QRCodeEstatico.NomeRecebedor := fRecebedor.Nome;
    QRCodeEstatico.CidadeRecebedor := fRecebedor.Cidade;
    QRCodeEstatico.CEPRecebedor := fRecebedor.CEP;
    QRCodeEstatico.ChavePix := fPSP.ChavePIX;
    QRCodeEstatico.Valor := Valor;
    QRCodeEstatico.infoAdicional := infoAdicional;
    QRCodeEstatico.TxId := TxId;

    Result := QRCodeEstatico.QRCode;
  finally
    QRCodeEstatico.Free;
  end;
end;

end.

