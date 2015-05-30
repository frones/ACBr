{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

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

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeSSL;

interface

uses
  Classes, SysUtils;

type
  { TDFeSSLClass }

  TDFeSSL = class;

  TDFeSSLClass = class
  private
  protected
    FpDFeSSL: TDFeSSL;
    FpInicializado: Boolean;

    function GetCertDataVenc: TDateTime; virtual;
    function GetCertNumeroSerie: String; virtual;
    function GetCertSubjectName: String; virtual;
    function GetCertCNPJ: String; virtual;
    function GetHTTPResultCode: Integer; virtual;
    function GetInternalErrorCode: Integer; virtual;

    function SignatureElement(const URI: String; AddX509Data: Boolean): String;
      virtual;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;

    property Inicializado: Boolean read FpInicializado;

    procedure Inicializar; virtual;
    procedure DesInicializar; virtual;

    function Assinar(const ConteudoXML, docElement, infElement: String): String;
      virtual;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String): String; virtual;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; virtual;
    function VerificarAssinatura(const ConteudoXML: String;
      out MsgErro: String): Boolean; virtual;

    procedure CarregarCertificado; virtual;
    procedure DescarregarCertificado; virtual;
    function SelecionarCertificado: String; virtual;

    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertSubjectName: String read GetCertSubjectName;
    property CertCNPJ: String read GetCertCNPJ;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;
  end;


  TSSLLib = (libNone, libOpenSSL, libCapicom, libCapicomDelphiSoap);

  { TDFeSSL }

  TDFeSSL = class
  private
    FArquivoPFX: String;
    FDadosPFX: AnsiString;
    FNameSpaceURI: String;
    FNumeroSerie: String;
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;
    FSenha: AnsiString;
    FK: String;
    FSSLClass: TDFeSSLClass;
    FSSLLib: TSSLLib;
    FTimeOut: Integer;
    FUnloadSSLLib: Boolean;
    FCertificadoLido: Boolean;

    function GetCertCNPJ: String;
    function GetCertDataVenc: TDateTime;
    function GetCertNumeroSerie: String;
    function GetCertSubjectName: String;
    function GetHTTPResultCode: Integer;
    function GetInternalErrorCode: Integer;
    function GetSenha: AnsiString;

    procedure InitSSLClass(LerCertificado: Boolean = True);
    procedure DeInitSSLClass;
    procedure SetArquivoPFX(AValue: String);
    procedure SetDadosPFX(AValue: AnsiString);
    procedure SetNumeroSerie(AValue: String);
    procedure SetSenha(AValue: AnsiString);

    procedure SetSSLLib(ASSLLib: TSSLLib);
  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;

    // Nota: ConteudoXML, DEVE estar em UTF8 //
    function Assinar(const ConteudoXML, docElement, infElement: String): String;
    // Envia por SoapAction o ConteudoXML para URL. Retorna a resposta do Servico //
    function Enviar(var ConteudoXML: String; const URL: String;
      const SoapAction: String): String;
    // Valida um Arquivo contra o seu Schema. Retorna True se OK, preenche MsgErro se False //
    function Validar(const ConteudoXML: String; ArqSchema: String;
      out MsgErro: String): Boolean;
    // Verifica se assinatura de um XML é válida. Retorna True se OK, preenche MsgErro se False //
    function VerificarAssinatura(const ConteudoXML: String;
      out MsgErro: String): Boolean;

    procedure CarregarCertificado;
    procedure DescarregarCertificado;
    function SelecionarCertificado: String;

    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertSubjectName: String read GetCertSubjectName;
    property CertCNPJ: String read GetCertCNPJ;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;

  published
    property SSLLib: TSSLLib read FSSLLib write SetSSLLib;
    property UnloadSSLLib: Boolean read FUnloadSSLLib write FUnloadSSLLib default True;
    property ArquivoPFX: String read FArquivoPFX write SetArquivoPFX;
    property DadosPFX: AnsiString read FDadosPFX write SetDadosPFX;
    property NumeroSerie: String read FNumeroSerie write SetNumeroSerie;
    property Senha: AnsiString read GetSenha write SetSenha;

    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;

    property TimeOut: Integer read FTimeOut write FTimeOut default 5000;
    property NameSpaceURI: String read FNameSpaceURI write FNameSpaceURI;
  end;


implementation

uses strutils, ACBrUtil, ACBrDFeException, ACBrDFeOpenSSL
  {$IFDEF MSWINDOWS}
  , ACBrDFeCapicom {$IFNDEF FPC}, ACBrDFeCapicomDelphiSoap{$ENDIF}
  {$ENDIF};

{ TDFeSSL }

constructor TDFeSSL.Create;
begin
  inherited Create;

  Clear;
end;

procedure TDFeSSL.Clear;
begin
  FArquivoPFX  := '';
  FDadosPFX    := '';
  FNumeroSerie := '';
  FProxyHost   := '';
  FProxyPass   := '';
  FProxyPort   := '';
  FProxyUser   := '';
  FSenha       := '';
  FSSLLib      := libNone;
  FTimeOut     := 5000;
  FNameSpaceURI:= '';
  FUnloadSSLLib:= True;
  FCertificadoLido := False;

  if Assigned(FSSLClass) then
    FSSLClass.Free;

  FSSLClass := TDFeSSLClass.Create(Self);
end;

destructor TDFeSSL.Destroy;
begin
  if Assigned(FSSLClass) then
  begin
    DeInitSSLClass;
    FreeAndNil(FSSLClass);
  end;

  inherited Destroy;
end;

function TDFeSSL.Assinar(const ConteudoXML, docElement, infElement: String): String;
Var
  XmlAss, xmlHeaderAntes, xmlHeaderDepois: String;
  I: integer;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  InitSSLClass;

  // Lendo Header antes de assinar //
  xmlHeaderAntes := '';
  I := pos('?>', ConteudoXML);
  if I > 0 then
    xmlHeaderAntes := copy(ConteudoXML, 1, I + 1);

  XmlAss := FSSLClass.Assinar(ConteudoXML, docElement, infElement);

  // Verificando se modificou o Header do XML assinado, e voltando para o anterior //
  if xmlHeaderAntes <> '' then
  begin
    I := pos('?>', XmlAss);
    if I > 0 then
    begin
      xmlHeaderDepois := copy(XmlAss, 1, I + 1);
      if xmlHeaderAntes <> xmlHeaderDepois then
        XmlAss := StuffString(XmlAss, 1, length(xmlHeaderDepois), xmlHeaderAntes);
    end
    else
      XmlAss := xmlHeaderAntes + XmlAss;
  end;

  Result := XmlAss;
end;

function TDFeSSL.Enviar(var ConteudoXML: String; const URL: String;
  const SoapAction: String): String;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  InitSSLClass;
  Result := FSSLClass.Enviar(ConteudoXML, URL, SoapAction);
end;

function TDFeSSL.Validar(const ConteudoXML: String; ArqSchema: String;
  out MsgErro: String): Boolean;
begin
  InitSSLClass;

  // ArqSchema deve vir com o Path Completo
  if not FileExists(ArqSchema) then
    raise EACBrDFeException.Create('Arquivo ' + sLineBreak + ArqSchema +
      sLineBreak + 'Não encontrado');

  Result := FSSLClass.Validar(ConteudoXML, ArqSchema, MsgErro);
end;

function TDFeSSL.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String): Boolean;
begin
  InitSSLClass;
  Result := FSSLClass.VerificarAssinatura(ConteudoXML, MsgErro);
end;

procedure TDFeSSL.CarregarCertificado;
begin
  InitSSLClass( True );
end;

procedure TDFeSSL.DescarregarCertificado;
begin
  if FSSLClass.Inicializado then
    FSSLClass.DescarregarCertificado;

  FCertificadoLido := False;
end;

function TDFeSSL.SelecionarCertificado: String;
begin
  InitSSLClass(False);
  Result := FSSLClass.SelecionarCertificado;

  if NaoEstaVazio(Result) then
    FSSLClass.CarregarCertificado;
end;

function TDFeSSL.GetCertDataVenc: TDateTime;
begin
  InitSSLClass;
  Result := FSSLClass.CertDataVenc;
end;

function TDFeSSL.GetCertCNPJ: String;
begin
  InitSSLClass;
  Result := FSSLClass.CertCNPJ;
end;

function TDFeSSL.GetCertNumeroSerie: String;
begin
  InitSSLClass;
  Result := FSSLClass.CertNumeroSerie;
end;

function TDFeSSL.GetCertSubjectName: String;
begin
  InitSSLClass;
  Result := FSSLClass.CertSubjectName;
end;

function TDFeSSL.GetHTTPResultCode: Integer;
begin
  Result := FSSLClass.HTTPResultCode;
end;

function TDFeSSL.GetInternalErrorCode: Integer;
begin
  Result := FSSLClass.InternalErrorCode;
end;

function TDFeSSL.GetSenha: AnsiString;
begin
  Result := StrCrypt(FSenha, FK)  // Descritografa a Senha
end;

procedure TDFeSSL.InitSSLClass(LerCertificado: Boolean);
begin
  if not FSSLClass.Inicializado then
    FSSLClass.Inicializar;

  if LerCertificado and (not FCertificadoLido) then
  begin
    FSSLClass.CarregarCertificado;
    FCertificadoLido := True;
  end;
end;

procedure TDFeSSL.DeInitSSLClass;
begin
  DescarregarCertificado;
  FSSLClass.DesInicializar;
end;

procedure TDFeSSL.SetArquivoPFX(AValue: String);
begin
  if FArquivoPFX = AValue then Exit;
  FArquivoPFX := AValue;
  FDadosPFX := '';   // Força a releitura de DadosPFX;
  DescarregarCertificado;
end;

procedure TDFeSSL.SetDadosPFX(AValue: AnsiString);
begin
  if FDadosPFX = AValue then Exit;
  FDadosPFX := AValue;
  DescarregarCertificado;
end;

procedure TDFeSSL.SetNumeroSerie(AValue: String);
begin
  if FNumeroSerie = AValue then Exit;
  FNumeroSerie := AValue;
  DescarregarCertificado;
end;

procedure TDFeSSL.SetSenha(AValue: AnsiString);
begin
  if FSenha = AValue then Exit;
  FK := FormatDateTime('hhnnsszzz',Now);
  FSenha := StrCrypt(AValue, FK);  // Salva Senha de forma Criptografada, para evitar "Inspect"

  DescarregarCertificado;
end;

procedure TDFeSSL.SetSSLLib(ASSLLib: TSSLLib);
begin
  if ASSLLib = FSSLLib then
    exit;

  if Assigned(FSSLClass) then
    FreeAndNil(FSSLClass);

  {$IFDEF MSWINDOWS}
  case ASSLLib of
    libCapicom: FSSLClass := TDFeCapicom.Create(Self);
    libOpenSSL: FSSLClass := TDFeOpenSSL.Create(Self);
    libCapicomDelphiSoap:
    begin
       {$IFNDEF FPC}
      FSSLClass := TDFeCapicomDelphiSoap.Create(Self);
       {$ELSE}
      FSSLClass := TDFeCapicom.Create(Self);
       {$ENDIF}
    end
    else
      FSSLClass := TDFeSSLClass.Create(Self);
  end;
  {$ELSE}
  case ASSLLib of
    libOpenSSL, libCapicom, libCapicomDelphiSoap: FSSLClass :=
        TDFeOpenSSL.Create(Self);
  end;
  {$ENDIF}

  FSSLLib := ASSLLib;
end;

{ TDFeSSLClass }

constructor TDFeSSLClass.Create(ADFeSSL: TDFeSSL);
begin
  FpDFeSSL := ADFeSSL;
  FpInicializado := False;
end;

procedure TDFeSSLClass.Inicializar;
begin
  if FpInicializado then exit ;

  CarregarCertificado;
  FpInicializado := True;
end;

procedure TDFeSSLClass.DesInicializar;
begin
  if not FpInicializado then exit;

  DescarregarCertificado;
  FpInicializado := False;
end;

function TDFeSSLClass.Assinar(const ConteudoXML, docElement, infElement: String): String;
begin
  Result := '';
  raise EACBrDFeException.Create(ClassName + '.Assinar, não implementado');
end;

function TDFeSSLClass.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String): String;
begin
  Result := '';
  raise EACBrDFeException.Create(ClassName + '.Enviar não implementado');
end;

function TDFeSSLClass.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create('"Validar" não suportado em: ' + ClassName);
end;

function TDFeSSLClass.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String): Boolean;
begin
  Result := False;
  raise EACBrDFeException.Create('"ValidarAssinatura" não suportado em: ' + ClassName);
end;

function TDFeSSLClass.SelecionarCertificado: String;
begin
  Result := '';
  raise EACBrDFeException.Create('"SelecionarCertificado" não suportado em: ' +
    ClassName);
end;

procedure TDFeSSLClass.CarregarCertificado;
begin
  { nada aqui, método virtual}
end;

procedure TDFeSSLClass.DescarregarCertificado;
begin
  { nada aqui, método virtual}
end;

function TDFeSSLClass.GetHTTPResultCode: Integer;
begin
  Result := 0;
end;

function TDFeSSLClass.GetInternalErrorCode: Integer;
begin
  Result := 0;
end;

function TDFeSSLClass.GetCertDataVenc: TDateTime;
begin
  Result := 0;
end;

function TDFeSSLClass.GetCertNumeroSerie: String;
begin
  Result := '';
end;

function TDFeSSLClass.GetCertSubjectName: String;
begin
  Result := '';
end;

function TDFeSSLClass.GetCertCNPJ: String;
begin
  Result := '';
end;

function TDFeSSLClass.SignatureElement(const URI: String; AddX509Data: Boolean): String;
begin
  {(*}
  Result :=
  '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
    '<SignedInfo>' +
      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
      '<Reference URI="#' + URI + '">' +
        '<Transforms>' +
          '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
          '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
        '</Transforms>' +
        '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
        '<DigestValue></DigestValue>' +
      '</Reference>' +
    '</SignedInfo>' +
    '<SignatureValue></SignatureValue>' +
    '<KeyInfo>' +
    IfThen(AddX509Data,
      '<X509Data>' +
        '<X509Certificate></X509Certificate>' +
      '</X509Data>',
      '')+
    '</KeyInfo>'+
  '</Signature>';
  {*)}
end;

end.
