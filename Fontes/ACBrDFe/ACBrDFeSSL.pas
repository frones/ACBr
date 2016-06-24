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

Const
  CBufferSize = 32768;

type

  { TDFeSSLClass }

  TSSLTipoCertificado = (tpcA1, tpcA3);
  TSSLDgst = (dgstMD2, dgstMD4, dgstMD5, dgstRMD160, dgstSHA, dgstSHA1, dgstSHA256, dgstSHA512) ;
  TSSLHashOutput = (outHexa, outBase64, outBinary) ;

  TDFeSSL = class;

  TDFeSSLClass = class
  private
  protected
    FpDFeSSL: TDFeSSL;
    FpCertificadoLido: Boolean;

    function GetCertDataVenc: TDateTime; virtual;
    function GetCertNumeroSerie: String; virtual;
    function GetCertSubjectName: String; virtual;
    function GetCertRazaoSocial: String; virtual;
    function GetCertCNPJ: String; virtual;
    function GetHTTPResultCode: Integer; virtual;
    function GetInternalErrorCode: Integer; virtual;
    function GetCertTipo: TSSLTipoCertificado; virtual;

    function GetCNPJFromSubjectName(SubjectName: String): String;
    function GetRazaoSocialFromSubjectName(SubjectName: String): String;

    function SignatureElement(const URI: String; AddX509Data: Boolean;
      IdSignature: String = ''): String;
      virtual;
    function AdicionarSignatureElement( ConteudoXML: String; AddX509Data: Boolean;
      docElement, IdSignature: String): String;
    function AjustarXMLAssinado(const ConteudoXML: String; X509DER: String = ''): String;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''): String; virtual;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String; const MimeType: String = ''): String; virtual;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; virtual;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''): Boolean; virtual;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assinar: Boolean =  False): AnsiString; virtual;

    procedure CarregarCertificado; virtual;
    procedure DescarregarCertificado; virtual;
    function SelecionarCertificado: String; virtual;

    property CertificadoLido: Boolean read FpCertificadoLido;

    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertSubjectName: String read GetCertSubjectName;
    property CertRazaoSocial: String read GetCertRazaoSocial;
    property CertCNPJ: String read GetCertCNPJ;
    property CertTipo: TSSLTipoCertificado read GetCertTipo;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;
  end;

  TSSLLib = (libNone, libOpenSSL, libCapicom, libCapicomDelphiSoap);

  { TDFeSSL }

  TDFeSSL = class(TPersistent)
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
    FUseCertificateHTTP: Boolean;

    function GetCertCNPJ: String;
    function GetCertDataVenc: TDateTime;
    function GetCertificadoLido: Boolean;
    function GetCertNumeroSerie: String;
    function GetCertRazaoSocial: String;
    function GetCertSubjectName: String;
    function GetHTTPResultCode: Integer;
    function GetInternalErrorCode: Integer;
    function GetSenha: AnsiString;

    procedure SetArquivoPFX(AValue: String);
    procedure SetDadosPFX(AValue: AnsiString);
    procedure SetNumeroSerie(AValue: String);
    procedure SetSenha(AValue: AnsiString);

    procedure SetSSLLib(ASSLLib: TSSLLib);
    function GetCertTipo: TSSLTipoCertificado;

  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;

    // Nota: ConteudoXML, DEVE estar em UTF8 //
    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''): String;
    // Envia por SoapAction o ConteudoXML (em UTF8) para URL. Retorna a resposta do Servico //
    function Enviar(var ConteudoXML: String; const URL: String;
      const SoapAction: String; const MimeType: String = ''): String;
    // Valida um Arquivo contra o seu Schema. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    function Validar(const ConteudoXML: String; ArqSchema: String;
      out MsgErro: String): Boolean;
    // Verifica se assinatura de um XML é válida. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''): Boolean;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assinar: Boolean =  False): AnsiString; overload;
    function CalcHashArquivo( const NomeArquivo : String;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assinar: Boolean =  False): AnsiString; overload;
    function CalcHash( const BinaryString : AnsiString;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assinar: Boolean =  False): AnsiString; overload;
    function CalcHash( const AStringList : TStringList;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assinar: Boolean =  False): AnsiString; overload;

    procedure CarregarCertificado;
    procedure DescarregarCertificado;
    function SelecionarCertificado: String;

    procedure ValidarCNPJCertificado(CNPJDocumento: String);

    property CertificadoLido: Boolean read GetCertificadoLido;

    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertSubjectName: String read GetCertSubjectName;
    property CertRazaoSocial: String read GetCertRazaoSocial;
    property CertCNPJ: String read GetCertCNPJ;
    property CertTipo: TSSLTipoCertificado read GetCertTipo;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;

  published
    property SSLLib: TSSLLib read FSSLLib write SetSSLLib;
    property SSLClass: TDFeSSLClass read FSSLClass;

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

    property UseCertificateHTTP: Boolean read FUseCertificateHTTP write FUseCertificateHTTP;
  end;


implementation

uses strutils,
  synacode,
  ACBrDFeUtil, ACBrValidador, ACBrUtil, ACBrDFeException
  {$IFNDEF DFE_SEM_OPENSSL}
   ,ACBrDFeOpenSSL
  {$ENDIF}
  {$IFNDEF DFE_SEM_CAPICOM}
   ,ACBrDFeCapicom
   {$IFNDEF FPC}
    ,ACBrDFeCapicomDelphiSoap
   {$ENDIF}
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
  FK           := '';
  FSSLLib      := libNone;
  FTimeOut     := 5000;
  FNameSpaceURI:= '';

  // Para emissão de NFS-e essas propriedades podem ter valores diferentes dos
  // atribuidos abaixo, dependendo do provedor...
  FUseCertificateHTTP := True;

  if Assigned(FSSLClass) then
    FSSLClass.Free;

  FSSLClass := TDFeSSLClass.Create(Self);
end;

destructor TDFeSSL.Destroy;
begin
  if Assigned(FSSLClass) then
  begin
    DescarregarCertificado;
    FreeAndNil(FSSLClass);
  end;

  inherited Destroy;
end;

function TDFeSSL.Assinar(const ConteudoXML, docElement, infElement: String;
  SignatureNode: String; SelectionNamespaces: String; IdSignature: String
  ): String;
Var
  XmlAss, DeclaracaoXMLAntes, DeclaracaoXMLDepois: String;
  I: integer;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  // Lendo Header antes de assinar, Se Header não for UTF8 não usa... //
  if XmlEhUTF8(ConteudoXML) then
    DeclaracaoXMLAntes := ObtemDeclaracaoXML(ConteudoXML)
  else
    DeclaracaoXMLAntes := '';

  XmlAss := FSSLClass.Assinar( ConteudoXML, docElement, infElement,
                               SignatureNode, SelectionNamespaces, IdSignature);

  // Verificando se modificou o Header do XML assinado, e voltando para o anterior //
  if DeclaracaoXMLAntes <> '' then
  begin
    DeclaracaoXMLDepois := ObtemDeclaracaoXML(XmlAss);

    if DeclaracaoXMLAntes <> DeclaracaoXMLDepois then
      XmlAss := StringReplace(XmlAss, DeclaracaoXMLAntes, DeclaracaoXMLDepois, []);
  end;

  Result := XmlAss;
end;

function TDFeSSL.Enviar(var ConteudoXML: String; const URL: String;
  const SoapAction: String; const MimeType: String): String;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  Result := FSSLClass.Enviar(ConteudoXML, URL, SoapAction, MimeType);
end;

function TDFeSSL.Validar(const ConteudoXML: String; ArqSchema: String;
  out MsgErro: String): Boolean;
begin
  if EstaVazio(ArqSchema) then
    raise EACBrDFeException.Create('Arquivo de Schema não especificado');

  // ArqSchema deve vir com o Path Completo
  if not FileExists(ArqSchema) then
    raise EACBrDFeException.Create('Arquivo ' + sLineBreak + ArqSchema +
      sLineBreak + 'Não encontrado');

  Result := FSSLClass.Validar(ConteudoXML, ArqSchema, MsgErro);
end;

function TDFeSSL.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String): Boolean;
begin
  Result := FSSLClass.VerificarAssinatura(ConteudoXML, MsgErro,
                              infElement, SignatureNode, SelectionNamespaces);
end;

function TDFeSSL.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const ModoSaida: TSSLHashOutput; const Assinar: Boolean): AnsiString;
var
  ABinStr: AnsiString;
begin
  if not Assigned(AStream) then
    raise EACBrDFeException.CreateDef('Stream inválido');

  if AStream.Size <= 0 then
    raise EACBrDFeException.CreateDef('Stream vazio');

  ABinStr := FSSLClass.CalcHash(AStream, Digest, Assinar);

  case ModoSaida of
    outBase64 : Result := Trim(EncodeBase64( ABinStr ));
    outHexa   : Result := AsciiToHex( ABinStr ); // TESTAR
  else
    Result := ABinStr;
  end;
end;

function TDFeSSL.CalcHashArquivo(const NomeArquivo: String;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput;
  const Assinar: Boolean): AnsiString;
Var
   FS : TFileStream ;
begin
  FS := TFileStream.Create(NomeArquivo, fmOpenRead or fmShareDenyWrite);
  try
    Result := CalcHash( FS, Digest, ModoSaida, Assinar );
  finally
    FS.Free ;
  end ;
end;

function TDFeSSL.CalcHash(const BinaryString: AnsiString;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput;
  const Assinar: Boolean): AnsiString;
Var
   MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write( Pointer(BinaryString)^, Length(BinaryString) );
    Result := CalcHash( MS, Digest, ModoSaida, Assinar );
  finally
    MS.Free ;
  end ;
end;

function TDFeSSL.CalcHash(const AStringList: TStringList;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput;
  const Assinar: Boolean): AnsiString;
Var
  MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    AStringList.SaveToStream( MS );
    Result := CalcHash( MS, Digest, ModoSaida, Assinar );
  finally
    MS.Free ;
  end ;
end;

procedure TDFeSSL.CarregarCertificado;
begin
  FSSLClass.CarregarCertificado;
end;

procedure TDFeSSL.DescarregarCertificado;
begin
  FSSLClass.DescarregarCertificado;
end;

function TDFeSSL.SelecionarCertificado: String;
begin
  Result := FSSLClass.SelecionarCertificado;

  if NaoEstaVazio(Result) then
    FSSLClass.CarregarCertificado;
end;

{ Verifica se o "CNPJDocumento", é da mesma raiz do CNPJ do Certificado }
procedure TDFeSSL.ValidarCNPJCertificado(CNPJDocumento: String);
var
  ErroCNPJ, CNPJCertificado: String;
begin
  CNPJDocumento := OnlyNumber(CNPJDocumento);
  if (CNPJDocumento = '') or              // Informou vazio
     (Length(CNPJDocumento) <> 14) then   // Não é CNPJ
    exit;

  CNPJCertificado := OnlyNumber(CertCNPJ);  // Lendo CNPJ do Certificado...
  if (CNPJCertificado = '') or              // Não foi capaz de ler CNPJ do Certificado (Senha, NumSerie, Path... há algo errado na configuração)
     (Length(CNPJCertificado) <> 14) then   // Não é CNPJ (estranho.. pode ser um eCPF)
    exit;

  ErroCNPJ := ValidarCNPJ(CNPJDocumento);
  if (ErroCNPJ <> '') then
    raise EACBrDFeException.CreateDef('Erro CNPJ Documento: '+ErroCNPJ);

  { Deve verificar somente os 8 primeiros digitos, para evitar problemas quando
    a filial estiver utilizando o certificado da matriz }
  if (Copy(CNPJDocumento, 1, 8) <> Copy(CNPJCertificado, 1, 8)) then
    raise EACBrDFeException.Create('O CNPJ do Documento é diferente do CNPJ do Certificado Digital' );
end;

function TDFeSSL.GetCertDataVenc: TDateTime;
begin
  Result := FSSLClass.CertDataVenc;
end;

function TDFeSSL.GetCertificadoLido: Boolean;
begin
  Result := FSSLClass.CertificadoLido;
end;

function TDFeSSL.GetCertCNPJ: String;
begin
  Result := FSSLClass.CertCNPJ;
end;

function TDFeSSL.GetCertNumeroSerie: String;
begin
  Result := FSSLClass.CertNumeroSerie;
end;

function TDFeSSL.GetCertRazaoSocial: String;
begin
  Result := FSSLClass.CertRazaoSocial;
end;

function TDFeSSL.GetCertSubjectName: String;
begin
  Result := FSSLClass.CertSubjectName;
end;

function TDFeSSL.GetCertTipo: TSSLTipoCertificado;
begin
  Result := FSSLClass.GetCertTipo;
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
  FNumeroSerie := Trim(UpperCase(StringReplace(AValue, ' ', '', [rfReplaceAll])));
  DescarregarCertificado;
end;

procedure TDFeSSL.SetSenha(AValue: AnsiString);
begin
  if (FK <> '') and (FSenha = StrCrypt(AValue, FK)) then
    Exit;

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

  {$IFNDEF DFE_SEM_CAPICOM}
  case ASSLLib of
    libCapicom:
      FSSLClass := TDFeCapicom.Create(Self);

    libOpenSSL:
    begin
      {$IFNDEF DFE_SEM_OPENSSL}
       FSSLClass := TDFeOpenSSL.Create(Self);
      {$ELSE}
       raise EACBrDFeException.Create('Suporte a libOpenSSL foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL}');
      {$ENDIF}
    end;

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
    libOpenSSL, libCapicom, libCapicomDelphiSoap:
      FSSLClass := TDFeOpenSSL.Create(Self);
  else
    FSSLClass := TDFeSSLClass.Create(Self);
  end;
  {$ENDIF}

  FSSLLib := ASSLLib;
end;

{ TDFeSSLClass }

constructor TDFeSSLClass.Create(ADFeSSL: TDFeSSL);
begin
  FpDFeSSL := ADFeSSL;
  FpCertificadoLido := False;
end;

function TDFeSSLClass.Assinar(const ConteudoXML, docElement,
  infElement: String; SignatureNode: String; SelectionNamespaces: String;
  IdSignature: String): String;
begin
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
  raise EACBrDFeException.Create(ClassName + '.Assinar, não implementado');
end;

function TDFeSSLClass.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String; const MimeType: String): String;
begin
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
  raise EACBrDFeException.Create(ClassName + '.Enviar não implementado');
end;

function TDFeSSLClass.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
begin
  {$IFDEF FPC}
  Result := False;
  {$ENDIF}
  raise EACBrDFeException.Create('"Validar" não suportado em: ' + ClassName);
end;

function TDFeSSLClass.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String): Boolean;
begin
  {$IFDEF FPC}
  Result := False;
  {$ENDIF}
  raise EACBrDFeException.Create('"ValidarAssinatura" não suportado em: ' + ClassName);
end;

function TDFeSSLClass.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assinar: Boolean): AnsiString;
begin
  Result := '';
  raise EACBrDFeException.Create('"CalcHash" não suportado em: ' + ClassName);
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

function TDFeSSLClass.GetCertRazaoSocial: String;
begin
  Result := '';
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

function TDFeSSLClass.GetCertTipo: TSSLTipoCertificado;
begin
  Result := tpcA1;
end;

function TDFeSSLClass.GetCertCNPJ: String;
begin
  Result := '';
end;

function TDFeSSLClass.GetCNPJFromSubjectName( SubjectName: String ): String;
var
  P: Integer;
begin
  Result := '';
  P := pos('CN=',SubjectName);
  if P > 0 then
  begin
    P := PosEx(':', SubjectName, P);
    if P > 0 then
    begin
      Result := OnlyNumber(copy(SubjectName, P+1, 14));
    end;
  end;
end;

function TDFeSSLClass.GetRazaoSocialFromSubjectName( SubjectName: String ): String;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := pos('CN=',SubjectName);
  if P1 > 0 then
  begin
    P2 := PosEx(':', SubjectName, P1);
    if P2 < 0 then
      P2 := PosEx(',', SubjectName, P1);
    if P2 < 0 then
      P2 := Length(SubjectName);

    Result := copy(SubjectName, P1+3, P2-P1-3);
  end;
end;

function TDFeSSLClass.SignatureElement(const URI: String; AddX509Data: Boolean;
  IdSignature: String): String;
begin
  {(*}
  Result :=
  '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"' + IdSignature + '>' +
    '<SignedInfo>' +
      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
      '<Reference URI="' + IfThen(URI = '', '', '#' + URI) + '">' +
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

function TDFeSSLClass.AdicionarSignatureElement(ConteudoXML: String;
  AddX509Data: Boolean; docElement, IdSignature: String): String;
var
  URI, TagEndDocElement: String;
  I: Integer;
begin
  URI := ExtraiURI(ConteudoXML);

  TagEndDocElement := '</' + docElement + '>';
  I := PosLast(TagEndDocElement, ConteudoXML);
  if I = 0 then
    raise EACBrDFeException.Create('Não encontrei final do elemento: ' + TagEndDocElement);

  Result := copy(ConteudoXML, 1, I - 1) +
            SignatureElement(URI, AddX509Data, IdSignature) + TagEndDocElement;
end;

function TDFeSSLClass.AjustarXMLAssinado(const ConteudoXML: String;
  X509DER: String): String;
var
  XmlAss: String;
  PosIni, PosFim: Integer;

  function RemoveEspacos( const AXML, TagIni, TagFim : String): String;
  begin
    Result := '';
    PosIni := PosLast(TagIni, AXML);
    if PosIni > 0 then
    begin
      PosFim := PosEx(TagFim, AXML, PosIni + 1);
      if PosFim > 0 then
        Result := copy(AXML, 1, PosIni - 1) +
                  StringReplace(copy(AXML, PosIni, PosFim-PosIni), ' ', '', [rfReplaceAll])+
                  copy(AXML, PosFim, Length(AXML));
    end;

    if Result = '' then
      Result := AXML;
  end;
begin
  XmlAss := ConteudoXML;

  // Removendo Declaração sem UTF8 Ex: <?xml version="1.0"?> //
  if not XmlEhUTF8(ObtemDeclaracaoXML(XmlAss)) then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  // Removendo quebras de linha //
  XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
  XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

  PosIni := PosLast('<SignatureValue>', XmlAss);
  if X509DER = '' then
  begin
    // Considerando apenas o último Certificado X509, da assinatura //
    PosIni := PosEx('<X509Certificate>', XmlAss, PosIni) - 1;
    PosFim := PosLast('<X509Certificate>', XmlAss);
    XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));
  end
  else
  begin
    // Remove todos Certificados adiconados, e Adiciona o X509DER informado //
    PosIni := PosEx('<X509Certificate>', XmlAss, PosIni) + Length('<X509Certificate>') - 1;
    PosFim := PosLast('</X509Certificate>', XmlAss);
    XmlAss := copy(XmlAss, 1, PosIni) + X509DER + copy(XmlAss, PosFim, length(XmlAss));
  end;

  // CAPICOM insere espaços em alguns Elementos da Assinatura //
  XmlAss := RemoveEspacos(XmlAss, '<SignatureValue>', '</Signature>');

  Result := XmlAss;
end;

end.

