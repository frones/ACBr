{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Jean Patrick Figueiredo dos Santos             }
{         Silvio Clécio - xmailer - https://github.com/silvioprog/xmailer      }
{         Projeto PHPMailer - https://github.com/Synchro/PHPMailer             }
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

unit ACBrMail;

{$I ACBr.inc}

interface

uses
  Classes, syncobjs, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  SSL_OpenSSL, SMTPSend, MimePart, MimeMess, SynaChar, SynaUtil,
  ACBrBase,
  blcksock;

type

  TMailStatus = (pmsStartProcess, pmsConfigHeaders, pmsAddingMimeParts,
                 pmsLoginSMTP, pmsStartSends, pmsSendTo, pmsSendCC, pmsSendBCC,
                 pmsSendReplyTo, pmsSendData, pmsLogoutSMTP, pmsDone, pmsError);

  TMailCharset = TMimeChar;

  TMailAttachmentDisposition = (adAttachment, adInline);

  { TMailAttachment }

  TMailAttachment = class
  private
    FFileName: String;
    FDescription: String;
    FStream: TMemoryStream;
    FDisposition: TMailAttachmentDisposition;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Assign(Source: TMailAttachment);

    property FileName: String read FFileName write FFileName;
    property Stream: TMemoryStream read FStream;
    property Description: String read FDescription write FDescription;

    property Disposition: TMailAttachmentDisposition read FDisposition
      write FDisposition;
  end;

  { TMailAttachments }

  TMailAttachments = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TMailAttachment>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TMailAttachment);
    function GetObject (Index: Integer): TMailAttachment;
    procedure Insert (Index: Integer; Obj: TMailAttachment);
  public
    function New: TMailAttachment;
    function Add (Obj: TMailAttachment): Integer;
    property Objects [Index: Integer]: TMailAttachment read GetObject write SetObject; default;
  end;

  TACBrMail = class;

  TACBrOnMailProcess = procedure(const AMail: TACBrMail; const aStatus: TMailStatus) of object;
  TACBrOnMailException = procedure(const AMail: TACBrMail; const E: Exception; var ThrowIt: Boolean) of object;

  { TACBrMailThread }

  TACBrMailThread = class(TThread)
  private
    FACBrMail : TACBrMail;
    FException: Exception;
    FThrowIt: Boolean;
    FStatus: TMailStatus;
    FOnMailProcess: TACBrOnMailProcess;
    FOnMailException: TACBrOnMailException;
    FOnBeforeMailProcess: TNotifyEvent;
    FOnAfterMailProcess: TNotifyEvent;

    procedure MailException(const AMail: TACBrMail; const E: Exception; var ThrowIt: Boolean);
    procedure DoMailException;
    procedure MailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
    procedure DoMailProcess;
    procedure BeforeMailProcess(Sender: TObject);
    procedure DoBeforeMailProcess;
    procedure AfterMailProcess(Sender: TObject);
    procedure DoAfterMailProcess;

  protected
    procedure Execute; override;

  public
    constructor Create(AOwner : TACBrMail);
  end;

  { TACBrMail }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrMail = class(TACBrComponent)
  private
    fSMTP                : TSMTPSend;
    fMIMEMess            : TMimeMess;
    fArqMIMe             : TMemoryStream;

    fReadingConfirmation : boolean;
    fDeliveryConfirmation: boolean;
    fOnMailProcess       : TACBrOnMailProcess;
    fOnMailException     : TACBrOnMailException;

    fIsHTML              : boolean;
    fAttempts            : Byte;
    fFrom                : string;
    fFromName            : string;
    fSubject             : string;
    fBody                : TStringList;
    fAltBody             : TStringList;
    fAttachments         : TMailAttachments;
    fReplyTo             : TStringList;
    fBCC                 : TStringList;
    fTimeOut             : Integer;
    fUseThread           : boolean;

    fDefaultCharsetCode  : TMimeChar;
    fIDECharsetCode      : TMimeChar;

    fOnAfterMailProcess  : TNotifyEvent;
    fOnBeforeMailProcess : TNotifyEvent;

    fGetLastSmtpError    : String;

    function GetHost: string;
    function GetPort: string;
    function GetUsername: string;
    function GetPassword: string;
    function GetFullSSL: Boolean;
    function GetSSLType: TSSLType;
    function GetAutoTLS: Boolean;
    function GetPriority: TMessPriority;

    procedure SetHost(const aValue: string);
    procedure SetPort(const aValue: string);
    procedure SetUsername(const aValue: string);
    procedure SetPassword(const aValue: string);
    procedure SetFullSSL(aValue: Boolean);
    procedure SetSSLType(aValue: TSSLType);
    procedure SetAutoTLS(aValue: Boolean);
    procedure SetPriority(aValue: TMessPriority);
    procedure SetAttempts(AValue: Byte);

    procedure SmtpError(const pMsgError: string);

    procedure DoException(E: Exception);
    procedure AddEmailWithDelimitersToList( aEmail: String; aList: TStrings);

  protected
    procedure SendMail;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure MailProcess(const aStatus: TMailStatus);
    procedure Send(UseThreadNow: Boolean); overload;
    procedure Send; overload;
    procedure BuildMimeMess;
    procedure Clear;
    procedure SaveToFile(const AFileName: String);
    function SaveToStream(AStream: TStream): Boolean;

    procedure AddAttachment(const aFileName: string; aDescription: string;
      const aDisposition: TMailAttachmentDisposition = adInline); overload;
    procedure AddAttachment(const aFileName: string); overload;
    procedure AddAttachment(aStream: TStream; aDescription: string;
      const aDisposition: TMailAttachmentDisposition = adInline); overload;
    procedure AddAttachment(aStream: TStream); overload;
    procedure ClearAttachments;

    procedure AddAddress(const aEmail: string; const aName: string = '');
    procedure AddReplyTo(const aEmail: string; const aName: string = '');
    procedure AddCC(const aEmail: string; const aName: string = '');
    procedure AddBCC(const aEmail: string);

    property SMTP: TSMTPSend read fSMTP;
    property MIMEMess: TMimeMess read fMIMEMess;
    property Attachments: TMailAttachments read fAttachments;
    property BCC: TStringList read fBCC;
    property ReplyTo: TStringList read fReplyTo;

    property AltBody: TStringList read fAltBody;
    property Body: TStringList read fBody;

    property GetLastSmtpError: string read fGetLastSmtpError;

  published
    property Host: string read GetHost write SetHost;
    property Port: string read GetPort write SetPort;
    property Username: string read GetUsername write SetUsername;
    property Password: string read GetPassword write SetPassword;
    property SetSSL: boolean read GetFullSSL write SetFullSSL;
    property SSLType : TSSLType read GetSSLType write SetSSLType default LT_all;
    property SetTLS: boolean read GetAutoTLS write SetAutoTLS;
    property Priority: TMessPriority read GetPriority write SetPriority default MP_normal;
    property ReadingConfirmation: boolean read fReadingConfirmation write fReadingConfirmation default False;
    property DeliveryConfirmation: boolean read fDeliveryConfirmation write fDeliveryConfirmation default False;
    property IsHTML: boolean read fIsHTML write fIsHTML default False;
    property UseThread: boolean read fUseThread write fUseThread default False;
    property TimeOut: Integer read fTimeOut write fTimeOut default 0;
    property Attempts: Byte read fAttempts write SetAttempts;
    property From: string read fFrom write fFrom;
    property FromName: string read fFromName write fFromName;
    property Subject: string read fSubject write fSubject;
    property DefaultCharset: TMailCharset read fDefaultCharsetCode write fDefaultCharsetCode;
    property IDECharset: TMailCharset read fIDECharsetCode write fIDECharsetCode;
    property OnBeforeMailProcess: TNotifyEvent read fOnBeforeMailProcess write fOnBeforeMailProcess;
    property OnMailProcess: TACBrOnMailProcess read fOnMailProcess write fOnMailProcess;
    property OnAfterMailProcess: TNotifyEvent read fOnAfterMailProcess write fOnAfterMailProcess;
    property OnMailException: TACBrOnMailException read fOnMailException write fOnMailException;
  end;

procedure SendEmailByThread( MailToClone: TACBrMail);

var
  MailCriticalSection : TCriticalSection;

implementation

Uses
  strutils, math,
  ACBrUtil.Strings;

procedure SendEmailByThread(MailToClone: TACBrMail);
var
  AMail: TACBrMail;
begin
  if not Assigned(MailToClone) then
    raise Exception.Create( 'MailToClone not specified' );

  AMail := TACBrMail.Create(nil);
  AMail.Assign( MailToClone );

  // Thread is FreeOnTerminate, and also will destroy "AMail"
  TACBrMailThread.Create(AMail);
end;

{ TMailAttachment }

constructor TMailAttachment.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FDisposition := adInline;
  Clear;
end;

destructor TMailAttachment.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

procedure TMailAttachment.Clear;
begin
  FFileName := '';
  FDescription  := '';
  FStream.Clear;
end;

procedure TMailAttachment.Assign(Source: TMailAttachment);
begin
  Clear;
  FFileName := Source.FileName;
  FDescription  := Source.Description;
  Source.Stream.Position := 0;
  FStream.CopyFrom(Source.Stream, Stream.Size);
  FDisposition := Source.Disposition;
end;

{ TMailAttachments }

procedure TMailAttachments.SetObject(Index: Integer; Item: TMailAttachment);
begin
   inherited Items[Index] := Item;
end;

function TMailAttachments.GetObject(Index: Integer): TMailAttachment;
begin
  Result := TMailAttachment(inherited Items[Index]);
end;

procedure TMailAttachments.Insert(Index: Integer; Obj: TMailAttachment);
begin
  inherited Insert(Index, Obj);
end;

function TMailAttachments.New: TMailAttachment;
begin
  Result := TMailAttachment.Create;
  Add(Result);
end;

function TMailAttachments.Add(Obj: TMailAttachment): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TACBrMail }

function TACBrMail.GetHost: string;
begin
  Result := fSMTP.TargetHost;
end;

function TACBrMail.GetPort: string;
begin
  Result := fSMTP.TargetPort;
end;

function TACBrMail.GetUsername: string;
begin
  Result := fSMTP.UserName;
end;

function TACBrMail.GetPassword: string;
begin
  Result := fSMTP.Password;
end;

function TACBrMail.GetFullSSL: Boolean;
begin
  Result := fSMTP.FullSSL;
end;

function TACBrMail.GetSSLType: TSSLType;
begin
  Result := fSMTP.Sock.SSL.SSLType;
end;

function TACBrMail.GetAutoTLS: Boolean;
begin
  Result := fSMTP.AutoTLS;
end;

procedure TACBrMail.SetHost(const aValue: string);
begin
  fSMTP.TargetHost := aValue;
end;

procedure TACBrMail.SetPort(const aValue: string);
begin
  fSMTP.TargetPort := aValue;
end;

procedure TACBrMail.SetUsername(const aValue: string);
begin
  fSMTP.UserName := aValue;
end;

procedure TACBrMail.SetPassword(const aValue: string);
begin
  fSMTP.Password := aValue;
end;

procedure TACBrMail.SetFullSSL(aValue: Boolean);
begin
  fSMTP.FullSSL := aValue;
end;

procedure TACBrMail.SetSSLType(aValue: TSSLType);
begin
  fSMTP.Sock.SSL.SSLType := aValue;
end;

procedure TACBrMail.SetAutoTLS(aValue: Boolean);
begin
  fSMTP.AutoTLS := aValue;
end;

function TACBrMail.GetPriority: TMessPriority;
begin
  Result := fMIMEMess.Header.Priority;
end;

procedure TACBrMail.SetPriority(aValue: TMessPriority);
begin
  fMIMEMess.Header.Priority := aValue;
end;

procedure TACBrMail.SetAttempts(AValue: Byte);
begin
  if fAttempts = AValue then Exit;
  fAttempts := Max(AValue, 1);
end;

procedure TACBrMail.SmtpError(const pMsgError: string);
begin
  try
    fGetLastSmtpError := pMsgError;
    MailProcess(pmsError);
    DoException( Exception.Create(pMsgError) );
  finally
    Clear;
  end;
end;

procedure TACBrMail.DoException(E: Exception);
Var
  ThrowIt: Boolean;
begin
  if Assigned(fOnMailException) then
  begin
    ThrowIt := True;
    fOnMailException( Self, E, ThrowIt );

    if ThrowIt then
      raise E
    else
    begin
      E.Free;
      Abort;
    end;
  end
  else
    raise E;
end;

procedure TACBrMail.AddEmailWithDelimitersToList(aEmail: String; aList: TStrings
  );
var
  sDelimiter: Char;
begin
  aEmail := Trim(aEmail);
  sDelimiter := FindDelimiterInText(aEmail);

  if (sDelimiter = ' ') then
    aList.Add(aEmail)
  else
    AddDelimitedTextToList(aEmail, sDelimiter, aList);
end;

procedure TACBrMail.Clear;
begin
  ClearAttachments;
  fMIMEMess.Header.Clear;
  fMIMEMess.Clear;
  fReplyTo.Clear;
  fBCC.Clear;
  fSubject := '';
  fBody.Clear;
  fAltBody.Clear;
end;

procedure TACBrMail.SaveToFile(const AFileName: String);
begin
  BuildMimeMess;
  
  if AFileName <> '' then
    fArqMIMe.SaveToFile(AFileName);

  Clear;
end;

procedure TACBrMail.MailProcess(const aStatus: TMailStatus);
begin
  if Assigned(fOnMailProcess) then
    fOnMailProcess(Self, aStatus);
end;

constructor TACBrMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSMTP := TSMTPSend.Create;
  fMIMEMess := TMimeMess.Create;
  fAltBody := CreateStringList;
  fBody := CreateStringList;
  fArqMIMe := TMemoryStream.Create;
  fAttachments := TMailAttachments.Create(True); // FreeObjects
  fTimeOut := 0;

  fOnBeforeMailProcess := nil;
  fOnAfterMailProcess := nil;

  fAttachments.Clear;
  SetPriority(MP_normal);
  fDefaultCharsetCode := UTF_8;
  fIDECharsetCode := {$IfDef USE_UTF8}UTF_8{$Else}{$IfDef MSWINDOWS}CP1252{$Else}UTF_8{$EndIf}{$EndIf};
  fReadingConfirmation := False;
  fDeliveryConfirmation := False;
  fIsHTML := False;
  fUseThread := False;
  fAttempts := 3;
  fFrom := '';
  fFromName := '';
  fSubject := '';

  fReplyTo := CreateStringList;
  {$IfDef HAS_STRICTDELIMITER}
  fReplyTo.StrictDelimiter := True;
  {$EndIf}
  fReplyTo.Delimiter := ';';

  fBCC := CreateStringList;
  {$IfDef HAS_STRICTDELIMITER}
  fBCC.StrictDelimiter := True;
  {$EndIf}
  fBCC.Delimiter := ';';

  //definir como conexão padrão TLS1.2;
  //mantido LT_ALL por compatibilidade com os diversos provedores que por hora não suportam
  fSMTP.Sock.SSL.SSLType := LT_all;

  // NOTAR ISSO: fSMTP.Sock.OnStatus := ;

end;

destructor TACBrMail.Destroy;
begin
  ClearAttachments;
  fAltBody.Free;
  fBody.Free;
  fBCC.Free;
  fReplyTo.Free;
  fMIMEMess.Free;
  fSMTP.Free;
  fArqMIMe.Free;
  fAttachments.Free;
  
  inherited Destroy;
end;

procedure TACBrMail.Assign(Source: TPersistent);
var
  i: Integer;
  AAttachment: TMailAttachment;
begin
  if not (Source is TACBrMail) then
    raise Exception.Create('Source must be TACBrMail');

  with TACBrMail(Source) do
  begin
    Self.Host := Host;
    Self.Port := Port;
    Self.Username := Username;
    Self.Password := Password;
    Self.SetSSL := SetSSL;
    Self.SetTLS := SetTLS;
    Self.Priority := Priority;
    Self.ReadingConfirmation := ReadingConfirmation;
    Self.IsHTML := IsHTML;
    Self.UseThread := UseThread;
    Self.Attempts := Attempts;
    Self.From := From;
    Self.FromName := FromName;
    Self.Subject := Subject;
    Self.DefaultCharset := DefaultCharset;
    Self.IDECharset := IDECharset;
    Self.OnBeforeMailProcess := OnBeforeMailProcess;
    Self.OnMailProcess := OnMailProcess;
    Self.OnAfterMailProcess := OnAfterMailProcess;
    Self.OnMailException := OnMailException;
    Self.Tag := Tag;

    for i := 0 to Attachments.Count-1 do
    begin
      AAttachment := Self.Attachments.New;
      AAttachment.Assign(Attachments[I]);
    end;

    Self.AltBody.Assign(AltBody);
    Self.Body.Assign(Body);
    Self.ReplyTo.Assign(ReplyTo);
    Self.BCC.Assign(BCC);

    Self.MIMEMess.Header.ToList.Assign( MIMEMess.Header.ToList );
    Self.MIMEMess.Header.CCList.Assign( MIMEMess.Header.CCList );
    Self.MIMEMess.Header.Organization := MIMEMess.Header.Organization;
    Self.MIMEMess.Header.CustomHeaders.Assign( MIMEMess.Header.CustomHeaders );
    Self.MIMEMess.Header.Date := MIMEMess.Header.Date;
    Self.MIMEMess.Header.XMailer := MIMEMess.Header.XMailer;
  end;
end;

procedure TACBrMail.Send(UseThreadNow: Boolean);
begin
  if UseThreadNow then
    SendEmailByThread(Self)
  else
    SendMail;
end;

procedure TACBrMail.Send;
begin
  Send( UseThread );
end;

procedure TACBrMail.BuildMimeMess;
var
  i: Integer;
  MultiPartParent, MimePartAttach : TMimePart;
  NeedMultiPartRelated, BodyHasImage: Boolean;
  AAttachment: TMailAttachment;
begin
  if Assigned(OnBeforeMailProcess) then
    OnBeforeMailProcess( self );

  MailProcess(pmsStartProcess);

  // Configuring the Headers //
  MailProcess(pmsConfigHeaders);

  fMIMEMess.Header.CharsetCode := fDefaultCharsetCode;
  fMIMEMess.Header.TargetCharset := fIDECharsetCode;
  fMIMEMess.Header.Subject := fSubject;

  if Trim(fFromName) <> '' then
    fMIMEMess.Header.From := '"' + fFromName + '" <' + From + '>'
  else
    fMIMEMess.Header.From := fFrom;

  if fReplyTo.Count > 0 then
    fMIMEMess.Header.ReplyTo := fReplyTo.DelimitedText;

  if fReadingConfirmation then
    fMIMEMess.Header.CustomHeaders.Insert(0, 'Disposition-Notification-To: ' + fMIMEMess.Header.From);

  if fDeliveryConfirmation then
    fMIMEMess.Header.CustomHeaders.Insert(0, 'Return-Receipt-To: ' + fMIMEMess.Header.From);

  fMIMEMess.Header.XMailer := 'Synapse - ACBrMail';

  // Adding MimeParts //
  // Inspiration: http://www.ararat.cz/synapse/doku.php/public:howto:mimeparts
  MailProcess(pmsAddingMimeParts);

  // Encoding according to IDE and Mail Charset //
  NeedMultiPartRelated := fIsHTML and (fBody.Count > 0) and (fAltBody.Count > 0);

  // The Root //
  MultiPartParent := fMIMEMess.AddPartMultipart( IfThen(NeedMultiPartRelated, 'alternative', 'mixed'), nil );
  MultiPartParent.CharsetCode := fDefaultCharsetCode;
  MultiPartParent.TargetCharset := fIDECharsetCode;
  MultiPartParent.ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);

  // Text part //
  if (fAltBody.Count > 0) then
  begin
    with fMIMEMess.AddPart( MultiPartParent ) do
    begin
      fAltBody.SaveToStream(DecodedLines);
      Primary := 'text';
      Secondary := 'plain';
      Description := 'Message text';
      Disposition := 'inline';
      CharsetCode := fDefaultCharsetCode;
      TargetCharset := fIDECharsetCode;
      ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
      EncodingCode := ME_BASE64;
      EncodePart;
      EncodePartHeader;
    end;
  end;

  // Need New branch ? //
  if NeedMultiPartRelated then
  begin
    MultiPartParent := fMIMEMess.AddPartMultipart( 'related', MultiPartParent );
    MultiPartParent.CharsetCode := fDefaultCharsetCode;
    MultiPartParent.TargetCharset := fIDECharsetCode;
    MultiPartParent.ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
  end;

  if fIsHTML and (fBody.Count > 0) then
  begin
    // Adding HTML Part //
    with fMIMEMess.AddPart( MultiPartParent ) do
    begin
      fBody.SaveToStream(DecodedLines);
      Primary := 'text';
      Secondary := 'html';
      Description := 'HTML text';
      Disposition := 'inline';
      CharsetCode := fDefaultCharsetCode;
      TargetCharset := fIDECharsetCode;
      ConvertCharset := (fDefaultCharsetCode <> fIDECharsetCode);
      EncodingCode := ME_BASE64;
      EncodePart;
      EncodePartHeader;
    end;
  end;

  // Adding the Attachments //
  for i := 0 to fAttachments.Count-1 do
  begin
    AAttachment := fAttachments[i];
    BodyHasImage := pos(':'+LowerCase(AAttachment.Description), LowerCase(fBody.Text)) > 0;

    AAttachment.Stream.Position := 0;
    MimePartAttach := fMIMEMess.AddPart(MultiPartParent);
    MimePartAttach.DecodedLines.LoadFromStream(AAttachment.Stream);
    MimePartAttach.Description := AAttachment.Description;
    case AAttachment.Disposition of
      adInline: MimePartAttach.Disposition := 'inline';
    else
      MimePartAttach.Disposition := 'attachment';
    end;
    if fIsHTML and BodyHasImage then
      MimePartAttach.ContentID := AAttachment.Description;

    MimePartAttach.FileName    := ExtractFileName(AAttachment.FileName);
    MimePartAttach.EncodingCode:= ME_BASE64;
    MimePartAttach.PrimaryCode := MP_BINARY;  // To avoid MP_TEXT internal conversion ;
    MimePartAttach.CharsetCode := fIDECharsetCode;
    MimePartAttach.TargetCharset := fIDECharsetCode;
    MimePartAttach.ConvertCharset := False;
    MimePartAttach.MimeTypeFromExt(AAttachment.FileName);

    MimePartAttach.EncodePart;
    MimePartAttach.EncodePartHeader;
  end;

  fMIMEMess.EncodeMessage;

  fArqMIMe.Clear;
  fMIMEMess.Lines.SaveToStream(fArqMIMe);
end;

procedure TACBrMail.SendMail;
var
  vAttempts: Byte;
  c, i: Integer;
  ErrorMsgs: String;

  procedure AddErrorMsg(const AError: String);
  begin
    if Trim(AError) = '' then
      Exit;

    if (pos(AError, ErrorMsgs) = 0) then
    begin
      if (ErrorMsgs <> '') then
        ErrorMsgs := ErrorMsgs + sLineBreak;

      ErrorMsgs := ErrorMsgs + AError;
    end;
  end;

begin
  ErrorMsgs := '';
  BuildMimeMess;

  if fTimeOut > 0 then
  begin
    fSMTP.Timeout := fTimeOut;
    fSMTP.Sock.ConnectionTimeout := fTimeOut;
  end;

  // DEBUG //
  // SaveToFile('c:\app\Mail.eml'); {Para debug, comentar o Clear; da linha 367}

  // Login in SMTP //
  MailProcess(pmsLoginSMTP);
  if (fSMTP.TargetHost = '') then
    SmtpError('SMTP Error: Server not informed');

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.Login then
      Break;

    AddErrorMsg(fSMTP.ResultString);
    AddErrorMsg(IntToStr(fSMTP.Sock.LastError) + ' - ' + fSMTP.Sock.LastErrorDesc);

    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to Login.' + sLineBreak + ErrorMsgs);
  end;

  if fDeliveryConfirmation then
  begin
    if (fSMTP.FindCap('DSN') = '') then
      SmtpError('SMTP Error: The SMTP Server does not support Delivery Status Notification');

    fSMTP.DeliveryStatusNotification := [dsnSucecess, dsnFailure];
  end;

  // Sending Mail Form //
  MailProcess(pmsStartSends);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailFrom(fFrom, Length(fFrom)) then
      Break;

    AddErrorMsg(fSMTP.ResultString);
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to send MailFrom.' + sLineBreak + ErrorMsgs);
  end;

  // Sending MailTo //
  MailProcess(pmsSendTo);

  for i := 0 to fMIMEMess.Header.ToList.Count - 1 do
  begin
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.ToList[i]))then
        Break;

      AddErrorMsg(fSMTP.ResultString);
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send MailTo.' + sLineBreak + ErrorMsgs);
    end;
  end;

  // Sending Carbon Copies //
  c := fMIMEMess.Header.CCList.Count;
  if c > 0 then
    MailProcess(pmsSendCC);

  for i := 0 to c - 1 do
  begin
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.CCList[i])) then
        Break;

      AddErrorMsg(fSMTP.ResultString);
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send CC list.' + sLineBreak + ErrorMsgs);
    end;
  end;

  // Sending Blind Carbon Copies //
  c := fBCC.Count;
  if c > 0 then
    MailProcess(pmsSendBCC);

  for i := 0 to c - 1 do
  begin
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fBCC[I])) then
        Break;

      AddErrorMsg(fSMTP.ResultString);
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send BCC list.' + sLineBreak + ErrorMsgs);
    end;
  end;

  // Sending MIMEMess Data //
  MailProcess(pmsSendData);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailData(fMIMEMess.Lines) then
      Break;

    AddErrorMsg(fSMTP.ResultString);
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to send Mail data.' + sLineBreak + ErrorMsgs);
  end;

  // Login out from SMTP //
  MailProcess(pmsLogoutSMTP);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.Logout then
      Break;

    AddErrorMsg(fSMTP.ResultString);
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to Logout.' + sLineBreak + ErrorMsgs);
  end;

  // Done //
  try
    MailProcess(pmsDone);

    if Assigned(OnAfterMailProcess) then
      OnAfterMailProcess( self );
  finally
    Clear;
  end;
end;

procedure TACBrMail.ClearAttachments;
begin
  fAttachments.Clear;
end;

procedure TACBrMail.AddAttachment(const aFileName: string; aDescription: string;
  const aDisposition: TMailAttachmentDisposition = adInline);
var
  AAttachment: TMailAttachment;
begin
  if not FileExists(aFileName) then
    DoException( Exception.Create('Add Attachment: File not Exists.') );

  AAttachment := fAttachments.New;
  AAttachment.FileName     := aFileName;

  if (aDescription = '') then
    AAttachment.Description := ExtractFileName(AAttachment.FileName)
  else
    AAttachment.Description := aDescription;

  AAttachment.Disposition := aDisposition;

  AAttachment.Stream.LoadFromFile(aFileName)
end;

procedure TACBrMail.AddAttachment(const aFileName: string);
begin
  AddAttachment(aFileName, '');
end;

procedure TACBrMail.AddAttachment(aStream: TStream; aDescription: string;
  const aDisposition: TMailAttachmentDisposition = adInline);
var
  AAttachment: TMailAttachment;
begin
  if not Assigned(aStream) then
    DoException( Exception.Create('Add Attachment: Access Violation.') );

  if (Trim(aDescription) = '') then
    aDescription := 'file_' + FormatDateTime('hhnnsszzz',Now);

  aStream.Position := 0;
  AAttachment := fAttachments.New;
  AAttachment.FileName    := aDescription;
  AAttachment.Description := aDescription;
  AAttachment.Disposition := aDisposition;
  AAttachment.Stream.CopyFrom(aStream, aStream.Size);
end;

procedure TACBrMail.AddAttachment(aStream: TStream);
begin
  AddAttachment(aStream, '');
end;

procedure TACBrMail.AddAddress(const aEmail: string; const aName: string);
begin
  if Trim(aName) <> '' then
    fMIMEMess.Header.ToList.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fMIMEMess.Header.ToList);
end;

procedure TACBrMail.AddReplyTo(const aEmail: string; const aName: string);
begin
  if Trim(aName) <> '' then
    fReplyTo.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fReplyTo);
end;

procedure TACBrMail.AddCC(const aEmail: string; const aName: string);
begin
  if Trim(aName) <> '' then
    fMIMEMess.Header.CCList.Add('"' + aName + '" <' + aEmail + '>')
  else
    AddEmailWithDelimitersToList(aEmail, fMIMEMess.Header.CCList);
end;

procedure TACBrMail.AddBCC(const aEmail: string);
begin
  AddEmailWithDelimitersToList(aEmail, fBCC);
end;

function TACBrMail.SaveToStream(AStream: TStream): Boolean;
begin
  Result := True;
  try
    fArqMIMe.SaveToStream(AStream);
  except
    Result := False;
  end;
end;

{ TACBrMailThread }

constructor TACBrMailThread.Create(AOwner: TACBrMail);
begin
  FreeOnTerminate  := True;
  FACBrMail        := AOwner;

  inherited Create(False);
end;

procedure TACBrMailThread.Execute;
begin
  FStatus := pmsStartProcess;

  // Save events pointers
  FOnMailProcess   := FACBrMail.OnMailProcess ;
  FOnMailException := FACBrMail.OnMailException;
  FOnBeforeMailProcess := FACBrMail.OnBeforeMailProcess;
  FOnAfterMailProcess := FACBrMail.OnAfterMailProcess;
  MailCriticalSection.Acquire;
  try
    // Redirect events to Internal methods, to use Synchronize
    FACBrMail.OnMailException := MailException;
    FACBrMail.OnMailProcess := MailProcess;
    FACBrMail.OnBeforeMailProcess := BeforeMailProcess;
    FACBrMail.OnAfterMailProcess := AfterMailProcess;
    FACBrMail.UseThread := False;

    if (not Self.Terminated) then
      FACBrMail.SendMail;
  finally
    // Discard ACBrMail copy
    FACBrMail.Free;
    Terminate;
    MailCriticalSection.Release;
  end;
end;

procedure TACBrMailThread.MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  FStatus := aStatus;
  Synchronize(DoMailProcess);
end;

procedure TACBrMailThread.DoMailProcess;
begin
  if Assigned(FOnMailProcess) then
    FOnMailProcess(FACBrMail, FStatus) ;
end;

procedure TACBrMailThread.BeforeMailProcess(Sender: TObject);
begin
  Synchronize(DoBeforeMailProcess);
end;

procedure TACBrMailThread.DoBeforeMailProcess;
begin
  if Assigned(FOnBeforeMailProcess) then
    FOnBeforeMailProcess( FACBrMail );
end;

procedure TACBrMailThread.AfterMailProcess(Sender: TObject);
begin
  Synchronize(DoAfterMailProcess);
end;

procedure TACBrMailThread.DoAfterMailProcess;
begin
  if Assigned(FOnAfterMailProcess) then
    FOnAfterMailProcess( FACBrMail );
end;

procedure TACBrMailThread.MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  FException := E;
  Synchronize(DoMailException);
  ThrowIt := False;
end;

procedure TACBrMailThread.DoMailException;
begin
  FThrowIt := False;
  if Assigned(FOnMailException) then
    FOnMailException(FACBrMail, FException, FThrowIt);
end;

initialization
  MailCriticalSection := TCriticalSection.Create;

finalization;
  MailCriticalSection.Free;

end.


