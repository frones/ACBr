{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2013 Jean Patrick Figueiredo dos Santos     }
{                                       Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{         Silvio Clécio - xmailer - https://github.com/silvioprog/xmailer      }
{         Projeto PHPMailer - https://github.com/Synchro/PHPMailer             }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 11/10/2013: Primeira Versao
|*    Jean Patrick Figueiredo dos Santos
|*
******************************************************************************}

unit ACBrMail;

{$I ACBr.inc}

interface

uses
  SSL_OpenSSL, SMTPSend, MimePart, MimeMess, SynaChar, SynaUtil, Classes,
  SysUtils, ACBrBase {$IFDEF FPC}, FileUtil {$ENDIF} ;

type

  TMailStatus = (pmsStartProcess, pmsConfigHeaders, pmsLoginSMTP, pmsStartSends,
                 pmsSendTo, pmsSendCC, pmsSendBCC, pmsSendReplyTo, pmsSendData,
                 pmsLogoutSMTP, pmsDone, pmsError);

  TMailCharset = TMimeChar;

  TMailAttachments = array of record
    FileName: string;
    Stream: TMemoryStream;
    NameRef: string;
  end;

  TACBrThread = class(TThread)
  private
    fOwner : TComponent;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TComponent);
  end;

  TACBrOnMailProcess = procedure(const aStatus: TMailStatus) of object;

  { TACBrMail }

  TACBrMail = class(TACBrComponent)

  private

    fSMTP                : TSMTPSend;
    fMIMEMess            : TMimeMess;

    fReadingConfirmation : boolean;
    fOnMailProcess       : TACBrOnMailProcess;

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
    fThread              : TACBrThread;
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
    function GetAutoTLS: Boolean;
    procedure SetHost(aValue: string);
    procedure SetPort(aValue: string);
    procedure SetUsername(aValue: string);
    procedure SetPassword(aValue: string);
    procedure SetFullSSL(aValue: Boolean);
    procedure SetAutoTLS(aValue: Boolean);

    function GetPriority: TMessPriority;
    procedure SetPriority(aValue: TMessPriority);

    procedure SetBody(const aValue : TStringList);
    procedure SetAltBody(const aValue : TStringList);
    procedure SmtpError(const pMsgError: string);

  protected
    procedure SendMail;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MailProcess(const aStatus: TMailStatus);
    procedure Send(UseThreadNow: Boolean); overload;
    procedure Send; overload;
    procedure Clear;

    procedure AddAttachment(aFileName: string; aNameRef: string); overload;
    procedure AddAttachment(aFileName: string); overload;
    procedure AddAttachment(aStream: TStream; aNameRef: string); overload;
    procedure AddAttachment(aStream: TStream); overload;

    procedure AddAddress(aEmail: string; aName: string = '');
    procedure AddReplyTo(aEmail: string; aName: string = '');
    procedure AddCC(aEmail: string; aName: string = '');
    procedure AddBCC(aEmail: string);

    property AltBody: TStringList read fAltBody write SetAltBody;
    property Body: TStringList read fBody write SetBody;

    property GetLastSmtpError: string read fGetLastSmtpError;

  published
    property Host: string read GetHost write SetHost;
    property Port: string read GetPort write SetPort;
    property Username: string read GetUsername write SetUsername;
    property Password: string read GetPassword write SetPassword;
    property SetSSL: boolean read GetFullSSL write SetFullSSL;
    property SetTLS: boolean read GetAutoTLS write SetAutoTLS;
    property Priority: TMessPriority read GetPriority write SetPriority default MP_normal;
    property ReadingConfirmation: boolean read fReadingConfirmation write fReadingConfirmation default False;
    property IsHTML: boolean read fIsHTML write fIsHTML default False;
    property UseThread: boolean read fUseThread write fUseThread default False;
    property Attempts: Byte read fAttempts write fAttempts;
    property From: string read fFrom write fFrom;
    property FromName: string read fFromName write fFromName;
    property Subject: string read fSubject write fSubject;
    property DefaultCharset: TMailCharset read fDefaultCharsetCode write fDefaultCharsetCode;
    property IDECharset: TMailCharset read fIDECharsetCode write fIDECharsetCode;
    property OnBeforeMailProcess: TNotifyEvent read fOnBeforeMailProcess write fOnBeforeMailProcess;
    property OnMailProcess: TACBrOnMailProcess read fOnMailProcess write fOnMailProcess;
    property OnAfterMailProcess: TNotifyEvent read fOnAfterMailProcess write fOnAfterMailProcess;
  end;

implementation

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

function TACBrMail.GetAutoTLS: Boolean;
begin
  Result := fSMTP.AutoTLS;
end;

procedure TACBrMail.SetHost(aValue: string);
begin
  fSMTP.TargetHost := aValue;
end;

procedure TACBrMail.SetPort(aValue: string);
begin
  fSMTP.TargetPort := aValue;
end;

procedure TACBrMail.SetUsername(aValue: string);
begin
  fSMTP.UserName := aValue;
end;

procedure TACBrMail.SetPassword(aValue: string);
begin
  fSMTP.Password := aValue;
end;

procedure TACBrMail.SetFullSSL(aValue: Boolean);
begin
  fSMTP.FullSSL := aValue;
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

procedure TACBrMail.SetBody(const aValue: TStringList);
begin
  fBody.Assign( aValue );
end;

procedure TACBrMail.SetAltBody(const aValue: TStringList);
begin
  fAltBody.Assign( aValue );
end;

procedure TACBrMail.SmtpError(const pMsgError: string);
begin
  Clear;
  if fThread <> nil Then fThread.Terminate;
  fGetLastSmtpError := pMsgError;
  MailProcess(pmsError);
  raise Exception.Create(pMsgError);
end;

procedure TACBrMail.Clear;
var
  i: Integer;
begin
  if Length(fAttachments) > 0 then
  begin
    for i := 0 to Length(fAttachments) - 1 do
      if Assigned(fAttachments[i].Stream) then
        fAttachments[i].Stream.Free;

    SetLength(fAttachments, 0);
  end;

  fSMTP.Reset;
  fMIMEMess.Header.Clear;
  fMIMEMess.Clear;
  fReplyTo.Clear;
  fBCC.Clear;
  fBody.Clear;
  fAltBody.Clear;
end;

procedure TACBrMail.MailProcess(const aStatus: TMailStatus);
begin
  if Assigned(fOnMailProcess) then
    fOnMailProcess(aStatus);
end;

constructor TACBrMail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSMTP := TSMTPSend.Create;
  fMIMEMess := TMimeMess.Create;
  fAltBody := TStringList.Create;
  fBody := TStringList.Create;

  fOnBeforeMailProcess := nil;
  fOnAfterMailProcess := nil;

  SetLength(fAttachments, 0);
  SetPriority(MP_normal);
  fDefaultCharsetCode := UTF_8;
  fIDECharsetCode := {$IFDEF FPC}UTF_8{$ELSE}CP1252{$ENDIF};
  fReadingConfirmation := False;
  fIsHTML := False;
  fUseThread := False;
  fAttempts := 3;
  fFrom := '';
  fFromName := '';
  fSubject := '';

  fReplyTo := TStringList.Create;
  {$IFDEF FPC}
  fReplyTo.StrictDelimiter := True;
  {$ENDIF}
  fReplyTo.Delimiter := ';';

  fBCC := TStringList.Create;
  {$IFDEF FPC}
  fBCC.StrictDelimiter := True;
  {$ENDIF}
  fBCC.Delimiter := ';';

  // NOTAR ISSO: fSMTP.Sock.OnStatus := ;

end;

destructor TACBrMail.Destroy;
begin
  Clear;
  fAltBody.Free;
  fBody.Free;
  fBCC.Free;
  fReplyTo.Free;
  fMIMEMess.Free;
  fSMTP.Free;
  inherited Destroy;
end;

procedure TACBrMail.Send(UseThreadNow: Boolean);
begin
  if UseThreadNow then
  begin
    if fThread <> nil Then
      fThread.Terminate;

    fThread := TACBrThread.Create(Self);
  end
  else
    SendMail;
end;

procedure TACBrMail.Send;
begin
  Send( UseThread );
end;

procedure TACBrMail.SendMail;
var
  vAttempts: Byte;
  vMIMEPart, vMIMEPart2, vMIMEPart3: TMimePart;
  i, c: Integer;

  procedure _SetReadingConfirmation(const a: string);
  begin
    if fReadingConfirmation then
      fMIMEMess.Header.CustomHeaders.Insert(0,
        'Disposition-Notification-To: ' + a);
  end;

begin
  if Assigned(OnBeforeMailProcess) then
    OnBeforeMailProcess( self );

  MailProcess(pmsStartProcess);

  if fDefaultCharsetCode <> fIDECharsetCode then
  begin
    if fBody.Count > 0 then
      fBody.Text := CharsetConversion(fBody.Text, fIDECharsetCode, fDefaultCharsetCode);

    if fAltBody.Count > 0 then
      fAltBody.Text := CharsetConversion(fAltBody.Text, fIDECharsetCode, fDefaultCharsetCode);
  end;

  if fIsHTML then
  begin
    vMIMEPart := nil;

    if (fAltBody.Text <> '') then
    begin
      vMIMEPart := fMIMEMess.AddPartMultipart('alternative', nil);
      vMIMEPart2 := fMIMEMess.AddPartText(fAltBody, vMIMEPart);
      with vMIMEPart2 do
      begin
        Primary := 'text';
        Secondary := 'plain';
        Description := '';
        Disposition := '';
        CharsetCode := fDefaultCharsetCode;
        EncodingCode := ME_7BIT;
        TargetCharset := fDefaultCharsetCode;
        ConvertCharset := True;
        EncodePart;
        EncodePartHeader;
      end;
    end; // fim altbody

    if (Length(fAttachments) > 0) or (fAltBody.Text <> '') then
      vMIMEPart := fMIMEMess.AddPartMultipart('related', vMIMEPart);

    if (fBody.Text <> '') then
    begin
      if (Length(fAttachments) > 0) or (fAltBody.Text <> '') then
      begin
        vMIMEPart3 := fMIMEMess.AddPartHTML(fBody, vMIMEPart);
        with vMIMEPart3 do
        begin
          Primary := 'text';
          Secondary := 'html';
          Description := '';
          Disposition := '';
          CharsetCode := fDefaultCharsetCode;
          EncodingCode := ME_7BIT;
          TargetCharset := fDefaultCharsetCode;
          ConvertCharset := True;
          EncodePart;
          EncodePartHeader;
        end;
      end
      else
      begin
        vMIMEPart := fMIMEMess.AddPartHTML(fBody, nil);
        with vMIMEPart do
        begin
          Primary := 'text';
          Secondary := 'html';
          Description := '';
          CharsetCode := fDefaultCharsetCode;
          EncodingCode := ME_7BIT;
          TargetCharset := fDefaultCharsetCode;
          ConvertCharset := True;
          EncodePart;
          EncodePartHeader;
        end;
      end;
    end; // fim body

    // anexos
    if Length(fAttachments) > 0 then
      for i := 0 to Length(fAttachments) - 1 do
      begin
        if (fAttachments[i].FileName = '') then
        begin
          if (Trim(fAttachments[i].NameRef) = '') then
            fAttachments[i].NameRef := 'file_' + FormatDateTime('hhnnsszzz',Now);

          fMIMEMess.AddPartHTMLBinary(fAttachments[i].Stream, fAttachments[i].NameRef, '<' +
            fAttachments[i].NameRef + '>', vMIMEPart);
        end
        else
        begin
          if (Trim(fAttachments[i].NameRef) = '') then
            fAttachments[i].NameRef := ExtractFileName(fAttachments[i].NameRef);

          fMIMEMess.AddPartHTMLBinaryFromFile(fAttachments[i].FileName,
            '<' + fAttachments[i].NameRef + '>', vMIMEPart);
        end;
      end;
  // fim html
  end
  else
  begin
    if Length(fAttachments) > 0 then
      vMIMEPart := fMIMEMess.AddPartMultipart('mixed', nil);

    if (fBody.Text <> '') then
    begin
      if Length(fAttachments) > 0 then
      begin
        vMIMEPart2 := fMIMEMess.AddPartText(fBody, vMIMEPart);
        with vMIMEPart2 do
        begin
          Primary := 'text';
          Secondary := 'plain';
          Description := '';
          CharsetCode := fDefaultCharsetCode;
          EncodingCode := ME_7BIT;
          TargetCharset := fDefaultCharsetCode;
          ConvertCharset := True;
          EncodePart;
          EncodePartHeader;
        end;
      end
      else
      begin
        vMIMEPart := fMIMEMess.AddPartText(fBody, nil);
        with vMIMEPart do
        begin
          Primary := 'text';
          Secondary := 'plain';
          Description := '';
          CharsetCode := fDefaultCharsetCode;
          EncodingCode := ME_7BIT;
          TargetCharset := fDefaultCharsetCode;
          ConvertCharset := True;
          EncodePart;
          EncodePartHeader;
        end;
      end;
    end;

    if Length(fAttachments) > 0 then
      for i := 0 to Length(fAttachments) - 1 do
      begin
        if (fAttachments[i].FileName = '') then
        begin
          if (Trim(fAttachments[i].NameRef) = '') then
            fAttachments[i].NameRef := 'file_' + FormatDateTime('hhnnsszzz',Now);
          fMIMEMess.AddPartBinary(fAttachments[i].Stream, fAttachments[i].NameRef, vMIMEPart);
        end
        else
        begin
          fMIMEMess.AddPartBinaryFromFile(fAttachments[i].FileName, vMIMEPart);
        end;
      end;
  end;

  MailProcess(pmsConfigHeaders);

  fMIMEMess.Header.CharsetCode := fDefaultCharsetCode;

  if fDefaultCharsetCode <> fIDECharsetCode then
    fMIMEMess.Header.Subject := CharsetConversion(fSubject, fIDECharsetCode, fDefaultCharsetCode)
  else
    fMIMEMess.Header.Subject := fSubject;

  if Trim(fFromName) <> '' then
    fMIMEMess.Header.From := '"' + fFromName + ' <' + From + '>"'
  else
    fMIMEMess.Header.From := fFrom;

  if fReplyTo.Count > 0 then
    fMIMEMess.Header.ReplyTo := fReplyTo.DelimitedText;

  if fReadingConfirmation then
    _SetReadingConfirmation(fFrom);

  fMIMEMess.Header.XMailer := 'X-Mailer plugin';

  fMIMEMess.EncodeMessage;

  MailProcess(pmsLoginSMTP);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.Login then
      Break;
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to Login.');
  end;

  MailProcess(pmsStartSends);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailFrom(fFrom, Length(fFrom)) then
      Break;
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to send MailFrom.');
  end;

  MailProcess(pmsSendTo);

  for i := 0 to fMIMEMess.Header.ToList.Count - 1 do
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.ToList.Strings[i]))then
        Break;
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send MailTo.');
    end;

  c := fMIMEMess.Header.CCList.Count;

  if c > 0 then
    MailProcess(pmsSendCC);

  for i := 0 to c - 1 do
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fMIMEMess.Header.CCList.Strings[i])) then
        Break;
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send CC list.');
    end;

  c := fBCC.Count;

  if c > 0 then
    MailProcess(pmsSendBCC);

  for i := 0 to c - 1 do
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fBCC.Strings[I])) then
        Break;
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send BCC list.');
    end;

  c := fReplyTo.Count;

  if c > 0 then
    MailProcess(pmsSendReplyTo);

  for i := 0 to c - 1 do
    for vAttempts := 1 to fAttempts do
    begin
      if fSMTP.MailTo(GetEmailAddr(fReplyTo.Strings[I])) then
        Break;
      if vAttempts >= fAttempts then
        SmtpError('SMTP Error: Unable to send ReplyTo list.');
    end;

  MailProcess(pmsSendData);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.MailData(fMIMEMess.Lines) then
      Break;
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to send Mail data.');
  end;

  MailProcess(pmsLogoutSMTP);

  for vAttempts := 1 to fAttempts do
  begin
    if fSMTP.Logout then
      Break;
    if vAttempts >= fAttempts then
      SmtpError('SMTP Error: Unable to Logout.');
  end;

  Clear;

  MailProcess(pmsDone);

  if Assigned(OnAfterMailProcess) then
    OnAfterMailProcess( self );
end;

procedure TACBrMail.AddAttachment(aFileName: string; aNameRef: string);
var
  i: integer;
begin

  {$IFDEF FPC}
  if not FileExistsUTF8(aFileName) then
  begin
    if not FileExists(aFileName) then
      raise Exception.Create('Add Attachment: File not Exists.');
  end
  else
    aFileName := Utf8ToAnsi(aFileName);
  {$ELSE}
  if not FileExists(aFileName) then
    raise Exception.Create('Add Attachment: File not Exists.');
  {$ENDIF}

  i := Length(fAttachments);
  SetLength(fAttachments, i + 1);
  fAttachments[i].FileName := aFileName;
  fAttachments[i].Stream := nil;
  fAttachments[i].NameRef := aNameRef;
end;

procedure TACBrMail.AddAttachment(aFileName: string);
begin
  AddAttachment(aFileName, '');
end;

procedure TACBrMail.AddAttachment(aStream: TStream; aNameRef: string);
var
  i: integer;
begin
  if not Assigned(aStream) then
    raise Exception.Create('Add Attachment: Access Violation.');

  i := Length(fAttachments);
  SetLength(fAttachments, i + 1);

  aStream.Position := 0;
  fAttachments[i].FileName := '';
  fAttachments[i].Stream := TMemoryStream.Create;
  fAttachments[i].Stream.Position := 0;
  fAttachments[i].Stream.CopyFrom(aStream, aStream.Size);
  fAttachments[i].NameRef := aNameRef;
end;

procedure TACBrMail.AddAttachment(aStream: TStream);
begin
  AddAttachment(aStream, '');
end;

procedure TACBrMail.AddAddress(aEmail: string; aName: string);
begin
  if Trim(aName) <> '' then
    fMIMEMess.Header.ToList.Add('"' + aName + ' <' + aEmail + '>"')
  else
    fMIMEMess.Header.ToList.Add(aEmail);
end;

procedure TACBrMail.AddReplyTo(aEmail: string; aName: string);
begin
  if Trim(aName) <> '' then
    fReplyTo.Add('"' + aName + ' <' + aEmail + '>"')
  else
    fReplyTo.Add(aEmail);
end;

procedure TACBrMail.AddCC(aEmail: string; aName: string);
begin
  if Trim(aName) <> '' then
    fMIMEMess.Header.CCList.Add('"' + aName + ' <' + aEmail + '>"')
  else
    fMIMEMess.Header.CCList.Add(aEmail);
end;

procedure TACBrMail.AddBCC(aEmail: string);
begin
  fBCC.Add(aEmail);
end;

{ TACBrThread }

constructor TACBrThread.Create(AOwner: TComponent);
begin
  FreeOnTerminate := True;
  fOwner          := AOwner;

  inherited Create(False);
end;

procedure TACBrThread.Execute;
begin
  if (not terminated) then
    TACBrMail(FOwner).SendMail;
end;

end.
