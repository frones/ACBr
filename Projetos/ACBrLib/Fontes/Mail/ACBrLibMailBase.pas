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

unit ACBrLibMailBase;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibMailDataModule, ACBrMail, ACBrUtil.FilesIO;

type
  { TACBrLibMail }

  TACBrLibMail = class(TACBrLib)
  private
    FMailDM: TLibMailDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property MailDM: TLibMailDM read FMailDM;

    function SetSubject(const eSubject: PChar): longint;
    function AddAddress(const eEmail, eName: PChar): longint;
    function AddReplyTo(const eEmail, eName: PChar): longint;
    function AddCC(const eEmail, eName: PChar): longint;
    function AddBCC(const eEmail: PChar): longint;
    function ClearAttachment: longint;
    function AddAttachment(const eFileName, eDescription: PChar; const aDisposition: Integer): longint;
    function AddBody(const eBody: PChar): longint;
    function AddAltBody(const eAltBody: PChar): longint;
    function SaveToFile(const eFileName: PChar): longint;
    function GetMail: Pointer;
    function Clear: longint;
    function Send: longint;

  end;  


implementation

uses
  ACBrLibConsts, ACBrLibConfig, ACBrLibMailConfig;

{ TACBrLibMail }

constructor TACBrLibMail.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  FMailDM := TLibMailDM.Create(nil);
  FMailDM.Lib := Self;
end;

destructor TACBrLibMail.Destroy;
begin
  FMailDM.Free;
  inherited Destroy;
end;

procedure TACBrLibMail.Inicializar;
begin
  inherited Inicializar;

  GravarLog('TACBrLibMail.Inicializar - Feito', logParanoico);
end;

procedure TACBrLibMail.CriarConfiguracao(ArqConfig: string; ChaveCrypt: ansistring);
begin
  fpConfig := TLibMailConfig.Create(Self, ArqConfig, ChaveCrypt);
end;

procedure TACBrLibMail.Executar;
begin
  inherited Executar;
  FMailDM.AplicarConfiguracoes;
end;

function TACBrLibMail.SetSubject(const eSubject: PChar): longint;  
var
  ASubject: String;
begin
  try    
    ASubject := ConverterAnsiParaUTF8(eSubject);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_SetSubject( ' + ASubject + ' )', logCompleto, True)
    else
      GravarLog('MAIL_SetSubject', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.Subject := ASubject;
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddAddress(const eEmail, eName: PChar): longint;
var
  AEmail, AName: AnsiString;
begin
  try    
    AEmail := ConverterAnsiParaUTF8(eEmail);
    AName := ConverterAnsiParaUTF8(eName);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddAddress( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddAddress', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AddAddress(AEmail, AName);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddReplyTo(const eEmail, eName: PChar): longint;
var
  AEmail, AName: AnsiString;
begin
  try    
    AEmail := ConverterAnsiParaUTF8(eEmail);
    AName := ConverterAnsiParaUTF8(eName);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddReplyTo( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddReplyTo', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AddReplyTo(AEmail, AName);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddCC(const eEmail, eName: PChar): longint;
var
  AEmail, AName: AnsiString;
begin
  try    
    AEmail := ConverterAnsiParaUTF8(eEmail);
    AName := ConverterAnsiParaUTF8(eName);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddCC( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddCC', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AddCC(AEmail, AName);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddBCC(const eEmail: PChar): longint;
var
  AEmail: AnsiString;
begin
  try    
    AEmail := ConverterAnsiParaUTF8(eEmail);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddBCC( ' + AEmail + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddBCC', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AddBCC(AEmail);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddBody(const eBody: PChar): longint;  
var
  Body: TStringList;
begin
  try
    Body := TStringList.Create;
    Body.Text := ConverterAnsiParaUTF8(eBody);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddBody( ' + Body.Text + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddBody', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.Body.Assign(Body);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
      Body.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddAltBody(const eAltBody: PChar): longint;  
var
  AltBody: TStringList;
begin
  try
    AltBody := TStringList.Create;
    AltBody.Text := ConverterAnsiParaUTF8(eAltBody);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddAltBody( ' + AltBody.Text + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddAltBody', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AltBody.Assign(AltBody);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
      AltBody.Free;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.ClearAttachment: longint;
begin
  try
    GravarLog('MAIL_ClearAttachment', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.ClearAttachments;
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.AddAttachment(const eFileName, eDescription: PChar;
  const aDisposition: Integer): longint;
var
  AFileName, ADescription: AnsiString;
begin
  try    
    AFileName := ConverterAnsiParaUTF8(eFileName);
    ADescription := ConverterAnsiParaUTF8(eDescription);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_AddAttachment( ' + AFileName + ',' + ADescription + ',' +
             IntToStr(aDisposition) + ' )', logCompleto, True)
    else
      GravarLog('MAIL_AddAttachment', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.AddAttachment(AFileName, ADescription, TMailAttachmentDisposition(aDisposition));
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.SaveToFile(const eFileName: PChar): longint;
var
  AFileName: AnsiString;
begin
  try   
    AFileName := ConverterAnsiParaUTF8(eFileName);

    if Config.Log.Nivel > logNormal then
      GravarLog('MAIL_MailProcess( ' + AFileName + ' )', logCompleto, True)
    else
      GravarLog('MAIL_MailProcess', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.SaveToFile(AFileName);
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.GetMail: Pointer;  
begin
  try    
    GravarLog('MAIL_GetMail', logNormal);

    MailDM.Travar;
    try
      Result := MailDM.ACBrMail1;
      with TACBrMail(Result) do
        GravarLog('  ' + ClassName + ', ' + Name, logParanoico);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
    begin
      SetRetorno(E.Erro, E.Message);
      Result := Nil;
    end;

    on E: Exception do
    begin
      SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
      Result := Nil;
    end;
  end;
end;

function TACBrLibMail.Clear: longint;
begin
  try
    GravarLog('MAIL_Clear', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.Clear;
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

function TACBrLibMail.Send: longint;
begin
  try
    GravarLog('MAIL_Send', logNormal);

    MailDM.Travar;
    try
      MailDM.ACBrMail1.Send;
      Result := SetRetorno(ErrOK);
    finally
      MailDM.Destravar;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, ConverterUTF8ParaAnsi(E.Message));

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, ConverterUTF8ParaAnsi(E.Message));
  end;
end;

end.

