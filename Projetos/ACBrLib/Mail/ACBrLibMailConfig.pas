{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }

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
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{        Rua Cel.Aureliano de Camargo, 973 - Tatuí - SP - 18270-170            }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrLibMailConfig;

interface

uses
  Classes, SysUtils, IniFiles, MimeMess, SynaChar,
  ACBrLibConfig, ACBrMail;

type

  { TMailConfig }
  TMailConfig = class
  private
    FAttempts: Byte;
    FSetTLS: boolean;
    FDefaultCharset: TMailCharset;
    FFrom: string;
    FFromName: string;
    FSetSSL: boolean;
    FHost: string;
    FIDECharset: TMailCharset;
    FIsHTML: boolean;
    FPassword: string;
    FPort: string;
    FPriority: TMessPriority;
    FReadingConfirmation: boolean;
    FSubject: string;
    FTimeOut: Integer;
    FUsername: string;
    FUseThread: boolean;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LerIni(const AIni: TCustomIniFile);
    procedure GravarIni(const AIni: TCustomIniFile);

    property Host: string read FHost write FHost;
    property Port: string read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property SetSSL: boolean read FSetSSL write FSetSSL;
    property SetTLS: boolean read FSetTLS write FSetTLS;
    property Priority: TMessPriority read FPriority write FPriority;
    property ReadingConfirmation: boolean read FReadingConfirmation write FReadingConfirmation;
    property IsHTML: boolean read FIsHTML write FIsHTML;
    property UseThread: boolean read FUseThread write FUseThread;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property Attempts: Byte read FAttempts write FAttempts;
    property From: string read FFrom write FFrom;
    property FromName: string read FFromName write FFromName;
    property Subject: string read FSubject write FSubject;
    property DefaultCharset: TMailCharset read FDefaultCharset write FDefaultCharset;
    property IDECharset: TMailCharset read FIDECharset write FIDECharset;
  end;

  { TLibMailConfig }
  TLibMailConfig = class(TLibConfig)
  private
    FMailConfig: TMailConfig;

  protected
    function AtualizarArquivoConfiguracao: Boolean; override;

    procedure INIParaClasse; override;
    procedure ClasseParaINI; override;
    procedure ClasseParaComponentes; override;

    procedure Travar; override;
    procedure Destravar; override;

  public
    constructor Create(AOwner: TObject; ANomeArquivo: String = ''; AChaveCrypt: AnsiString = ''); override;
    destructor Destroy; override;

    property MailConfig: TMailConfig read FMailConfig;
  end;

implementation

uses
  ACBrLibMailClass, ACBrLibMailConsts, ACBrLibConsts, ACBrLibComum, ACBrUtil;

{ TMailConfig }
constructor TMailConfig.Create;
begin
  FAttempts := 0;
  FSetTLS := False;
  FDefaultCharset := UTF_8;
  FFrom := '';
  FFromName := '';
  FSetSSL := False;
  FHost := '';
  FIDECharset := {$IFDEF FPC}UTF_8{$ELSE}CP1252{$ENDIF};
  FIsHTML := False;
  FPassword := '';
  FPort := '';
  FPriority := MP_normal;
  FReadingConfirmation := False;
  FSubject := '';
  FTimeOut := 0;
  FUsername := '';
  FUseThread := False;
end;

destructor TMailConfig.Destroy;
begin

  inherited Destroy;
end;

procedure TMailConfig.LerIni(const AIni: TCustomIniFile);
begin
  FAttempts := AIni.ReadInteger(CSessaoMail, CChaveAttempts, FAttempts);
  FSetTLS := AIni.ReadBool(CSessaoMail, CChaveSetTLS, FSetTLS);
  FDefaultCharset := TMailCharSet(AIni.ReadInteger(CSessaoMail, CChaveDefaultCharset, Integer(FDefaultCharset)));
  FFrom := AIni.ReadString(CSessaoMail, CChaveFrom, FFrom);
  FFromName := AIni.ReadString(CSessaoMail, CChaveFromName, FFromName);
  FSetSSL := AIni.ReadBool(CSessaoMail, CChaveSetSSL, FSetSSL);
  FHost := AIni.ReadString(CSessaoMail, CChaveHost, FHost);
  FIDECharset := TMailCharSet(AIni.ReadInteger(CSessaoMail, CChaveIDECharset, Integer(FIDECharset)));
  FIsHTML := AIni.ReadBool(CSessaoMail, CChaveIsHTML, FIsHTML);
  FPassword := AIni.ReadString(CSessaoMail, CChavePassword, FPassword);
  FPort := AIni.ReadString(CSessaoMail, CChavePort, FPort);
  FPriority := TMessPriority(AIni.ReadInteger(CSessaoMail, CChavePriority, Integer(FPriority)));
  FReadingConfirmation := AIni.ReadBool(CSessaoMail, CChaveReadingConfirmation, FReadingConfirmation);
  FSubject := AIni.ReadString(CSessaoMail, CChaveSubject, FSubject);
  FTimeOut := AIni.readInteger(CSessaoMail, CChaveTimeOut, FTimeOut);
  FUsername := AIni.ReadString(CSessaoMail, CChaveUsername, FUsername);
  FUseThread := AIni.ReadBool(CSessaoMail, CChaveUseThread, FUseThread);
end;

procedure TMailConfig.GravarIni(const AIni: TCustomIniFile);
begin
  AIni.WriteInteger(CSessaoMail, CChaveAttempts, FAttempts);
  AIni.WriteBool(CSessaoMail, CChaveSetTLS, FSetTLS);
  AIni.WriteInteger(CSessaoMail, CChaveDefaultCharset, Integer(FDefaultCharset));
  AIni.WriteString(CSessaoMail, CChaveFrom, FFrom);
  AIni.WriteString(CSessaoMail, CChaveFromName, FFromName);
  AIni.WriteBool(CSessaoMail, CChaveSetSSL, FSetSSL);
  AIni.WriteString(CSessaoMail, CChaveHost, FHost);
  AIni.WriteInteger(CSessaoMail, CChaveIDECharset, Integer(FIDECharset));
  AIni.WriteBool(CSessaoMail, CChaveIsHTML, FIsHTML);
  AIni.WriteString(CSessaoMail, CChavePassword, FPassword);
  AIni.WriteString(CSessaoMail, CChavePort, FPort);
  AIni.WriteInteger(CSessaoMail, CChavePriority, Integer(FPriority));
  AIni.WriteBool(CSessaoMail, CChaveReadingConfirmation, FReadingConfirmation);
  AIni.WriteString(CSessaoMail, CChaveSubject, FSubject);
  AIni.WriteInteger(CSessaoMail, CChaveTimeOut, FTimeOut);
  AIni.WriteString(CSessaoMail, CChaveUsername, FUsername);
  AIni.WriteBool(CSessaoMail, CChaveUseThread, FUseThread);
end;

{ TLibMailConfig }

constructor TLibMailConfig.Create(AOwner: TObject; ANomeArquivo: String; AChaveCrypt: AnsiString);
begin
  inherited Create(AOwner, ANomeArquivo, AChaveCrypt);

  FMailConfig := TMailConfig.Create;
end;

destructor TLibMailConfig.Destroy;
begin
  FMailConfig.Free;

  inherited Destroy;
end;

function TLibMailConfig.AtualizarArquivoConfiguracao: Boolean;
var
  Versao: String;
begin
  Versao := Ini.ReadString(CSessaoVersao, CLibMailNome, '0');
  Result := (CompareVersions(CLibMailVersao, Versao) > 0) or
            (inherited AtualizarArquivoConfiguracao);
end;

procedure TLibMailConfig.INIParaClasse;
begin
  inherited INIParaClasse;

  FMailConfig.LerIni(Ini);
end;

procedure TLibMailConfig.ClasseParaINI;
begin
  inherited ClasseParaINI;

  Ini.WriteString(CSessaoVersao, CLibMailNome, CLibMailVersao);

  FMailConfig.GravarIni(Ini);
end;

procedure TLibMailConfig.ClasseParaComponentes;
begin
  if Assigned(Owner) then
    TACBrLibMail(Owner).MailDM.AplicarConfiguracoes;
end;

procedure TLibMailConfig.Travar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibMail(Owner) do
      MailDM.Travar;
  end;
end;

procedure TLibMailConfig.Destravar;
begin
  if Assigned(Owner) then
  begin
    with TACBrLibMail(Owner) do
      MailDM.Destravar;
  end;
end;

end.

