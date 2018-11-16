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

unit ACBrLibMailClass;

interface

uses
  Classes, SysUtils, typinfo,
  ACBrLibComum, ACBrLibMailDataModule, ACBrMail;

type
  PACBrMail = ^TACBrMail;

  { TACBrLibMail }

  TACBrLibMail = class(TACBrLib)
  private
    FMailDM: TLibMailDM;

  protected
    procedure Inicializar; override;
    procedure CriarConfiguracao(ArqConfig: string = ''; ChaveCrypt: ansistring = '');
      override;
    procedure Executar; override;
  public
    constructor Create(ArqConfig: string = ''; ChaveCrypt: ansistring = ''); override;
    destructor Destroy; override;

    property MailDM: TLibMailDM read FMailDM;
  end;

{%region Declaração da funções}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MAIL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_Inicializada: Boolean;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Diversos}
function MAIL_SetSubject(const eSubject: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddAddress(const eEmail, eName: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddReplyTo(const eEmail, eName: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddCC(const eEmail, eName: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddBCC(const eEmail: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_ClearAttachment: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddAttachment(const eFileName, eDescription: PChar;
            const aDisposition: Integer): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddBody(const eBody: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_AddAltBody(const eAltBody: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_SaveToFile(const eFileName: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_GetMail: Pointer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%region Envio}
function MAIL_Clear: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
function MAIL_Send(UseThreadNow: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
{%endregion}

{%endregion}

implementation

uses
  ACBrLibConsts, ACBrLibMailConsts, ACBrLibConfig, ACBrLibMailConfig;

{ TACBrLibMail }

constructor TACBrLibMail.Create(ArqConfig: string; ChaveCrypt: ansistring);
begin
  inherited Create(ArqConfig, ChaveCrypt);
  fpNome := CLibMailNome;
  fpVersao := CLibMailVersao;

  FMailDM := TLibMailDM.Create(nil);
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

{%region Mail}

{%region Redeclarando Métodos de ACBrLibComum, com nome específico}
function MAIL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicializar(eArqConfig, eChaveCrypt);
end;

function MAIL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Finalizar;
end;

function MAIL_Inicializada: Boolean;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Inicalizada;
end;

function MAIL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Nome(sNome, esTamanho);
end;

function MAIL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_Versao(sVersao, esTamanho);
end;

function MAIL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_UltimoRetorno(sMensagem, esTamanho);
end;

function MAIL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLer(eArqConfig);
end;

function MAIL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravar(eArqConfig);
end;

function MAIL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar;
  var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigLerValor(eSessao, eChave, sValor, esTamanho);
end;

function MAIL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  Result := LIB_ConfigGravarValor(eSessao, eChave, eValor);
end;
{%endregion}

{%region Diversos}
function MAIL_SetSubject(const eSubject: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  ASubject: String;
begin
  try
    VerificarLibInicializada;
    ASubject := String(eSubject);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_SetSubject( ' + ASubject + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_SetSubject', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.Subject := ASubject;
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddAddress(const eEmail, eName: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AEmail: AnsiString;
  AName: AnsiString;
begin
  try
    VerificarLibInicializada;
    AEmail := AnsiString(eEmail);
    AName := AnsiString(eName);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_AddAddress( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_AddAddress', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.AddAddress(AEmail, AName);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddReplyTo(const eEmail, eName: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AEmail: AnsiString;
  AName: AnsiString;
begin
  try
    VerificarLibInicializada;
    AEmail := AnsiString(eEmail);
    AName := AnsiString(eName);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_AddReplyTo( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_AddReplyTo', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.AddReplyTo(AEmail, AName);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddCC(const eEmail, eName: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AEmail: AnsiString;
  AName: AnsiString;
begin
  try
    VerificarLibInicializada;
    AEmail := AnsiString(eEmail);
    AName := AnsiString(eName);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_AddCC( ' + AEmail + ',' + AName + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_AddCC', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.AddCC(AEmail, AName);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddBCC(const eEmail: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AEmail: AnsiString;
begin
  try
    VerificarLibInicializada;
    AEmail := AnsiString(eEmail);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_AddBCC( ' + AEmail + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_AddBCC', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.AddBCC(AEmail);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddBody(const eBody: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  Body: TStringList;
begin
  try
    try
      VerificarLibInicializada;
      Body := TStringList.Create;
      Body.Text := String(eBody);

      if pLib.Config.Log.Nivel > logNormal then
        pLib.GravarLog('MAIL_AddBody( ' + Body.Text + ' )', logCompleto, True)
      else
        pLib.GravarLog('MAIL_AddBody', logNormal);

      with TACBrLibMail(pLib) do
      begin
        MailDM.Travar;
        try
          MailDM.ACBrMail1.Body.Assign(Body);
          Result := SetRetorno(ErrOK);
        finally
          MailDM.Destravar;
        end;
      end;
    except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
    end;
  finally
    Body.Free;
  end;
end;

function MAIL_AddAltBody(const eAltBody: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AltBody: TStringList;
begin
  try
    try
      VerificarLibInicializada;
      AltBody := TStringList.Create;
      AltBody.Text := String(eAltBody);

      if pLib.Config.Log.Nivel > logNormal then
        pLib.GravarLog('MAIL_AddAltBody( ' + AltBody.Text + ' )', logCompleto, True)
      else
        pLib.GravarLog('MAIL_AddAltBody', logNormal);

      with TACBrLibMail(pLib) do
      begin
        MailDM.Travar;
        try
          MailDM.ACBrMail1.AltBody.Assign(AltBody);
          Result := SetRetorno(ErrOK);
        finally
          MailDM.Destravar;
        end;
      end;
    except
      on E: EACBrLibException do
        Result := SetRetorno(E.Erro, E.Message);

      on E: Exception do
        Result := SetRetorno(ErrExecutandoMetodo, E.Message);
    end;
  finally
    AltBody.Free;
  end;
end;

function MAIL_ClearAttachment: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('MAIL_ClearAttachment', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.ClearAttachments;
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_AddAttachment(const eFileName, eDescription: PChar;
  const aDisposition: Integer): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AFileName: AnsiString;
  ADescription: AnsiString;
begin
  try
    VerificarLibInicializada;
    AFileName := AnsiString(eFileName);
    ADescription := AnsiString(eDescription);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_AddAttachment( ' + AFileName + ',' + ADescription + ',' +
             IntToStr(aDisposition) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_AddAttachment', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.AddAttachment(AFileName, ADescription, TMailAttachmentDisposition(aDisposition));
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_SaveToFile(const eFileName: PChar): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
var
  AFileName: AnsiString;
begin
  try
    VerificarLibInicializada;
    AFileName := AnsiString(eFileName);

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_MailProcess( ' + AFileName + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_MailProcess', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.SaveToFile(AFileName);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_GetMail: Pointer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;
    pLib.GravarLog('MAIL_GetMail', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        Result := MailDM.ACBrMail1;
        with TACBrMail(Result) do
          pLib.GravarLog('  '+ClassName+', '+Name, logParanoico);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
    begin
      SetRetorno(E.Erro, E.Message);
      Result := Nil;
    end;

    on E: Exception do
    begin
      SetRetorno(ErrExecutandoMetodo, E.Message);
      Result := Nil;
    end;
  end;
end;

{%endregion}

{%region Envio}
function MAIL_Clear: longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    pLib.GravarLog('MAIL_Clear', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.Clear;
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;

function MAIL_Send(UseThreadNow: Boolean): longint;{$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
begin
  try
    VerificarLibInicializada;

    if pLib.Config.Log.Nivel > logNormal then
      pLib.GravarLog('MAIL_Send( ' + BoolToStr(UseThreadNow, True) + ' )', logCompleto, True)
    else
      pLib.GravarLog('MAIL_Send', logNormal);

    with TACBrLibMail(pLib) do
    begin
      MailDM.Travar;
      try
        MailDM.ACBrMail1.Send(UseThreadNow);
        Result := SetRetorno(ErrOK);
      finally
        MailDM.Destravar;
      end;
    end;
  except
    on E: EACBrLibException do
      Result := SetRetorno(E.Erro, E.Message);

    on E: Exception do
      Result := SetRetorno(ErrExecutandoMetodo, E.Message);
  end;
end;
{%endregion}

{%endregion}

end.

