unit ACBrLibMailStaticImport;

{$IfDef FPC}
{$mode objfpc}{$H+}
{$EndIf}

{.$Define STDCALL}

interface

uses
  Classes, SysUtils;

const
 {$IfDef MSWINDOWS}
  {$IfDef CPU64}
  CACBrMailLIBName = 'ACBrMail64.dll';
  {$Else}
  CACBrMailLIBName = 'ACBrMail32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrMailLIBName = 'ACBrMail64.so';
  {$Else}
  CACBrMailLIBName = 'ACBrMail32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function MAIL_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Versao/Retorno}
function MAIL_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Ler/Gravar Config }
function MAIL_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Diversos}
function MAIL_AddAddress(eEmail: PChar; eName: PChar = ''): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddReplyTo(eEmail: PChar; eName: PChar = ''): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddCC(eEmail: PChar; eName: PChar = ''): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddBCC(eEmail: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_ClearAttachment: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddAttachment(eFileName: PChar; eDescription: PChar = '';
            const aDisposition: TMailAttachmentDisposition = adInline): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_AddAttachmentStream(aStream: TStream; eDescription: PChar = '';
            const aDisposition: TMailAttachmentDisposition = adInline): longint;
 {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;

function MAIL_MailProcess(const aStatus: TMailStatus): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_SaveToFile(const eFileName: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_SaveToStream(AStream: TStream): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

{%region Envio}
function MAIL_Clear: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_BuildMimeMess: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
function MAIL_Send(UseThreadNow: Boolean): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrMailLIBName;
{%endregion}

implementation

end.
