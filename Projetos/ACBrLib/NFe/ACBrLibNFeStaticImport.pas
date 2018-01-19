unit ACBrLibNFeStaticImport;

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
  CACBrNFeLIBName = 'ACBrNFe64.dll';
  {$Else}
  CACBrNFeLIBName = 'ACBrNFe32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrNFeLIBName = 'ACBrNFe64.so';
  {$Else}
  CACBrNFeLIBName = 'ACBrNFe32.so';

  {$EndIf}
 {$EndIf}

{$I ACBrLibErros.inc}

{%region Constructor/Destructor}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Finalizar: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFE_Nome(const sNome: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Versao(const sVersao: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_UltimoRetorno(const sMensagem: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFE_ConfigLer(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravar(const eArqConfig: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_LimparListaNFEs: longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Servicos}
function NFE_StatusServico(const sResposta: PChar; var esTamanho: longint): longint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}


implementation

end.
