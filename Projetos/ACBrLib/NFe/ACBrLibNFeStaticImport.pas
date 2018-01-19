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

type
  cint = longint;

{%region Constructor/Destructor}
function NFE_Inicializar(const eArqConfig, eChaveCrypt: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Versao/Retorno}
function NFE_Nome(const sNome: PChar; var esLen: cint): cint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_Versao(const sVersao: PChar; var esLen: cint): cint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_UltimoRetorno(const sMensagem: PChar; var esLen: cint): cint;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function NFE_ConfigLer(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravar(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_ConfigGravarValor(const eSessao, eChave, eValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region NFe}
function NFE_CarregarXMLNFe(const eArquivoOuXML: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_CarregarININFe(const eArquivoOuINI: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function NFE_LimparListaNFEs: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Servicos}
function NFE_StatusServico(const Buffer: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}


implementation

end.

