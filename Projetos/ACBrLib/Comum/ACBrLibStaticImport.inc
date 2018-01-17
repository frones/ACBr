{%region Constructor/Destructor}
function LIB_Inicializar(const eArqConfig, eChaveCrypt: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function LIB_Finalizar: Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Versao/Retorno}
function LIB_NomeEVersao(const sNome, sVersao: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function LIB_UltimoRetorno(const sMensagem: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}

{%region Ler/Gravar Config }
function LIB_ConfigLer(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function LIB_ConfigGravar(const eArqConfig: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function LIB_ConfigLerValor(const eSessao, eChave: PChar; sValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;

function LIB_ConfigGravarValor(const eSessao, eChave, eValor: PChar): Integer;
  {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf}; external CACBrNFeLIBName;
{%endregion}


