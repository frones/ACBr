{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

{$IFDEF FPC}
 {$IFNDEF NOGUI}
  {$DEFINE USE_LCLIntf}
 {$ENDIF}
{$ENDIF}

unit ACBrUtil.XMLHTML;

interface

Uses
  SysUtils, Math, Classes,
  ACBrBase, ACBrConsts, IniFiles,
  {$IfDef COMPILER6_UP} StrUtils, DateUtils {$Else} ACBrD5, FileCtrl {$EndIf}
  {$IfDef FPC}
    ,dynlibs, LazUTF8, LConvEncoding, LCLType
    {$IfDef USE_LCLIntf} ,LCLIntf {$EndIf}
  {$EndIf}
  {$IfDef MSWINDOWS}
    ,Windows, ShellAPI
  {$Else}
    {$IfNDef FPC}
      {$IfDef ANDROID}
      ,System.IOUtils
      {$EndIf}
      {$IfDef  POSIX}
      ,Posix.Stdlib
      ,Posix.Unistd
      ,Posix.Fcntl
      {$Else}
      ,Libc
      {$EndIf}
    {$Else}
      ,unix, BaseUnix
    {$EndIf}
    {$IfNDef NOGUI}
      {$IfDef FMX}
        ,FMX.Forms
      {$Else}
        ,Forms
      {$EndIf}
    {$EndIf}
  {$EndIf} ;


function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True) : String;

function LerTagXML( const AXML, ATag: String; IgnoreCase: Boolean = True) : String; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Use o método SeparaDados()' {$ENDIF};
function XmlEhUTF8(const AXML: String): Boolean;
function ConverteXMLtoUTF8(const AXML: String): String;
function ConverteXMLtoNativeString(const AXML: String): String;
function ObtemDeclaracaoXML(const AXML: String): String;
function RemoverDeclaracaoXML(const AXML: String; aTodas: Boolean = False): String;
function InserirDeclaracaoXMLSeNecessario(const AXML: String;
   const ADeclaracao: String = CUTF8DeclaracaoXML): String;

function SeparaDados(const AString: String; const Chave: String; const MantemChave : Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String;
function SeparaDadosArray(const AArray: Array of String; const AString: String; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True) : String;
procedure EncontrarInicioFinalTag(const aText, ATag: String; var PosIni, PosFim: integer;const PosOffset: integer = 0);
function StripHTML(const AHTMLString : String) : String;
procedure AcharProximaTag(const ABinaryString: AnsiString; const PosIni: Integer; var ATag: AnsiString; var PosTag: Integer);

function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String;
  RetirarAcentos: boolean = True; SubstituirQuebrasLinha: Boolean = True;
  const QuebraLinha: String = ';'): String;
function ReverterFiltroTextoXML(aTexto: String): String;
function xml4line(texto: String): String;

implementation

uses
  ACBrUtil.Compatibilidade, ACBrUtil.Base, ACBrUtil.Strings,
  ACBrUtil.Math,
  StrUtilsEx;

{------------------------------------------------------------------------------
   Realiza o tratamento de uma String recebida de um Serviço Web
   Transforma caracteres HTML Entity em ASCII ou vice versa.
   No caso de decodificação, também transforma o Encoding de UTF8 para a String
   nativa da IDE
 ------------------------------------------------------------------------------}
function ParseText( const Texto : AnsiString; const Decode : Boolean = True;
   const IsUTF8: Boolean = True ) : String;
var
  AStr: String;

  function InternalStringReplace(const S, OldPatern, NewPattern: String ): String;
  begin
    if pos(OldPatern, S) > 0 then
      Result := FastStringReplace(S, OldPatern, ACBrStr(NewPattern), [rfReplaceAll])
    else
      Result := S;
  end;

begin
  if Decode then
  begin
    Astr := DecodeToString( Texto, IsUTF8 ) ;

    Astr := InternalStringReplace(AStr, '&amp;'   , '&');
    AStr := InternalStringReplace(AStr, '&lt;'    , '<');
    AStr := InternalStringReplace(AStr, '&gt;'    , '>');
    AStr := InternalStringReplace(AStr, '&quot;'  , '"');
    AStr := InternalStringReplace(AStr, '&#39;'   , #39);
    AStr := InternalStringReplace(AStr, '&#45;'   , '-');
    AStr := InternalStringReplace(AStr, '&aacute;', 'á');
    AStr := InternalStringReplace(AStr, '&Aacute;', 'Á');
    AStr := InternalStringReplace(AStr, '&acirc;' , 'â');
    AStr := InternalStringReplace(AStr, '&Acirc;' , 'Â');
    AStr := InternalStringReplace(AStr, '&atilde;', 'ã');
    AStr := InternalStringReplace(AStr, '&Atilde;', 'Ã');
    AStr := InternalStringReplace(AStr, '&agrave;', 'à');
    AStr := InternalStringReplace(AStr, '&Agrave;', 'À');
    AStr := InternalStringReplace(AStr, '&eacute;', 'é');
    AStr := InternalStringReplace(AStr, '&Eacute;', 'É');
    AStr := InternalStringReplace(AStr, '&ecirc;' , 'ê');
    AStr := InternalStringReplace(AStr, '&Ecirc;' , 'Ê');
    AStr := InternalStringReplace(AStr, '&iacute;', 'í');
    AStr := InternalStringReplace(AStr, '&Iacute;', 'Í');
    AStr := InternalStringReplace(AStr, '&oacute;', 'ó');
    AStr := InternalStringReplace(AStr, '&Oacute;', 'Ó');
    AStr := InternalStringReplace(AStr, '&otilde;', 'õ');
    AStr := InternalStringReplace(AStr, '&Otilde;', 'Õ');
    AStr := InternalStringReplace(AStr, '&ocirc;' , 'ô');
    AStr := InternalStringReplace(AStr, '&Ocirc;' , 'Ô');
    AStr := InternalStringReplace(AStr, '&uacute;', 'ú');
    AStr := InternalStringReplace(AStr, '&Uacute;', 'Ú');
    AStr := InternalStringReplace(AStr, '&uuml;'  , 'ü');
    AStr := InternalStringReplace(AStr, '&Uuml;'  , 'Ü');
    AStr := InternalStringReplace(AStr, '&ccedil;', 'ç');
    AStr := InternalStringReplace(AStr, '&Ccedil;', 'Ç');
    AStr := InternalStringReplace(AStr, '&apos;'  , '''');
  end
  else
  begin
    AStr := string(Texto);
    AStr := StringReplace(AStr, '&', '&amp;' , [rfReplaceAll]);
    AStr := StringReplace(AStr, '<', '&lt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '>', '&gt;'  , [rfReplaceAll]);
    AStr := StringReplace(AStr, '"', '&quot;', [rfReplaceAll]);
    AStr := StringReplace(AStr, #39, '&#39;' , [rfReplaceAll]);
    AStr := StringReplace(AStr, '''','&apos;', [rfReplaceAll]);
  end;

  Result := AStr;
end;

{------------------------------------------------------------------------------
   Retorna o conteudo de uma Tag dentro de um arquivo XML
 ------------------------------------------------------------------------------}
function LerTagXML(const AXML, ATag: String; IgnoreCase: Boolean): String;
begin
  Result := SeparaDados(AXML, ATag, False, True, IgnoreCase);
end ;

{------------------------------------------------------------------------------
   Retorna True se o XML contêm a TAG de encoding em UTF8, no seu início.
 ------------------------------------------------------------------------------}
function XmlEhUTF8(const AXML: String): Boolean;
var
  XmlStart: String;
  P: Integer;
begin
  XmlStart := LowerCase(LeftStr(AXML, 50));
  P := pos('encoding', XmlStart);
  Result := (P > 0) and (pos('utf-8', XmlStart) > P);
end;

{------------------------------------------------------------------------------
   Se XML não contiver a TAG de encoding em UTF8, no seu início, adiciona a TAG
   e converte o conteudo do mesmo para UTF8 (se necessário, dependendo da IDE)
 ------------------------------------------------------------------------------}
function ConverteXMLtoUTF8(const AXML: String): String;
var
  UTF8Str: AnsiString;
begin
  if not XmlEhUTF8(AXML) then   // Já foi convertido antes ou montado em UTF8 ?
  begin
    UTF8Str := NativeStringToUTF8(AXML);
    Result := CUTF8DeclaracaoXML + String(UTF8Str);
  end
  else
    Result := AXML;
end;

{------------------------------------------------------------------------------
   Retorna a Declaração do XML, Ex: <?xml version="1.0"?>
   http://www.tizag.com/xmlTutorial/xmlprolog.php
 ------------------------------------------------------------------------------}
function ObtemDeclaracaoXML(const AXML: String): String;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := pos('<?', AXML);
  if P1 > 0 then
  begin
    P2 := PosEx('?>', AXML, P1+2);
    if P2 > 0 then
      Result := copy(AXML, P1, P2-P1+2);
  end;
end;

{------------------------------------------------------------------------------
   Retorna XML sem a Declaração, Ex: <?xml version="1.0"?>
 ------------------------------------------------------------------------------}
function RemoverDeclaracaoXML(const AXML: String; aTodas: Boolean = False): String;
var
  DeclaracaoXML: String;
begin
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);

  if DeclaracaoXML <> '' then
  begin
    if aTodas then
      Result := FastStringReplace(AXML, DeclaracaoXML, '', [rfReplaceAll])
    else
      Result := FastStringReplace(AXML, DeclaracaoXML, '', []);
  end
  else
    Result := AXML;
end;

{------------------------------------------------------------------------------
   Insere uma Declaração no XML, caso o mesmo não tenha nenhuma
   Se "ADeclaracao" não for informado, usará '<?xml version="1.0" encoding="UTF-8"?>'
 ------------------------------------------------------------------------------}
function InserirDeclaracaoXMLSeNecessario(const AXML: String;
  const ADeclaracao: String): String;
var
  DeclaracaoXML: String;
begin
 Result := AXML;

 // Verificando se a Declaração informada é válida
  if (LeftStr(ADeclaracao,2) <> '<?') or (RightStr(ADeclaracao,2) <> '?>') then
    Exit;

  DeclaracaoXML := ObtemDeclaracaoXML(AXML);
  if EstaVazio(DeclaracaoXML) then
    Result := ADeclaracao + Result;
end;

{------------------------------------------------------------------------------
   Se XML contiver a TAG de encoding em UTF8, no seu início, remove a TAG
   e converte o conteudo do mesmo para String Nativa da IDE (se necessário, dependendo da IDE)
 ------------------------------------------------------------------------------}
function ConverteXMLtoNativeString(const AXML: String): String;
begin
  if XmlEhUTF8(AXML) then   // Já foi convertido antes ou montado em UTF8 ?
  begin
    Result := UTF8ToNativeString(AnsiString(AXML));
    {$IfNDef FPC}
     Result := RemoverDeclaracaoXML(Result);
    {$EndIf}
  end
  else
    Result := AXML;
end;

function SeparaDados(const AString: String; const Chave: String; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True): String;
var
  PosIni, PosFim: Integer;
  UTexto, UChave: String;
  Prefixo: String;
begin
  Result := '';
  PosFim := 0;
  Prefixo := '';

  if AIgnoreCase then
  begin
    UTexto := AnsiUpperCase(AString);
    UChave := AnsiUpperCase(Chave);
  end
  else
  begin
    UTexto := AString;
    UChave := Chave;
  end;

  PosIni := Pos('<' + UChave, UTexto);
  while (PosIni > 0) and not CharInSet(UTexto[PosIni + Length('<' + UChave)], ['>', ' ']) do
    PosIni := PosEx('<' + UChave, UTexto, PosIni + 1);

  if PosIni > 0 then
  begin
    if MantemChave then
      PosFim := Pos('/' + UChave, UTexto) + length(UChave) + 3
    else
    begin
      PosIni := PosIni + Pos('>', copy(UTexto, PosIni, length(UTexto)));
      PosFim := Pos('/' + UChave + '>', UTexto);
    end;
  end;

  if (PosFim = 0) and PermitePrefixo then
  begin
    PosIni := Pos(':' + Chave, AString);
    if PosIni > 1 then
    begin
      while (PosIni > 1) and (AString[PosIni - 1] <> '<') do
      begin
        Prefixo := AString[PosIni - 1] + Prefixo;
        PosIni := PosIni - 1;
      end;
      Result := SeparaDados(AString, Prefixo + ':' + Chave, MantemChave, False, AIgnoreCase);
    end
  end
  else
    Result := copy(AString, PosIni, PosFim - (PosIni + 1));
end;

function SeparaDadosArray(const AArray: array of String; const AString: String; const MantemChave: Boolean = False;
  const PermitePrefixo: Boolean = True; const AIgnoreCase: Boolean = True): String;
var
  I : Integer;
begin
  Result := '';
 for I:=Low(AArray) to High(AArray) do
 begin
   Result := Trim(SeparaDados(AString,AArray[I], MantemChave, PermitePrefixo, AIgnoreCase));
   if Result <> '' then
      Exit;
 end;
end;

{------------------------------------------------------------------------------
   Retorna a posição inicial e final da Tag do XML
 ------------------------------------------------------------------------------}
procedure EncontrarInicioFinalTag(const aText, ATag: String;
  var PosIni, PosFim: integer; const PosOffset: integer = 0);
begin
  PosFim := 0;
  PosIni := PosEx('<' + ATag + '>', aText, PosOffset);
  if (PosIni > 0) then
  begin
    PosIni := PosIni + Length(ATag) + 1;
    PosFim := PosLast('</' + ATag + '>', aText);
    if PosFim < PosIni then
      PosFim := 0;
  end;
end;

{-----------------------------------------------------------------------------
   Localiza uma Tag dentro de uma String, iniciando a busca em PosIni.
   Se encontrar uma Tag, Retorna a mesma em ATag, e a posição inicial dela em PosTag
 ---------------------------------------------------------------------------- }
procedure AcharProximaTag(const ABinaryString: AnsiString;
  const PosIni: Integer; var ATag: AnsiString; var PosTag: Integer);
var
  PosTagAux, FimTag, LenTag : Integer ;
begin
  ATag   := '';
  PosTag := PosExA( '<', ABinaryString, PosIni);
  if PosTag > 0 then
  begin
    PosTagAux := PosExA( '<', ABinaryString, PosTag + 1);  // Verificando se Tag é inválida
    FimTag    := PosExA( '>', ABinaryString, PosTag + 1);
    if FimTag = 0 then                             // Tag não fechada ?
    begin
      PosTag := 0;
      exit ;
    end ;

    while (PosTagAux > 0) and (PosTagAux < FimTag) do  // Achou duas aberturas Ex: <<e>
    begin
      PosTag    := PosTagAux;
      PosTagAux := PosExA( '<', ABinaryString, PosTag + 1);
    end ;

    LenTag := FimTag - PosTag + 1 ;
    ATag   := LowerCase( copy( ABinaryString, PosTag, LenTag ) );
  end ;
end ;

{-----------------------------------------------------------------------------
   Remove todas as TAGS de HTML de uma String, retornando a String alterada
 ---------------------------------------------------------------------------- }
function StripHTML(const AHTMLString: String): String;
var
  ATag, VHTMLString: AnsiString;
  PosTag, LenTag: Integer;
begin
  VHTMLString := AHTMLString;
  ATag   := '';
  PosTag := 0;

  AcharProximaTag( VHTMLString, 1, ATag, PosTag);
  while ATag <> '' do
  begin
    LenTag := Length( ATag );
    Delete(VHTMLString, PosTag, LenTag);

    ATag := '';
    AcharProximaTag( VHTMLString, PosTag, ATag, PosTag );
  end ;
  Result := VHTMLString;
end;

function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String;
  RetirarAcentos: boolean; SubstituirQuebrasLinha: Boolean; const QuebraLinha: String): String;
begin
  if RetirarAcentos then
     aTexto := TiraAcentos(aTexto);

  aTexto := ParseText(AnsiString(aTexto), False );

  if RetirarEspacos then
  begin
    while pos('  ', aTexto) > 0 do
      aTexto := StringReplace(aTexto, '  ', ' ', [rfReplaceAll]);
  end;

  if SubstituirQuebrasLinha then
    aTexto := ChangeLineBreak( aTexto, QuebraLinha);

  Result := Trim(aTexto);
end;

function ReverterFiltroTextoXML(aTexto: String): String;
var
  p1,p2:Integer;
  vHex,vStr:String;
  vStrResult:AnsiString;
begin
  if Pos('<![CDATA[', aTexto) > 0 then
  begin
    aTexto := StringReplace(aTexto, '<![CDATA[', '', []);
    aTexto := StringReplace(aTexto, ']]>', '', []);
  end
  else
  begin
    aTexto := StringReplace(aTexto, '&amp;', '&', [rfReplaceAll]);
    aTexto := StringReplace(aTexto, '&lt;', '<', [rfReplaceAll]);
    aTexto := StringReplace(aTexto, '&gt;', '>', [rfReplaceAll]);
    aTexto := StringReplace(aTexto, '&quot;', '"', [rfReplaceAll]);
    aTexto := StringReplace(aTexto, '&#39;', #39, [rfReplaceAll]);
    p1:=Pos('&#x',aTexto);
    while p1>0 do begin
      for p2:=p1 to Length(aTexto) do
          if aTexto[p2]=';' then
             break;
      vHex:=Copy(aTexto,p1,p2-p1+1);
      vStr:=StringReplace(vHex,'&#x','',[rfReplaceAll]);
      vStr:=StringReplace(vStr,';','',[rfReplaceAll]);
      if not TryHexToAscii(vStr, vStrResult) then
        vStrResult := AnsiString(vStr);
      aTexto:=StringReplace(aTexto,vHex,String(vStrResult),[rfReplaceAll]);
      p1:=Pos('&#x',aTexto);
    end;
  end;
  result := Trim(aTexto);
end;

{------------------------------------------------------------------------------
   Esta função insere um quebra de linha entre os caracteres >< do xml
   Usada para facilitar os teste de comparação de arquivos
 ------------------------------------------------------------------------------}
function xml4line(texto: String): String;
var
  xml: TStringList;
  i: integer;
begin
  Texto := Texto + '<';
  Texto := stringreplace(Texto, #$D#$A, '', [rfReplaceAll]);
  Xml := TStringList.create;
  try
    Result := '';
    while length(texto) > 1 do
    begin
      i := pos('><', Texto);
      Xml.Add(copy(Texto, 1, i));
      Texto := copy(Texto, i + 1, maxInt);
    end;
    Result := Xml.Text;
  finally
    Xml.Free;
  end;
end;

end.
