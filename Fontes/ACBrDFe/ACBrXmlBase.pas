{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Rafael Teno Dias                               }
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

unit ACBrXmlBase;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument;

type
  TACBrTipoAmbiente = (taProducao, taHomologacao);

  TACBrTagAssinatura = (taSempre, taNunca, taSomenteSeAssinada,
                        taSomenteParaNaoAssinada);

  TACBrTipoCampo = (tcStr, tcInt, tcDat, tcDatHor, tcEsp, tcDe2, tcDe3, tcDe4,
                    tcDe6, tcDe8, tcDe10, tcHor, tcDatCFe, tcHorCFe, tcDatVcto,
                    tcDatHorCFe, tcBool, tcStrOrig, tcNumStr);

const
  LineBreak = #13#10;

function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String; RetirarAcentos: boolean = True;
                         SubstituirQuebrasLinha: Boolean = True; const QuebraLinha: String = ';'): String;

function StrToEnumerado(out ok: boolean; const s: string; const AString: array of string;
  const AEnumerados: array of variant): variant;

function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;

function RemoverIdentacao(const AXML: string): string;
function XmlToStr(const AXML: string): string;
function StrToXml(const AXML: string): string;
function IncluirCDATA(const aXML: string): string;
function RemoverCDATA(const aXML: string): string;
function ConverterUnicode(const aXML: string): string;
function TratarXmlRetorno(const aXML: string): string;
function RemoverPrefixos(const aXML: string; APrefixo: array of string): string;
function RemoverPrefixosDesnecessarios(const aXML: string): string;

function ProcessarConteudoXml(const ANode: TACBrXmlNode; const Tipo: TACBrTipoCampo): variant;

//procedure ApplyNamespacePrefix(const ANode: TACBrXmlNode; nsPrefix: string; excludeElements: array of string);

implementation

uses
  StrUtilsEx, ACBrUtil;

function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String;
  RetirarAcentos: boolean; SubstituirQuebrasLinha: Boolean; const QuebraLinha: String): String;
begin
  if RetirarAcentos then
     aTexto := TiraAcentos(aTexto);

  aTexto := ParseText(AnsiString(aTexto), False );

  if RetirarEspacos then
  begin
    while pos('  ', aTexto) > 0 do
      aTexto := FaststringReplace(aTexto, '  ', ' ', [rfReplaceAll]);
  end;

  if SubstituirQuebrasLinha then
    aTexto := ChangeLineBreak( aTexto, QuebraLinha);

  Result := Trim(aTexto);
end;

function StrToEnumerado(out ok: boolean; const s: string; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: integer;
begin
  result := -1;
  for i := Low(AString) to High(AString) do
    if AnsiSameText(s, AString[i]) then
      result := AEnumerados[i];
  ok := result <> -1;
  if not ok then
    result := AEnumerados[0];
end;

function EnumeradoToStr(const t: variant; const AString:
  array of string; const AEnumerados: array of variant): variant;
var
  i: integer;
begin
  result := '';
  for i := Low(AEnumerados) to High(AEnumerados) do
    if t = AEnumerados[i] then
      result := AString[i];
end;

function RemoverIdentacao(const AXML: string): string;
var
  XMLe, XMLs: string;
begin
  XMLe := AXML;
  XMLs := '';

  while XMLe <> XMLs do
  begin
    if XMLs <> '' then
      XMLe := XMLs;

    XMLs := FaststringReplace(XMLe, ' <', '<', [rfReplaceAll]);
    XMLs := FaststringReplace(XMLs, #13 + '<', '<', [rfReplaceAll]);
    XMLs := FaststringReplace(XMLs, '> ', '>', [rfReplaceAll]);
    XMLs := FaststringReplace(XMLs, '>' + #13, '>', [rfReplaceAll]);
  end;

  Result := XMLs;
end;

function XmlToStr(const AXML: string): string;
begin
  Result := FaststringReplace(AXML, '<', '&lt;', [rfReplaceAll]);
  Result := FaststringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function StrToXml(const AXML: string): string;
begin
  Result := FaststringReplace(AXML, '&lt;', '<', [rfReplaceAll]);
  Result := FaststringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := FaststringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := FaststringReplace(Result, ''#$A'', '', [rfReplaceAll]);
  Result := FaststringReplace(Result, ''#$A#$A'', '', [rfReplaceAll]);
end;

function IncluirCDATA(const aXML: string): string;
begin
  Result := '<![CDATA[' + aXML + ']]>';
end;

function RemoverCDATA(const aXML: string): string;
begin
  Result := FaststringReplace(aXML, '<![CDATA[', '', [rfReplaceAll]);
  Result := FaststringReplace(Result, ']]>', '', [rfReplaceAll]);
end;

function ConverterUnicode(const aXML: string): string;
var
  Xml: string;
  p: Integer;
begin
  Xml := aXML;
  p := Pos('&#', Xml);

  while p > 0 do
  begin
    if Xml[p+5] = ';' then
      Xml := FaststringReplace(Xml, Copy(Xml, p, 6), '\' + Copy(Xml, p+2, 3), [rfReplaceAll])
    else
      Xml := FaststringReplace(Xml, Copy(Xml, p, 5), '\' + Copy(Xml, p+2, 3), [rfReplaceAll]);

    p := Pos('&#', Xml);
  end;

  Result := StringToBinaryString(Xml);
end;

function TratarXmlRetorno(const aXML: string): string;
begin
  Result := StrToXml(aXML);
  Result := RemoverCDATA(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
//  Result := ConverterUnicode(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
end;

function RemoverPrefixos(const aXML: string; APrefixo: array of string): string;
var
  i: Integer;
begin
  Result := aXML;

  if (Result = '') or (Length(APrefixo) = 0) then
    exit ;

  for i := Low(APrefixo) to High(APrefixo) do
    Result := FaststringReplace(FaststringReplace(Result, '<' + APrefixo[i], '<', [rfReplaceAll]),
                          '</' + APrefixo[i], '</', [rfReplaceAll]);
end;

function RemoverPrefixosDesnecessarios(const aXML: string): string;
begin
  Result := RemoverPrefixos(aXML, ['ns1:', 'ns2:', 'ns3:', 'ns4:', 'ns5:', 'tc:',
              'ii:', 'p1:']);
end;

function ProcessarConteudoXml(const ANode: TACBrXmlNode; const Tipo: TACBrTipoCampo): variant;
var
  ConteudoTag: string;
begin
  if not Assigned(ANode) or (ANode = nil) then
    ConteudoTag := ''
  else
    ConteudoTag := Trim(ANode.Content);

  case Tipo of
    tcStr,
    tcEsp:
      result := ConteudoTag;

    tcDat:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 1, 4)), StrToInt(copy(ConteudoTag, 6, 2)), StrToInt(copy(ConteudoTag, 9, 2)))
        else
          result := 0;
      end;

    tcDatVcto:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 7, 4)), StrToInt(copy(ConteudoTag, 4, 2)), StrToInt(copy(ConteudoTag, 1, 2)))
        else
          Result := 0;
      end;

    tcDatCFe:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 1, 4)), StrToInt(copy(ConteudoTag, 5, 2)), StrToInt(copy(ConteudoTag, 7, 2)))
        else
          result := 0;
      end;

    tcDatHor:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 1, 4)), StrToInt(copy(ConteudoTag, 6, 2)), StrToInt(copy(ConteudoTag, 9, 2))) +
                    EncodeTime(StrToIntDef(copy(ConteudoTag, 12, 2), 0),
                               StrToIntDef(copy(ConteudoTag, 15, 2), 0),
                               StrToIntDef(copy(ConteudoTag, 18, 2), 0), 0)
        else
          result := 0;
      end;

    tcHor:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 4, 2)), StrToInt(copy(ConteudoTag, 7, 2)), 0)
        else
          result := 0;
      end;

    tcHorCFe:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeTime(StrToInt(copy(ConteudoTag, 1, 2)), StrToInt(copy(ConteudoTag, 3, 2)), StrToInt(copy(ConteudoTag, 5, 2)), 0)
        else
          result := 0;
      end;

    tcDatHorCFe:
      begin
        if length(ConteudoTag) > 0 then
          result := EncodeDate(StrToInt(copy(ConteudoTag, 1, 4)), StrToInt(copy(ConteudoTag, 05, 2)), StrToInt(copy(ConteudoTag, 07, 2))) +
                    EncodeTime(StrToInt(copy(ConteudoTag, 9, 2)), StrToInt(copy(ConteudoTag, 11, 2)), StrToInt(copy(ConteudoTag, 13, 2)), 0)
        else
          result := 0;
      end;

    tcDe2, tcDe3, tcDe4, tcDe6, tcDe8, tcDe10:
      begin
        if length(ConteudoTag) > 0 then
          result := StringToFloatDef(ConteudoTag, 0)
        else
          result := 0;
      end;

    tcInt:
      begin
        if length(ConteudoTag) > 0 then
          result := StrToIntDef(Trim(OnlyNumber(ConteudoTag)), 0)
        else
          result := 0;
      end;

    tcBool:
      begin
        if length(ConteudoTag) > 0 then
          result := StrToBool(ConteudoTag)
        else
          result := False;
      end;

    tcStrOrig:
      begin
        // Falta implementar
        Result := '';
      end;

    tcNumStr:
      begin
        // Falta implementar
        Result := '';
      end

  else
    raise Exception.Create('Node <' + ANode.Name + '> com conteúdo inválido. '+ ConteudoTag);
  end;
end;

procedure ApplyNamespacePrefix(const ANode: TACBrXmlNode; nsPrefix : string; excludeElements: array of string);
var
  i: Integer;

  function StringInArray(const Value: string; aStrings: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Low(aStrings) to High(aStrings) do
      if aStrings[i] = Value then Exit;
    Result := False;
  end;
begin
  if (ANode = nil) then Exit;

  if not StringInArray(ANode.LocalName, excludeElements) then
      ANode.Name :=  nsPrefix + ':' + ANode.Name;

  for i := 0 to ANode.Childrens.Count -1 do
    ApplyNamespacePrefix(ANode.Childrens[i], nsPrefix, excludeElements);
end;

end.
