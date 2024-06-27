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

unit ACBrXmlWriter;

interface

uses
  Classes, SysUtils,
  pcnSignature, ACBrXmlBase,
  ACBrXmlDocument;

type
  { TACBrXmlWriterOptions }
  TACBrXmlWriterOptions = class
  private
    FDecimalChar: Char;
    FSomenteValidar: Boolean;
    FRetirarEspacos: Boolean;
    FRetirarAcentos: Boolean;
    FIdentarXML: Boolean;
    FSuprimirDecimais: Boolean;
    FFormatoAlerta: string;
    FQuebraLinha: string;

  public
    constructor Create;

    property DecimalChar: Char read FDecimalChar write FDecimalChar;
    property SomenteValidar: Boolean read FSomenteValidar write FSomenteValidar default False;
    property RetirarEspacos: Boolean read FRetirarEspacos write FRetirarEspacos default True;
    property RetirarAcentos: Boolean read FRetirarAcentos write FRetirarAcentos default True;
    property IdentarXML: Boolean read FIdentarXML write FIdentarXML default False;
    property SuprimirDecimais: Boolean read FSuprimirDecimais write FSuprimirDecimais default False;
    property FormatoAlerta: string read FFormatoAlerta write FFormatoAlerta;
    property QuebraLinha: string read FQuebraLinha write FQuebraLinha;

  end;

  { TACBrXmlWriter }
  TACBrXmlWriter = class
  private
    FListaDeAlertas: TStringList;

  protected
    FDocument: TACBrXmlDocument;
    FOpcoes: TACBrXmlWriterOptions;
    FPrefixoPadrao: string;

    function AddNodeCNPJCPF(const ID1, ID2: string; CNPJCPF: string;
      obrigatorio: boolean = True; PreencheZeros: boolean = True): TACBrXmlNode;
    function AddNodeCNPJ(const ID: string; CNPJ: string; const cPais: integer;
      obrigatorio: boolean): TACBrXmlNode;
    function AddNodeCPF(const ID: string; CPF: string; const cPais: integer;
      obrigatorio: boolean): TACBrXmlNode;
    function CreateElement(AName: string; ANamespace: string = '';
      APrefixNamespace: string = ''): TACBrXmlNode; virtual;
    function AddNode(const Tipo: TACBrTipoCampo; ID, TAG: string;
      const min, max, ocorrencias: smallint; const valor: variant;
      const Descricao: string = ''; ParseTextoXML: boolean = True;
      Atributo: string = ''): TACBrXmlNode; virtual;
    procedure wAlerta(const ID, TAG, Descricao, Alerta: string);
    function GerarSignature(const Signature: TSignature): TACBrXmlNode;
    function CreateOptions: TACBrXmlWriterOptions; virtual; abstract;

  public
    constructor Create;
    destructor Destroy; override;

    function GerarXml: boolean; virtual; abstract;
    function ObterNomeArquivo: string; overload;

    property Document: TACBrXmlDocument read FDocument;
    property ListaDeAlertas: TStringList read FListaDeAlertas write FListaDeAlertas;
    property PrefixoPadrao: string read FPrefixoPadrao write FPrefixoPadrao;

  end;

implementation

uses
  variants, dateutils,
  ACBrDFeUtil,
  ACBrDFeConsts,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrValidador;

{ TACBrXmlWriterOptions }
constructor TACBrXmlWriterOptions.Create;
begin
  inherited;

  FIdentarXML := False;
  FFormatoAlerta := 'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.'; // Vide comentário em wAlerta
  FRetirarEspacos := True;
  FRetirarAcentos := True;
  FSuprimirDecimais := False;
  FSomenteValidar := False;
  FDecimalChar := '.';
  FQuebraLinha := ';';
end;

{ TACBrXmlWriter }
constructor TACBrXmlWriter.Create;
begin
  FListaDeAlertas := TStringList.Create;
  FOpcoes := CreateOptions;
  FDocument := TACBrXmlDocument.Create();
  FDocument.SaveOptions := [xmlNoEmpty, xmlNoDecl];
end;

destructor TACBrXmlWriter.Destroy;
begin
  FOpcoes.Free;
  FListaDeAlertas.Free;
  if FDocument <> nil then FDocument.Free;
  inherited Destroy;
end;

function TACBrXmlWriter.AddNodeCNPJCPF(const ID1, ID2: string; CNPJCPF: string;
  obrigatorio: boolean = True; PreencheZeros: boolean = True): TACBrXmlNode;
var
  Tamanho: integer;
  Ocorrencia: integer;
begin
  CNPJCPF := OnlyNumber(trim(CNPJCPF));
  Tamanho := length(CNPJCPF);
  Ocorrencia := integer(obrigatorio);

  if (Tamanho <= 11) and (Tamanho > 0) then    // Se Vazio dá preferencia a CNPJ
  begin
    if PreencheZeros and (Tamanho <> 11) then
    begin
      CNPJCPF := PadLeft(CNPJCPF, 11, '0');
      Tamanho := 11;
    end;

    Result := AddNode(tcStr, ID2, 'CPF  ', 0, 11, Ocorrencia, CNPJCPF);
    if ValidarCPF(CNPJCPF) <> '' then
      wAlerta(ID2, 'CPF', 'CPF', ERR_MSG_INVALIDO);
  end
  else
  begin
    if PreencheZeros and (obrigatorio or (Tamanho > 0)) and (Tamanho <> 14) then
    begin
      CNPJCPF := PadLeft(CNPJCPF, 14, '0');
      Tamanho := 14;
    end;

    Result := AddNode(tcStr, ID1, 'CNPJ', 0, 14, Ocorrencia, CNPJCPF);
    if (Tamanho > 0) and (ValidarCNPJ(CNPJCPF) <> '') then
      wAlerta(ID1, 'CNPJ', 'CNPJ', ERR_MSG_INVALIDO);
  end;

  if (not (Tamanho in [0, 11, 14])) then
    wAlerta(ID1 + '-' + ID2, 'CNPJ-CPF', 'CNPJ/CPF', ERR_MSG_INVALIDO);
end;

function TACBrXmlWriter.AddNodeCNPJ(const ID: string; CNPJ: string;
  const cPais: integer; obrigatorio: boolean): TACBrXmlNode;
begin
  if cPais <> 1058 then
  begin
    Result := AddNode(tcStr, ID, 'CNPJ', 00, 00, 1, '');
    exit;
  end;

  CNPJ := OnlyNumber(Trim(CNPJ));

  if obrigatorio then
    Result := AddNode(tcEsp, ID, 'CNPJ', 14, 14, 1, CNPJ, DSC_CNPJ)
  else
    Result := AddNode(tcEsp, ID, 'CNPJ', 14, 14, 0, CNPJ, DSC_CNPJ);

  if ValidarCNPJ(CNPJ) <> '' then
    wAlerta(ID, 'CNPJ', DSC_CNPJ, ERR_MSG_INVALIDO);
end;

function TACBrXmlWriter.AddNodeCPF(const ID: string; CPF: string;
  const cPais: integer; obrigatorio: boolean): TACBrXmlNode;
begin
  if cPais <> 1058 then
  begin
    Result := AddNode(tcStr, ID, 'CPF', 00, 00, 1, '');
    exit;
  end;

  CPF := OnlyNumber(Trim(CPF));

  if obrigatorio then
    Result := AddNode(tcEsp, ID, 'CPF', 11, 11, 1, CPF, DSC_CPF)
  else
    Result := AddNode(tcEsp, ID, 'CPF', 11, 11, 0, CPF, DSC_CPF);

  if ValidarCPF(CPF) <> '' then
    wAlerta(ID, 'CPF', DSC_CPF, ERR_MSG_INVALIDO);
end;

function TACBrXmlWriter.CreateElement(AName: string; ANamespace: string;
  APrefixNamespace: string): TACBrXmlNode;
begin
  if NaoEstaVazio(FPrefixoPadrao) then
    Result := FDocument.CreateElement(FPrefixoPadrao + ':' + AName, ANamespace,  APrefixNamespace)
  else
    Result := FDocument.CreateElement(AName, ANamespace,  APrefixNamespace);
end;

function TACBrXmlWriter.AddNode(const Tipo: TACBrTipoCampo; ID, TAG: string;
  const min, max, ocorrencias: smallint; const valor: variant;
  const Descricao: string = ''; ParseTextoXML: boolean = True;
  Atributo: string = ''): TACBrXmlNode;

  function IsEmptyDate(wAno, wMes, wDia: word): boolean;
  begin
    Result := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
  end;

var
  NumeroDecimais: smallint;
  TamMin, TamMax: integer;
  valorDbl: double;
  Alerta, ConteudoProcessado: string;
  wAno, wMes, wDia, wHor, wMin, wSeg, wMse: word;
  EstaVazio: boolean;
  AttSplit: TSplitResult;
begin
  Result := nil;

  if ocorrencias < 0 then Exit;

  ID := Trim(ID);
  Tag := Trim(TAG);
  Atributo := Trim(Atributo);
  EstaVazio := False;
  NumeroDecimais := 0;
  ConteudoProcessado := '';
  TamMax := max;
  TamMin := min;

  case Tipo of
    tcStr:
    begin
      ConteudoProcessado := Trim(VarToStr(valor));
      EstaVazio := ConteudoProcessado = '';
    end;

    tcNumStr:
    begin
      ConteudoProcessado := Trim(VarToStr(valor));
      EstaVazio := ConteudoProcessado = '';

      if Length(ConteudoProcessado) < TamMin then
        ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
    end;

    tcStrOrig:
    begin
      ConteudoProcessado := VarToStr(valor);
      EstaVazio := ConteudoProcessado = '';
    end;

    tcDat, tcDatCFe:
    begin
      DecodeDate(VarToDateTime(valor), wAno, wMes, wDia);
      ConteudoProcessado := FormatFloat('0000', wAno) + '-' +
        FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);
      if Tipo = tcDatCFe then
        ConteudoProcessado := OnlyNumber(ConteudoProcessado);

      EstaVazio := IsEmptyDate(wAno, wMes, wDia);
    end;

    tcDatVcto:
    begin
      DecodeDate(VarToDateTime(valor), wAno, wMes, wDia);
      ConteudoProcessado := FormatFloat('00', wDia) + '/' +
        FormatFloat('00', wMes) + '/' + FormatFloat('0000', wAno);
      EstaVazio := IsEmptyDate(wAno, wMes, wDia);
    end;

    tcHor, tcHorCFe:
    begin
      DecodeTime(VarToDateTime(valor), wHor, wMin, wSeg, wMse);
      ConteudoProcessado := FormatFloat('00', wHor) + ':' +
        FormatFloat('00', wMin) + ':' + FormatFloat('00', wSeg);
      if Tipo = tcHorCFe then
        ConteudoProcessado := OnlyNumber(ConteudoProcessado);

      EstaVazio := (wHor = 0) and (wMin = 0) and (wSeg = 0);
    end;

    tcDatHor:
    begin
      DecodeDateTime(VarToDateTime(valor), wAno, wMes, wDia, wHor, wMin, wSeg, wMse);
      ConteudoProcessado := FormatFloat('0000', wAno) + '-' +
        FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia) +
        'T' + FormatFloat('00', wHor) + ':' + FormatFloat('00', wMin) +
        ':' + FormatFloat('00', wSeg);
      EstaVazio := ((wAno = 1899) and (wMes = 12) and (wDia = 30));
    end;

    tcDe2, tcDe3, tcDe4, tcDe5, tcDe6, tcDe7, tcDe8, tcDe10:
    begin
      // adicionar um para que o máximo e mínimo não considerem a virgula
      if not FOpcoes.SuprimirDecimais then
      begin
        TamMax := TamMax + 1;
        TamMin := TamMin + 1;
      end;

      // Tipo numerico com decimais
      case Tipo of
        tcDe2: NumeroDecimais := 2;
        tcDe3: NumeroDecimais := 3;
        tcDe4: NumeroDecimais := 4;
        tcDe5: NumeroDecimais := 5;
        tcDe6: NumeroDecimais := 6;
        tcDe7: NumeroDecimais := 7;
        tcDe8: NumeroDecimais := 8;
        tcDe10: NumeroDecimais := 10;
      end;

      try
        valorDbl := valor; // Converte Variant para Double
        ConteudoProcessado :=
          FloatToString(valorDbl, FOpcoes.DecimalChar, FloatMask(NumeroDecimais, False));
      except
        valorDbl := 0;
        ConteudoProcessado := '0.00';
      end;

      EstaVazio := (valorDbl = 0) and (ocorrencias = 0);

      if StrToIntDef(Copy(ConteudoProcessado,
        pos(FOpcoes.DecimalChar, ConteudoProcessado) + NumeroDecimais + 1, 10),
        0) > 0 then
        walerta(ID, Tag, Descricao, ERR_MSG_MAXIMO_DECIMAIS + ' ' +
          IntToStr(NumeroDecimais));

      // Caso não seja um valor fracionário; retira os decimais.
      if FOpcoes.SuprimirDecimais then
        if int(valorDbl) = valorDbl then
          ConteudoProcessado := IntToStr(Round(valorDbl));

      if Length(ConteudoProcessado) < TamMin then
        ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
    end;

    tcEsp:
    begin
      // Tipo String - somente numeros
      ConteudoProcessado := Trim(VarToStrDef(valor, ''));
      EstaVazio := (ConteudoProcessado = '');

      if not StrIsNumber(ConteudoProcessado) then
        walerta(ID, Tag, Descricao, ERR_MSG_INVALIDO);
    end;

    tcInt, tcInt64:
    begin
      // Tipo Inteiro
      try
        if tipo = tcInt then
          ConteudoProcessado := IntToStr(StrToInt(VarToStr(valor)))
        else
          ConteudoProcessado := IntToStr(StrToInt64(VarToStr(valor)));
      except
        ConteudoProcessado := '0';
      end;

      EstaVazio := (ConteudoProcessado = '0') and (ocorrencias = 0);

      if Length(ConteudoProcessado) < TamMin then
        ConteudoProcessado := PadLeft(ConteudoProcessado, TamMin, '0');
    end;

    tcBool:
    begin
      ConteudoProcessado := LowerCase(BoolToStr(valor, True));
      EstaVazio := ConteudoProcessado = '';
    end;
  end;

  Alerta := '';
  //(Existem tags obrigatórias que podem ser nulas ex. cEAN)  if (ocorrencias = 1) and (EstaVazio) then
  if (ocorrencias = 1) and (EstaVazio) and (TamMin > 0) then
    Alerta := ERR_MSG_VAZIO;

  if (length(ConteudoProcessado) < TamMin) and (Alerta = '') and
    (length(ConteudoProcessado) > 1) then
    Alerta := ERR_MSG_MENOR;

  if length(ConteudoProcessado) > TamMax then
    Alerta := ERR_MSG_MAIOR;

  // Grava alerta //
  if (Alerta <> '') and (pos(ERR_MSG_VAZIO, Alerta) = 0) and (not EstaVazio) then
    Alerta := Alerta + ' [' + VarToStr(valor) + ']';

  walerta(ID, TAG, Descricao, alerta);
  // Sai se for apenas para validar //
  if FOpcoes.FSomenteValidar then exit;

  // Grava a tag no arquivo - Quando não existir algum conteúdo
  if ((ocorrencias = 1) and (EstaVazio)) then
  begin
    Result := CreateElement(Tag);
    exit;
  end;

  // Grava a tag no arquivo - Quando existir algum conteúdo
  if ((ocorrencias = 1) or (not EstaVazio)) then
  begin
    Result := CreateElement(Tag);

    if ParseTextoXML then
      Result.Content := FiltrarTextoXML(FOpcoes.RetirarEspacos,
        ConteudoProcessado, FOpcoes.RetirarAcentos, True, FOpcoes.FQuebraLinha)
    else
      Result.Content := ConteudoProcessado;

    if (Atributo <> '') and (Result <> nil) then
    begin
      AttSplit := Split('=', Atributo);
      Result.SetAttribute(Trim(AttSplit[0]), Trim(AttSplit[1]));
    end;
  end;
end;

procedure TACBrXmlWriter.wAlerta(const ID, TAG, Descricao, Alerta: string);
var
  s: string;
begin
  // O Formato da mensagem de erro pode ser alterado pelo usuario alterando-se a property FFormatoAlerta: onde;
  // %TAGNIVEL%  : Representa o Nivel da TAG; ex: <transp><vol><lacres>
  // %TAG%       : Representa a TAG; ex: <nLacre>
  // %ID%        : Representa a ID da TAG; ex X34
  // %MSG%       : Representa a mensagem de alerta
  // %DESCRICAO% : Representa a Descrição da TAG
  s := FOpcoes.FormatoAlerta;
  //s := StringReplace(s, '%TAGNIVEL%', TagNivel, [rfReplaceAll]);
  s := StringReplace(s, '%TAG%', TAG, [rfReplaceAll]);
  s := StringReplace(s, '%ID%', ID, [rfReplaceAll]);
  s := StringReplace(s, '%MSG%', Alerta, [rfReplaceAll]);
  s := StringReplace(s, '%DESCRICAO%', Trim(Descricao), [rfReplaceAll]);
  if Trim(Alerta) <> '' then
    FListaDeAlertas.Add(s);
end;

function TACBrXmlWriter.GerarSignature(const Signature: TSignature): TACBrXmlNode;
var
  xmlNode, xmlNodeAux: TACBrXmlNode;
begin
  Result := FDocument.CreateElement('Signature', 'http://www.w3.org/2000/09/xmldsig#');
  xmlNode := Result.AddChild('SignedInfo');

  xmlNodeAux := xmlNode.AddChild('CanonicalizationMethod');
  xmlNodeAux.SetAttribute('Algorithm', 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315');

  xmlNodeAux := xmlNode.AddChild('SignatureMethod');
  xmlNodeAux.SetAttribute('Algorithm', 'http://www.w3.org/2000/09/xmldsig#rsa-sha1');

  xmlNode := xmlNode.AddChild('Reference');
  xmlNode.SetAttribute('URI', Signature.URI);

  xmlNodeAux := xmlNode.AddChild('Transforms');
  xmlNodeAux.AddChild('Transform ').SetAttribute('Algorithm', 'http://www.w3.org/2000/09/xmldsig#enveloped-signature');
  xmlNodeAux.AddChild('Transform ').SetAttribute('Algorithm', 'http://www.w3.org/TR/2001/REC-xml-c14n-20010315');

  xmlNode.AddChild('DigestMethod').SetAttribute('Algorithm', 'http://www.w3.org/2000/09/xmldsig#sha1');
  xmlNode.AddChild('DigestValue').Content := Signature.DigestValue;

  Result.AddChild('SignatureValue').Content := Signature.SignatureValue;
  xmlNode := Result.AddChild('KeyInfo').AddChild('X509Data');
  xmlNode.AddChild('X509Certificate').Content := Signature.X509Certificate;
end;

function TACBrXmlWriter.ObterNomeArquivo: string;
begin
  Result := '';
end;

end.
