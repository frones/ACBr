{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

unit ACBrDFeUtil;

interface

uses
  Classes, StrUtils, SysUtils, synacode, synautil,
  ACBrDFeSSL, ACBrIBGE,
  ACBrDFe;

function FormatarNumeroDocumentoFiscal(AValue: String): String;
function FormatarNumeroDocumentoFiscalNFSe(AValue: String): String;

function GerarCodigoNumerico(numero: integer): integer;
function GerarCodigoDFe(AnDF: Integer; ADigitos: Integer = 8): integer;

function GerarChaveAcesso(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ:String;
                          ASerie, ANumero, AtpEmi, ACodigo: Integer;
                          AModelo: Integer = 55; ASiteAutorizador: Integer = -1): String;
function FormatarChaveAcesso(AValue: String): String;

function ValidaUFCidade(const UF, Cidade: integer): Boolean; overload;
procedure ValidaUFCidade(const UF, Cidade: integer; const AMensagem: String); overload;
function ValidaDIDSI(AValue: String): Boolean;
function ValidaDIRE(const AValue: String): Boolean;
function ValidaRE(const AValue: String): Boolean;
function ValidaDrawback(AValue: String): Boolean;
function ValidaSUFRAMA(AValue: String): Boolean;
function ValidaRECOPI(AValue: String): Boolean;
function ValidaNVE(const AValue: string): Boolean;

function XmlEstaAssinado(const AXML: String): Boolean;
function SignatureElement(const URI: String; AddX509Data: Boolean;
    const IdSignature: String = ''; const Digest: TSSLDgst = dgstSHA1;
    const IdSignatureValue: String = ''): String;
function EncontrarURI(const AXML: String; docElement: String = ''; IdAttr: String = ''): String;
function ObterNomeMunicipio(const AcMun: Integer; var AxUF: String;
  const APathArqMun: String = ''; const AGerarException : Boolean = True): String;
function ObterCodigoMunicipio(const AxMun, AxUF: String;
                              const APathArqMun: String = ''): Integer;
function ObterCodigoUF(const AUF: String; APathArqMun: String = ''): Integer;

function CalcularHashCSRT(const ACSRT, AChave: String): string;
function CalcularHashDados(const ADados: TStream; AChave: String): string;
function CalcularHashArquivo(const APathArquivo: String; AChave: String): string;

function ObterDFeXML(const AXML, Grupo, NameSpace: String): String;
function DataHoraTimeZoneModoDeteccao(const AComponente : TACBrDFe): TDateTime;

function GerarDigito(out Digito: integer; chave: string): boolean;

function ValidarAAMM(const AAMM: string): boolean;
function ValidarCListServ(const cListServ: integer): boolean;
function ValidarChave(const chave: string): boolean;
function ValidarCodigoPais(const iPais: integer): smallint;
function ValidarCodigoUF(const Codigo: integer): boolean;
function ValidarCNPJ(const numero: string): boolean;
function ValidarCPF(const numero: string): boolean;
function ValidarCNPJouCPF(const numero: string): boolean;
function ValidarMod(const modelo: integer; versao : Real): boolean;
function ValidarMunicipio(const Municipio: integer): boolean;
function ValidarNumeros(const s: string): boolean;
function ValidarUF(const UF: string): boolean;
function ValidarIE(const IE, UF: string): boolean;
function ValidarISUF(const ISUF: string): boolean;
function ValidarGTIN(const numero: string): boolean;
function ValidarPrefixoGTIN(const numero: string): boolean;
function ValidarCodigoDFe(AcDF, AnDF: Integer; ADigitos: Integer = 8): Boolean;
function ValidarProtocolo(const AProtocolo: string): Boolean;
function ValidarRecibo(const ARecibo: string): Boolean;

function ExtrairModeloChaveAcesso(const AChave: string): string;
function ExtrairUFChaveAcesso(const AChave: string): Integer;
function ExtrairCNPJChaveAcesso(const AChave: string): string; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função ExtrairCNPJCPFChaveAcesso'{$EndIf};
function ExtrairCNPJCPFChaveAcesso(const AChave: string): string;
function ExtrairSerieChaveAcesso(const AChave: string): Integer;
function ExtrairNumeroChaveAcesso(const AChave: string): Integer;
function ExtrairCodigoChaveAcesso(const AChave: string): Integer;
function ExtrairTipoEmissaoChaveAcesso(const AChave: string): Integer;
function ExtrairDigitoChaveAcesso(const AChave: string): Integer;
function ExtrairChaveMsg(const AMsg: string): string;
function ExtrairProtocoloMsg(const AMsg: string): string;
function ExtrairReciboMsg(const AMsg: string): string;

function ExecutarAjusteTagNro(Corrigir: boolean; Nro: string): string;

var
  ACBrIBGE1: TACBrIBGE;

implementation

uses
  Variants, DateUtils,
  ACBrDFeException,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrUtil.Compatibilidade,
  ACBrValidador;

function FormatarNumeroDocumentoFiscal(AValue: String): String;
begin
  AValue := ACBrUtil.Strings.Poem_Zeros(AValue, 9);
  Result := copy(AValue, 1, 3) + '.' + copy(AValue, 4, 3) + '.' + copy(AValue, 7, 3);
end;

function FormatarNumeroDocumentoFiscalNFSe(AValue: String): String;
begin
  AValue := ACBrUtil.Strings.Poem_Zeros(AValue, 15);
  Result := copy(AValue, 1, 4) + '.' + copy(AValue, 5, 12);
end;

function ValidaUFCidade(const UF, Cidade: integer): Boolean;
begin
  Result := (Copy(IntToStr(UF), 1, 2) = Copy(IntToStr(Cidade), 1, 2));
end;

procedure ValidaUFCidade(const UF, Cidade: integer; const AMensagem: String);
begin
  if not (ValidaUFCidade(UF, Cidade)) then
    raise EACBrDFeException.Create(AMensagem);
end;

function GerarCodigoNumerico(numero: integer): integer;
var
  s: string;
  i, j, k: integer;
begin
  // Essa função gera um código numerico atravéz de calculos realizados sobre o parametro numero
  s := intToStr(numero);
  for i := 1 to 9 do
    s := s + intToStr(numero);
  for i := 1 to 9 do
  begin
    k := 0;
    for j := 1 to 9 do
      k := k + StrToInt(s[j]) * (j + 1);
    s := IntToStr((k mod 11)) + s;
  end;
  Result := StrToInt(copy(s, 1, 8));
end;

function GerarCodigoDFe(AnDF: Integer; ADigitos: Integer = 8): integer;
var
 ACodigo, ARange: Integer;
begin
  Repeat
    ARange := StrToInt(ACBrUtil.Strings.PadRight('9', ADigitos, '9'));
    ACodigo := Random(ARange);
  Until ValidarCodigoDFe(ACodigo, AnDF, ADigitos);

  Result := ACodigo;
end;

function GerarChaveAcesso(AUF: Integer; ADataEmissao: TDateTime; const ACNPJ: String;
   ASerie, ANumero, AtpEmi, ACodigo: Integer; AModelo: Integer;
   ASiteAutorizador: Integer): String;
var
  vUF, vDataEmissao, vSerie, vNumero, vCodigo, vModelo, vCNPJ, vtpEmi,
  vSiteAutorizador: String;
begin
  // Se o usuario informar 0 ou -1; o código numerico sera gerado de maneira aleatória //
  if ACodigo = -1 then
    ACodigo := 0;

  if ACodigo = 0 then
    ACodigo := GerarCodigoDFe(ANumero);

  // Se o usuario informar um código inferior ou igual a -2 a chave será gerada
  // com o código igual a zero, mas poderá não ser autorizada pela SEFAZ.
  if ACodigo <= -2 then
    ACodigo := 0;

  vUF          := ACBrUtil.Strings.Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', AjustarDataHoraParaUf(ADataEmissao, AUF));
  vCNPJ        := PadLeft(OnlyNumber(ACNPJ), 14, '0');
  vModelo      := ACBrUtil.Strings.Poem_Zeros(AModelo, 2);
  vSerie       := ACBrUtil.Strings.Poem_Zeros(ASerie, 3);
  vNumero      := ACBrUtil.Strings.Poem_Zeros(ANumero, 9);
  vtpEmi       := ACBrUtil.Strings.Poem_Zeros(AtpEmi, 1);

  if ASiteAutorizador = -1 then
  begin
    vSiteAutorizador := '';
    vCodigo := ACBrUtil.Strings.Poem_Zeros(ACodigo, 8);
  end
  else
  begin
    vSiteAutorizador := ACBrUtil.Strings.Poem_Zeros(ASiteAutorizador, 1);
    vCodigo := ACBrUtil.Strings.Poem_Zeros(ACodigo, 7);
  end;

  Result := vUF + vDataEmissao + vCNPJ + vModelo + vSerie + vNumero + vtpEmi +
            vSiteAutorizador + vCodigo;
  Result := Result + Modulo11(Result);
end;

function FormatarChaveAcesso(AValue: String): String;
var
  I: Integer;
begin
  AValue := OnlyNumber(AValue);
  I := 1;
  Result := '';
  while I < Length(AValue) do
  begin
    Result := Result+copy(AValue,I,4)+' ';
    Inc( I, 4);
  end;

  Result := Trim(Result);
end;

function ValidaDIDSI(AValue: String): Boolean;
var
  ano: integer;
  sValue: String;
begin
  // AValue = TAANNNNNNND
  // Onde: T Identifica o tipo de documento ( 2 = DI e 4 = DSI )
  //       AA Ano corrente da geração do documento
  //       NNNNNNN Número sequencial dentro do Ano ( 7 ou 8 dígitos )
  //       D Dígito Verificador, Módulo 11, Pesos de 2 a 9
  AValue := OnlyNumber(AValue);
  ano := StrToInt(Copy(IntToStr(YearOf(Date)), 3, 2));

  if (length(AValue) < 11) or (length(AValue) > 12) then
    Result := False
  else if (copy(Avalue, 1, 1) <> '2') and (copy(Avalue, 1, 1) <> '4') then
    Result := False
  else if not ((StrToInt(copy(Avalue, 2, 2)) >= ano - 1) and
    (StrToInt(copy(Avalue, 2, 2)) <= ano + 1)) then
    Result := False
  else
  begin
    sValue := copy(AValue, 1, length(AValue) - 1);
    Result := copy(AValue, length(AValue), 1) = Modulo11(sValue);
  end;
end;

function ValidaDIRE(const AValue: String): Boolean;
var
  AnoData, AnoValue: integer;
begin
  // AValue = AANNNNNNNNNN
  // Onde: AA AnoData corrente da geração do documento
  //       NNNNNNNNNN Número sequencial dentro do AnoData ( 10 dígitos )

  Result := StrIsNumber(AValue) and (Length(AValue) = 12);

  if Result then
  begin
    AnoData  := StrToInt(Copy(IntToStr(YearOf(Date)), 3, 2));
    AnoValue := StrToInt(Copy(AValue, 1, 2));

    Result := (AnoValue >= (AnoData - 1)) and (AnoValue <= (AnoData + 1));
  end;
end;

function ValidaRE(const AValue: String): Boolean;
var
  AnoData, AnoValue, SerieRE: integer;
begin
  // AValue = AANNNNNNNSSS
  // Onde: AA AnoData corrente da geração do documento
  //       NNNNNNN Número sequencial dentro do AnoData ( 7 dígitos )
  //       SSS Serie do RE (001, 002, ...)

  if (AValue = '000000000000') or (AValue = '') then
  begin
    // Deve aceitar doze zeros, pois há casos onde a RE é gerada somente depois
    // http://normas.receita.fazenda.gov.br/sijut2consulta/link.action?idAto=81446&visao=anotado
    // No link acima diz que o DUE substitui o RE.
    Result := True;
  end
  else
  begin
    Result := StrIsNumber(AValue) and (Length(AValue) = 12);

    if Result then
    begin
      AnoData  := StrToInt(Copy(IntToStr(YearOf(Date)), 3, 2));
      AnoValue := StrToInt(Copy(AValue,  1, 2));
      SerieRE  := StrToInt(Copy(AValue, 10, 3));

      Result := ((AnoValue >= (AnoData - 1)) and (AnoValue <= (AnoData + 1))) and
                ((SerieRE >= 1) and (SerieRE <= 999));
    end;
  end;
end;

function ValidaDrawback(AValue: String): Boolean;
var
  ano: integer;
begin
  // AValue = AAAANNNNNND
  // Onde: AAAA Ano corrente do registro
  //       NNNNNN Número sequencial dentro do Ano ( 6 dígitos )
  //       D Dígito Verificador, Módulo 11, Pesos de 2 a 9
  AValue := OnlyNumber(AValue);
  ano := StrToInt(Copy(IntToStr(YearOf(Date)), 3, 2));
  if length(AValue) = 11 then
    AValue := copy(AValue, 3, 9);

  if length(AValue) <> 9 then
    Result := False
  else if not ((StrToInt(copy(Avalue, 1, 2)) >= ano - 2) and
    (StrToInt(copy(Avalue, 1, 2)) <= ano + 2)) then
    Result := False
  else
    Result := copy(AValue, 9, 1) = Modulo11(copy(AValue, 1, 8));
end;

function ValidaSUFRAMA(AValue: String): Boolean;
var
  SS, LL: integer;
begin
  // AValue = SSNNNNLLD
  // Onde: SS Código do setor de atividade da empresa ( 01, 02, 10, 11, 20 e 60 )
  //       NNNN Número sequencial ( 4 dígitos )
  //       LL Código da localidade da Unidade Administrativa da Suframa ( 01 = Manaus, 10 = Boa Vista e 30 = Porto Velho )
  //       D Dígito Verificador, Módulo 11, Pesos de 2 a 9
  AValue := OnlyNumber(AValue);
  if length(AValue) < 9 then
    AValue := '0' + AValue;
  if length(AValue) <> 9 then
    Result := False
  else
  begin
    SS := StrToInt(copy(Avalue, 1, 2));
    LL := StrToInt(copy(Avalue, 7, 2));
    if not (SS in [01, 02, 10, 11, 20, 60]) then
      Result := False
    else if not (LL in [01, 10, 30]) then
      Result := False
    else
      Result := copy(AValue, 9, 1) = Modulo11(copy(AValue, 1, 8));
  end;
end;

function ValidaRECOPI(AValue: String): Boolean;
begin
  // AValue = aaaammddhhmmssffffDD
  // Onde: aaaammdd Ano/Mes/Dia da autorização
  //       hhmmssffff Hora/Minuto/Segundo da autorização com mais 4 digitos da fração de segundo
  //       DD Dígitos Verificadores, Módulo 11, Pesos de 1 a 18 e de 1 a 19
  AValue := OnlyNumber(AValue);
  if length(AValue) <> 20 then
    Result := False
  else if copy(AValue, 19, 1) <> Modulo11(copy(AValue, 1, 18), 1, 18) then
    Result := False
  else
    Result := copy(AValue, 20, 1) = Modulo11(copy(AValue, 1, 19), 1, 19);
end;

function ValidaNVE(const AValue: string): Boolean;
begin
  //TODO: A NVE (Nomenclatura de Valor Aduaneiro e Estatística) é baseada no NCM,
  // mas formada de 2 letras (atributos) e 4 números (especificações). Ex: AA0001
  Result := ( (Length(AValue) = 6) and ( CharIsAlpha(AValue[1]) and
                                         CharIsAlpha(AValue[2]) and
                                         CharIsNum(AValue[3])   and
                                         CharIsNum(AValue[4])   and
                                         CharIsNum(AValue[5])   and
                                         CharIsNum(AValue[6]) ));
end;

function XmlEstaAssinado(const AXML: String): Boolean;
begin
  Result := (pos('<signature', lowercase(AXML)) > 0);
end;

function SignatureElement(const URI: String; AddX509Data: Boolean;
  const IdSignature: String; const Digest: TSSLDgst;
  const IdSignatureValue: String): String;
var
  MethodAlgorithm, DigestAlgorithm: String;
begin
  case Digest of
    dgstSHA256:
      begin
        MethodAlgorithm := 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha256';
        DigestAlgorithm := 'http://www.w3.org/2001/04/xmlenc#sha256';
      end;
    dgstSHA512:
      begin
        MethodAlgorithm := 'http://www.w3.org/2001/04/xmldsig-more#rsa-sha512';
        DigestAlgorithm := 'http://www.w3.org/2001/04/xmlenc#sha512';
      end;
    else
      begin
        MethodAlgorithm := 'http://www.w3.org/2000/09/xmldsig#rsa-sha1';
        DigestAlgorithm := 'http://www.w3.org/2000/09/xmldsig#sha1';
      end;
  end;

  {(*}
  Result :=
  '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"' + IdSignature + '>' +
    '<SignedInfo>' +
      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
      '<SignatureMethod Algorithm="'+MethodAlgorithm+'" />' +
      '<Reference URI="' + IfThen(URI = '', '', '#' + URI) + '">' +
        '<Transforms>' +
          '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
          '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
        '</Transforms>' +
        '<DigestMethod Algorithm="'+DigestAlgorithm+'" />' +
        '<DigestValue></DigestValue>' +
      '</Reference>' +
    '</SignedInfo>' +
    '<SignatureValue' + IdSignatureValue + '></SignatureValue>' +
    '<KeyInfo>' +
    IfThen(AddX509Data,
      '<X509Data>' +
        '<X509Certificate></X509Certificate>'+
      '</X509Data>',
      '')+
    '</KeyInfo>'+
  '</Signature>';
  {*)}
end;

function EncontrarURI(const AXML: String; docElement: String; IdAttr: String
  ): String;
var
  I, J: integer;
begin
  Result := '';
  if (IdAttr = '') then
    IdAttr := 'Id';

  if (docElement <> '') then
    I := Pos('<'+docElement, AXML)
  else
    I := 1;

  I := PosEx(IdAttr+'=', AXML, I);
  if I = 0 then       // XML não tem URI
    Exit;

  I := PosEx('"', AXML, I + 2);
  if I = 0 then
    raise EACBrDFeException.Create('Não encontrei inicio do URI: aspas inicial');

  J := PosEx('"', AXML, I + 1);
  if J = 0 then
    raise EACBrDFeException.Create('Não encontrei inicio do URI: aspas final');

  Result := copy(AXML, I + 1, J - I - 1);
end;

function GetACBrIBGE(const APathArqMun: String): TACBrIBGE;
var
  AfileName, PathArqMun: String;
begin
  if not Assigned(ACBrIBGE1) then
    ACBrIBGE1 := TACBrIBGE.Create(Nil);

  Result := ACBrIBGE1;
  AfileName := ExtractFileName(ACBrIBGE1.CacheArquivo);
  if (AfileName = '') then
    AfileName := 'ACBrIBGE.txt';

  if EstaVazio(APathArqMun) then
    PathArqMun := ApplicationPath
  else
    PathArqMun := APathArqMun;

  Result.CacheArquivo := PathWithDelim(PathArqMun) + AfileName ;
end;

function ObterNomeMunicipio(const AcMun: Integer; var AxUF: String;
  const APathArqMun: String = ''; const AGerarException : Boolean = True): String;
var
  p: String;
begin
  result := '';
  AxUF := '';

  if not ValidarCodigoUF( StrToInt(Copy(IntToStr(AcMun), 1, 2)) ) then
    Exit;

  p := IfEmptyThen(APathArqMun, ApplicationPath);
  if (GetACBrIBGE(p) = Nil) then
    Exit;

  try
    if (ACBrIBGE1.BuscarPorCodigo(AcMun) > 0) then
    begin
      AxUF := ACBrIBGE1.Cidades[0].UF;
      Result := ACBrIBGE1.Cidades[0].Municipio;
    end;
  except on E: Exception do
    begin
      // Exception controlada, exibição somente em tempo de Debug.
      if (AGerarException) then
        raise EACBrDFeException.Create(ACBrStr(E.Message));
    end;
  end;
end;

function ObterCodigoMunicipio(const AxMun, AxUF: String;
  const APathArqMun: String): Integer;
var
  p: String;
begin
  result := 0;
  p := IfEmptyThen(APathArqMun, ApplicationPath);
  if (GetACBrIBGE(p) = Nil) then
    Exit;

  if (ACBrIBGE1.BuscarPorNome(AxMun, AxUF) > 0) then
    Result := ACBrIBGE1.Cidades[0].CodMunicipio;
end;

function ObterCodigoUF(const AUF: String; APathArqMun: String = ''): Integer;
var
  LPath: String;
begin
  Result := 0;
  LPath := IfEmptyThen(APathArqMun, ApplicationPath);
  if(GetACBrIBGE(LPath)= Nil)then
    Exit;

  Result := ACBrIBGE1.UFToCodUF(AUF);

  if(Result < 0)then
    Result := 0;
end;

function CalcularHashCSRT(const ACSRT, AChave: String): string;
begin
  Result := EncodeBase64(SHA1(ACSRT + AChave));
end;

function CalcularHashDados(const ADados: TStream; AChave: String): string;
var
  sAux: AnsiString;
begin
  if (ADados.Size = 0) then
    raise EACBrDFeException.Create('Dados não especificados');

  ADados.Position := 0;
  sAux := ReadStrFromStream(ADados, ADados.Size);
  sAux := EncodeBase64(sAux);

  Result := EncodeBase64(SHA1(AnsiString(AChave) + sAux));
end;

function CalcularHashArquivo(const APathArquivo: String; AChave: String
  ): string;
var
  FS: TFileStream;
begin
  if (APathArquivo = '') then
    raise EACBrDFeException.Create('Path Arquivo não especificados');

  if not FileExists(APathArquivo) then
    raise EACBrDFeException.Create('Arquivo:  '+APathArquivo+'não encontrado');

  FS := TFileStream.Create(APathArquivo, fmOpenRead);
  try
    Result := CalcularHashDados(FS, AChave);
  finally
    FS.Free;
  end;
end;

function ObterDFeXML(const AXML, Grupo, NameSpace: String): String;
var
  DeclaracaoXML: String;
begin
  DeclaracaoXML := ObtemDeclaracaoXML(AXML);

  Result := RetornarConteudoEntre(AXML, '<' + Grupo + ' xmlns', '</' + Grupo + '>');

  if not EstaVazio(Result) then
    Result := '<' + Grupo + ' xmlns' + Result + '</' + Grupo + '>'
  else
  begin
    Result := SeparaDados(AXML, Grupo);
    if not EstaVazio(Result) then
      Result := '<' + Grupo + ' xmlns="' + NameSpace +'">' +
                Result + '</' + Grupo + '>'
  end;

  if not EstaVazio(Result) then
    Result := DeclaracaoXML + Result;
end;

function DataHoraTimeZoneModoDeteccao(const AComponente: TACBrDFe): TDateTime;
var
  Bias: Integer;
  UTC: String;
  DT: TDateTime;
begin
  DT := now;
  UTC := '';

  if (AComponente.Configuracoes.WebServices.TimeZoneConf.ModoDeteccao <> tzSistema) then
  begin
    TimeZoneConf.Assign( AComponente.Configuracoes.WebServices.TimeZoneConf );
    UTC := GetUTC( AComponente.Configuracoes.WebServices.UF, DT);
    Bias := TimeZoneToBias(DateTimeToStr(DT) + UTC );
    Result := IncMinute( DateTimeUniversal( GetUTCSistema , DT ), Bias *(-1)) ;
  end
  else
    Result := DT;

end;

function GerarDigito(out Digito: integer; chave: string): boolean;
var
  i, j: integer;
const
  PESO = '4329876543298765432987654329876543298765432';
begin
  // Manual Integracao Contribuinte v2.02a - Página: 70 //
  chave := OnlyNumber(chave);
  j := 0;
  Digito := 0;
  result := True;
  try
    for i := 1 to 43 do
      j := j + StrToInt(copy(chave, i, 1)) * StrToInt(copy(PESO, i, 1));
    Digito := 11 - (j mod 11);
    if (j mod 11) < 2 then
      Digito := 0;
  except
    result := False;
  end;
  if length(chave) <> 43 then
    result := False;
end;

function ValidarAAMM(const AAMM: string): boolean;
begin
  result := (length(AAMM) = 4);
  if not validarNumeros(AAMM) then
    result := false;
  if (result) and (not (StrToInt(copy(AAMM, 3, 2)) in [01..12])) then
    result := false;
end;

function ValidarCListServ(const cListServ: integer): boolean;
const
  CODIGO = '|101|102|103|104|105|106|107|108|201|302|303|' +
    '|304|305|401|402|403|404|405|406|407|408|409|' +
    '|410|411|412|413|414|415|416|417|418|419|420|' +
    '|421|422|423|501|502|503|504|505|506|507|508|' +
    '|509|601|602|603|604|605|701|702|703|704|705|' +
    '|706|707|708|709|710|711|712|713|716|717|718|' +
    '|719|720|721|722|801|802|901|902|903|' +
    '|1001|1002|1003|1004|1005|1006|1007|1008|1009|1010|1101|' +
    '|1102|1103|1104|1201|1202|1203|1204|1205|1206|1207|1208|' +
    '|1209|1210|1211|1212|1213|1214|1215|1216|1217|1302|1303|' +
    '|1304|1305|1401|1402|1403|1404|1405|1406|1407|1408|1409|' +
    '|1410|1411|1412|1413|1501|1502|1503|1504|1505|1506|1507|' +
    '|1508|1509|1510|1511|1512|1513|1514|1515|1516|1517|1518|' +
    '|1601|1701|1702|1703|1704|1705|1706|1708|1709|1710|1711|' +
    '|1712|1713|1714|1715|1716|1717|1718|1719|1720|1721|1722|' +
    '|1723|1724|1801|1901|2001|2002|2003|2101|2201|2301|2401|' +
    '|2501|2502|2503|2504|2601|2701|2801|2901|3001|3101|3201|' +
    '|3301|3401|3501|3601|3701|3801|3901|4001|';
begin
  result := pos('|' + IntToStr(cListServ) + '|', CODIGO) > 0;
end;

function ValidarChave(const chave: string): boolean;
var
  i: integer;
  aChave: string;
  ACNPJCPF: string;
begin
  result := false;

  aChave := OnlyNumber(chave);

  if length(aChave) <> 44 then
    exit;

  try
    i := 0;
    if GerarDigito(i, copy(aChave, 1, 43)) then
      result := i = StrToInt(aChave[length(aChave)]);

    if result then
    begin
      result := ValidarCodigoUF(StrToInt(copy(aChave, 1, 2)));

      if result then
        result := ValidarAAMM(copy(aChave, 3, 4));

      if result then
      begin
        ACNPJCPF := ExtrairCNPJCPFChaveAcesso(aChave);
        if Length(ACNPJCPF) = 11 then
          result := ValidarCPF(ACNPJCPF)
        else
          result := ValidarCNPJ(ACNPJCPF);
      end;
    end;

  except
    result := false;
  end;
end;

function ValidarCodigoPais(const iPais: integer): smallint;
var
  i, soma: integer;
  sPais: string;
const
  MAXIMO = 4;
  PESO = '432';
  CODIGO = '|9946|0132|7560|0175|0230|0370|0400|0418|0434|0477|0531|0590|0639|0647' +
    '|0655|0698|0728|0736|0779|0809|9950|0817|0833|0850|0876|0884|2291|0906' +
    '|0973|0990|0981|1015|1058|1082|1112|0310|1155|1198|1279|1457|1414|1490' +
    '|1511|1546|1376|1538|7889|1589|1600|1635|5118|7412|1651|1694|1732' +
    '|1775|8885|1830|1872|1902|1937|1961|1988|1953|1996|2003|2321|7838|2356' +
    '|2402|6874|2445|2399|2437|2470|2461|2453|2496|2518|7544|2534|2550|2593' +
    '|8702|2674|2712|1619|2755|2810|2852|2895|2917|2933|2976|3018|3050' +
    '|3093|3131|3174|1504|3379|3255|3298|3344|3310|3417|3450|3514|3557' +
    '|3573|3611|3654|3727|3697|3751|3794|3832|3867|3913|3999|1508|3964' +
    '|4030|4111|4200|4235|4260|4278|4316|4340|4383|4405|4421|4456|4472' +
    '|4499|4502|4525|4553|4588|4618|4642|4677|3595|4723|4740|4766|4774' +
    '|4855|4880|4885|4936|0930|4995|4901|5053|4944|4952|4979|4985|5010|5070' +
    '|5088|5177|5215|5258|5282|5312|5355|5380|5428|5487|5568|9970|5665|5738' +
    '|5754|5780|5800|5452|5762|5860|5894|5932|5991|6033|6114|6076|9903|6238|6254' +
    '|6289|6408|6475|6602|6700|6750|6769|6858|6781|6777|6904|6912|6971' +
    '|7102|7153|6955|6980|6998|0699|7005|7200|7056|7285|7358|7370|7315|7447|7480|7501' +
    '|7595|7600|7641|7676|7706|7722|7765|7803|7919|7820|7951|8001|8109|8052' +
    '|8150|8206|8230|8249|8273|8281|8311|8338|8451|8478|5517|8486|8508' +
    '|8583|8630|8664|8737|8907|6653|8958|';
begin
  // Resultados possiveis:
  //  1 = Validou - O código existia na lista.
  //  0 = Alerta  - O código não estava na lista (mas o digito confere).
  // -1 = Erro    - O código não estava na lista (o digito não confere).
  result := 1;
  sPais := copy('0000' + intToStr(iPais), length(intToStr(iPais)) + 1, 4);
  if pos('|' + sPais + '|', CODIGO) > 0 then
    exit;
  // Verificar o digíto caso o código não estaja na lista
  soma := 0;
  for i := 1 to MAXIMO - 1 do
    soma := soma + StrToInt(copy(sPais, i, 1)) * StrToInt(copy(PESO, i, 1));
  // Se o resto igual = 0 ou 1 o digito deve ser = '0'
  result := 0;
  if ((soma mod 11) < 2) and (sPais[MAXIMO] = '0') then
    exit;
  // Para resto maior que 1
  if IntToStr((11 - (soma mod 11))) <> sPais[MAXIMO] then
    result := -1;
end;

function ValidarCodigoUF(const Codigo: integer): boolean;
const
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.90.91';
begin
  result := pos('.' + IntToStr(Codigo) + '.', CODIGOS) > 0;
end;

function ValidarCNPJ(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarCNPJ(numero) = '');
end;

function ValidarCPF(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarCPF(numero) = '');
end;

function ValidarCNPJouCPF(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarCNPJouCPF(numero) = '');
end;

function ValidarMod(const modelo: integer; versao : Real): boolean;
const
  MODELOS = '|1|';
  MODELOSV4 = '|1|2|';
begin
  if versao < 4 then
    result := pos('|' + intToStr(modelo) + '|', MODELOS) > 0
  else
    result := pos('|' + intToStr(modelo) + '|', MODELOSV4) > 0 ;
end;

function ValidarMunicipio(const Municipio: integer): boolean;
var
  i, Valor, Soma: integer;
  Codigo, Digito: string;
const
  TAMANHO: smallint = 7;
  PESO = '1212120';
  NAO_VALIDAR = '|2201919|2202251|2201988|2611533|3117836|3152131|4305871|5203939|5203962|';
begin
  result := true;
  if Municipio = 9999999 then
    exit;
  Codigo := IntToStr(Municipio);
  if pos('|' + copy(Codigo, 1, 6), NAO_VALIDAR) > 0 then
  begin
    result := pos('|' + Codigo + '|', NAO_VALIDAR) > 0;
    exit;
  end;
  result := false;
  if length(Codigo) <> TAMANHO then
    exit;
  if not ValidarCodigoUF(StrToInt(copy(Codigo, 1, 2))) then
    exit;
  if copy(Codigo, 3, 4) = '0000' then
    exit;
  soma := 0;
  for i := 1 to TAMANHO do
  begin
    valor := StrToInt(copy(IntToStr(Municipio), i, 1)) * StrToInt(copy(PESO, i, 1));
    if valor > 9 then
      soma := soma + StrToInt(copy(IntToStr(valor), 1, 1)) + StrToInt(copy(IntToStr(valor), 2, 1))
    else
      soma := soma + valor;
  end;
  digito := IntToStr((10 - (soma mod 10)));
  if ((soma mod 10) = 0) then
    digito := '0';
  result := (digito = Codigo[TAMANHO]);
end;

function ValidarNumeros(const s: string): boolean;
begin
  result := StrIsNumber(s);
end;

function ValidarUF(const UF: string): boolean;
const
  UFS: string = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.EX.';
begin
  result := pos('.' + UF + '.', UFS) > 0;
end;

function ValidarIE(const IE, UF: string): boolean;
begin
  result := (ACBrValidador.ValidarIE(IE,UF) = '');
end;

function ValidarISUF(const ISUF: string): boolean;
var
  i: integer;
  Soma: integer;
  Digito: integer;
begin
  Result := False;
  if Length(OnlyNumber(ISUF)) < 9 then
    exit;
  Soma := 0;
  for i := 1 to 9 do
    Soma := Soma + StrToInt(ISUF[i]) * (10 - i);
  Digito := 11 - (Soma mod 11);
  if Digito > 9 then
    Digito := 0;
  Result := StrToInt(ISUF[9]) = Digito;
end;

function ValidarGTIN(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarGTIN(numero) = '');
end;

function ValidarPrefixoGTIN(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarPrefixoGTIN(numero) = '');
end;

function ValidarCodigoDFe(AcDF, AnDF: Integer; ADigitos: Integer = 8): Boolean;
const
  CCodigosDFeInvalidos: array[7..8, 0..19] of Integer =
      ((0, 1111111, 2222222,
           3333333, 4444444, 5555555, 6666666, 7777777, 8888888, 9999999,
           1234567, 2345678, 3456789, 4567890, 5678901, 6789012, 7890123,
           8901234, 9012345, 0123456),
       (0, 11111111, 22222222,
           33333333, 44444444, 55555555, 66666666, 77777777, 88888888, 99999999,
           12345678, 23456789, 34567890, 45678901, 56789012, 67890123, 78901234,
           89012345, 90123456, 01234567));
var
  i: Integer;
begin
  Result := (AcDF <> AnDF);
  i := 0;
  while Result and (i < 20) do
  begin
    Result := (AcDF <> CCodigosDFeInvalidos[ADigitos, i]);
    Inc(i);
  end;
end;

function ValidarProtocolo(const AProtocolo: string): Boolean;
var
  cUF, Ano, AnoAtual: Integer;
  Numero: Int64;
begin
  if Length(AProtocolo) <> 15 then
    Result := False
  else
  begin
    cUF := StrToIntDef(Copy(AProtocolo, 2, 2), 0);
    Ano := 2000 + StrToIntDef(Copy(AProtocolo, 4, 2), 0);
    AnoAtual := YearOf(Now);
    Numero := StrToInt64Def(Copy(AProtocolo, 6, 10), 0);

    Result := (Numero > 0) and (Ano > 2000) and (Ano <= AnoAtual) and
              CharInSet(AProtocolo[1] , ['1'..'5', '7'..'9']) and ValidarCodigoUF(cUF);
  end;
end;

function ValidarRecibo(const ARecibo: string): Boolean;
var
  cUF: Integer;
  Numero: Int64;
begin
  if Length(ARecibo) <> 15 then
    Result := False
  else
  begin
    cUF := StrToIntDef(Copy(ARecibo, 1, 2), 0);
    Numero := StrToInt64Def(Copy(ARecibo, 4, 12), 0);

    Result := (Numero > 0) and CharInSet(ARecibo[3] , ['0'..'4']) and ValidarCodigoUF(cUF);
  end;
end;

function ExtrairModeloChaveAcesso(const AChave: string): string;
begin
  Result := Copy(AChave, 21, 2)
end;

function ExtrairUFChaveAcesso(const AChave: string): Integer;
begin
  Result := StrToIntDef(Copy(OnlyNumber(AChave),1,2), 0);
end;

function ExtrairCNPJChaveAcesso(const AChave: string): string;
begin
  Result := ExtrairCNPJCPFChaveAcesso(AChave);
end;

function ExtrairCNPJCPFChaveAcesso(const AChave: string): string;
var
  AModelo: string;
  ASerie: Integer;
  ATpEmis: Integer;
  AIndEmisNFF: Integer;
begin
  AModelo := ExtrairModeloChaveAcesso(AChave);
  ASerie := ExtrairSerieChaveAcesso(AChave);
  ATpEmis := ExtrairTipoEmissaoChaveAcesso(AChave);
  AIndEmisNFF := StrToIntDef(Copy(AChave, 30, 1), 0); // Na NFF o 5o dígito do número identifica se o emissor é CPF ou CNPJ
  case StrToIntDef(AModelo, 0) of
    55, 65: begin  // NFe, NFCe
      case ATpEmis of
        3: begin // NFF
          case AindEmisNFF of
            2:  // 2-CPF
              Result := Copy(AChave, 10, 11);
          else  // 1-CNPJ
            Result := Copy(AChave, 7, 14);
          end;
        end;
      else
        case ASerie of
          000..889, // Séries (000-889) reservadas para NF-e eCNPJ emitida por aplicativo da Empresa Emitente
          890..899, // Séries (890-899) reservadas para NFA-e eCNPJ da SEFAZ emitida no Site do Fisco
          900..909: // Séries (900-909) reservadas para NFA-e eCNPJ emitida no Site do Fisco
            Result := Copy(AChave, 7, 14);
          910..919, // Séries (910-919) reservadas para NFA-e eCPF emitida no Site do Fisco
          920..969: // Séries (920-969) reservadas para NF-e eCPF emitida por aplicativo da Empresa Emitente
            Result := Copy(AChave, 10, 11);
        else
          // Outras possíveis Séries futuras, assume CNPJ
          Result := Copy(AChave, 7, 14);
        end;
      end;
    end;
    57: begin
      case ATpEmis of
        3: // NFF
          Result := Copy(AChave, 10, 11);
      else
        Result := Copy(AChave, 7, 14);
      end;
    end;
    58: begin // MDFe
      case ASerie of
        // Séries (920-969) reservadas para MDFe-e emitido por pessoa física com inscrição
        920..969: Result := Copy(aChave, 10, 11);
      else
        Result := Copy(aChave, 7, 14);
      end;
    end;
  else
    // 57-CTe, 5-SAT, 63-BPe, 66-NF3e, 67-CTeOS
    Result := Copy(OnlyNumber(AChave), 7, 14);
  end;
end;

function ExtrairSerieChaveAcesso(const AChave: string): Integer;
var
 VChave: string;
begin
  VChave:= OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(VChave) = '59' then  //SAT
    Result := StrToIntDef(Copy(VChave, 23, 9), 0)
  else
    Result := StrToIntDef(Copy(VChave, 23, 3), 0);
end;

function ExtrairNumeroChaveAcesso(const AChave: string): Integer;
var
 VChave: string;
begin
  VChave:= OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(VChave) = '59' then  //SAT
    Result := StrToIntDef(Copy(VChave, 32, 6), 0)
  else
    Result := StrToIntDef(Copy(VChave, 26, 9), 0);
end;

function ExtrairCodigoChaveAcesso(const AChave: string): Integer;
var
 VChave: string;
 Modelo: Integer;
begin
  VChave:= OnlyNumber(AChave);
  Modelo := StrToInt(ExtrairModeloChaveAcesso(VChave));

  case Modelo of
    59: Result := StrToIntDef(Copy(VChave, 38, 6), 0); // SAT

    62, // NFCom
    66, // NF3-e
    99: Result := StrToIntDef(Copy(VChave, 37, 7), 0); // DC-e
  else
    Result := StrToIntDef(Copy(VChave, 36, 8), 0); // Demais DF-e
  end;
end;

function ExtrairTipoEmissaoChaveAcesso(const AChave: string): Integer;
var
 VChave: string;
begin
  VChave:= OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(VChave) = '59' then  //SAT
    Result := 0
  else
    Result := StrToIntDef(Copy(VChave, 35, 1), 0);
end;

function ExtrairDigitoChaveAcesso(const AChave: string): Integer;
var
 VChave: string;
begin
  VChave:= OnlyNumber(AChave);
  Result := StrToIntDef(VChave[Length(VChave)], 0);
end;

function ExtrairChaveMsg(const AMsg: string): string;
var
  xStr: string;
  i: Integer;
  Encontrado: Boolean;
begin
  xStr := '';
  i := 0;
  Encontrado := False;

  repeat
    Inc(i);

    if CharInSet(AMsg[i] , ['0'..'9']) then
    begin
      xStr := OnlyNumber(Copy(AMsg, i, 44));
      Inc(i, Length(xStr) -1);

      if Length(xStr) = 44 then
        Encontrado := ValidarChave(xStr);
    end;

  until (i >= Length(AMsg)) or Encontrado;

  if not Encontrado then
    Result := ''
  else
    Result := xStr;
end;

function ExtrairProtocoloMsg(const AMsg: string): string;
var
  xStr: string;
  i: Integer;
  Encontrado: Boolean;
begin
  xStr := '';
  i := 0;
  Encontrado := False;

  repeat
    Inc(i);

    if CharInSet(AMsg[i], ['0'..'9']) then
    begin
      xStr := OnlyNumber(Copy(AMsg, i, 15));
      Inc(i, Length(xStr) -1);

      if Length(xStr) = 15 then
        Encontrado := ValidarProtocolo(xStr);
    end;

  until (i >= Length(AMsg)) or Encontrado;

  if not Encontrado then
    Result := ''
  else
    Result := xStr;
end;

function ExtrairReciboMsg(const AMsg: string): string;
var
  xStr: string;
  i: Integer;
  Encontrado: Boolean;
begin
  xStr := '';
  i := 0;
  Encontrado := False;

  repeat
    Inc(i);

    if CharInSet(AMsg[i] , ['0'..'9']) then
    begin
      xStr := OnlyNumber(Copy(AMsg, i, 15));
      Inc(i, Length(xStr) -1);

      if Length(xStr) = 15 then
        Encontrado := ValidarRecibo(xStr);
    end;

  until (i >= Length(AMsg)) or Encontrado;

  if not Encontrado then
    Result := ''
  else
    Result := xStr;
end;

function ExecutarAjusteTagNro(Corrigir: boolean; Nro: string): string;
begin
  Nro := trim(Nro);
  result := Nro;
  if not corrigir then
    exit;
  if (ValidarNumeros(Nro)) and (length(Nro) = 1) then
    Result := '00' + Nro;
  if (ValidarNumeros(Nro)) and (length(Nro) = 2) then
    Result := '0' + Nro;
end;

initialization
  Randomize;
  ACBrIBGE1 := Nil;

finalization;
  if Assigned(ACBrIBGE1) then
    FreeAndNil(ACBrIBGE1);

end.

