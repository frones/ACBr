{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                          }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeUtil;

interface

uses
  Classes, StrUtils, SysUtils,
  IniFiles, ACBrDFeSSL, pcnAuxiliar;

function FormatarNumeroDocumentoFiscal(AValue: String): String;
function FormatarNumeroDocumentoFiscalNFSe(AValue: String): String;
function GerarChaveAcesso(AUF:Integer; ADataEmissao:TDateTime; ACNPJ:String; ASerie:Integer;
                           ANumero,ACodigo: Integer; AModelo:Integer=55): String;
function FormatarChaveAcesso(AValue: String): String;

function ValidaUFCidade(const UF, Cidade: integer): Boolean; overload;
procedure ValidaUFCidade(const UF, Cidade: integer; const AMensagem: String); overload;
function ValidaDIDSI(AValue: String): Boolean;
function ValidaDIRE(AValue: String): Boolean;
function ValidaRE(AValue: String): Boolean;
function ValidaDrawback(AValue: String): Boolean;
function ValidaSUFRAMA(AValue: String): Boolean;
function ValidaRECOPI(AValue: String): Boolean;
function ValidaNVE(AValue: string): Boolean;

function XmlEstaAssinado(const AXML: String): Boolean;
function SignatureElement(const URI: String; AddX509Data: Boolean;
    IdSignature: String = ''; const Digest: TSSLDgst = dgstSHA1): String;
function ExtraiURI(const AXML: String; IdAttr: String = ''): String;
function ObterNomeMunicipio(const AxUF: String; const AcMun: Integer;
                              const APathArqMun: String): String;
function ObterCodigoMunicipio(const AxMun, AxUF, APathArqMun: String ): Integer;

procedure LerIniArquivoOuString(const IniArquivoOuString: AnsiString; AMemIni: TMemIniFile);

implementation

uses
  Variants, DateUtils,
  ACBrDFeException, ACBrConsts, ACBrUtil, ACBrValidador ;

function FormatarNumeroDocumentoFiscal(AValue: String): String;
begin
  AValue := Poem_Zeros(AValue, 9);
  Result := copy(AValue, 1, 3) + '.' + copy(AValue, 4, 3) + '.' + copy(AValue, 7, 3);
end;

function FormatarNumeroDocumentoFiscalNFSe(AValue: String): String;
begin
  AValue := Poem_Zeros(AValue, 15);
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

function GerarChaveAcesso(AUF: Integer; ADataEmissao: TDateTime; ACNPJ: String;
  ASerie: Integer; ANumero, ACodigo: Integer; AModelo: Integer): String;
var
  vUF, vDataEmissao, vSerie, vNumero, vCodigo, vModelo: String;
begin
  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vModelo      := Poem_Zeros(AModelo, 2);
  vSerie       := Poem_Zeros(ASerie, 3);
  vNumero      := Poem_Zeros(ANumero, 9);
  vCodigo      := Poem_Zeros(ACodigo, 9);

  Result := vUF + vDataEmissao + ACNPJ + vModelo + vSerie + vNumero + vCodigo;
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

function ValidaDIRE(AValue: String): Boolean;
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

function ValidaRE(AValue: String): Boolean;
var
  AnoData, AnoValue, SerieRE: integer;
begin
  // AValue = AANNNNNNNSSS
  // Onde: AA AnoData corrente da geração do documento
  //       NNNNNNN Número sequencial dentro do AnoData ( 7 dígitos )
  //       SSS Serie do RE (001, 002, ...)

  Result := StrIsNumber(AValue) and (Length(AValue) = 12);

  if Result then
  begin
    AnoData  := StrToInt(Copy(IntToStr(YearOf(Date)), 3, 2));
    AnoValue := StrToInt(Copy(AValue, 1, 2));
    SerieRE  := StrToInt(Copy(AValue,10, 3));

    Result := ((AnoValue >= (AnoData - 1)) and (AnoValue <= (AnoData + 1))) and
              ((SerieRE >= 1) and (SerieRE <= 999));
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

function ValidaNVE(AValue: string): Boolean;
begin
  //TODO:
  Result := True;
end;

function XmlEstaAssinado(const AXML: String): Boolean;
begin
  Result := (pos('<signature', lowercase(AXML)) > 0);
end;

function SignatureElement(const URI: String; AddX509Data: Boolean;
  IdSignature: String; const Digest: TSSLDgst): String;
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
    '<SignatureValue></SignatureValue>' +
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

function ExtraiURI(const AXML: String; IdAttr: String): String;
var
  I, J: integer;
begin
  Result := '';
  if IdAttr = '' then
    IdAttr := 'Id';

  I := PosEx(IdAttr+'=', AXML);
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

function ObterNomeMunicipio(const AxUF: String; const AcMun: Integer;
  const APathArqMun: String): String;
var
  i: Integer;
  PathArqMun, Codigo: String;
  List: TStringList;
begin
  result := '';
  if EstaVazio(APathArqMun) then
    PathArqMun := ApplicationPath + 'MunIBGE'
  else
    PathArqMun := APathArqMun;

  PathArqMun := PathWithDelim(PathArqMun) + 'MunIBGE-UF' +
             InttoStr(UFparaCodigo(AxUF)) + '.txt';

  if FileExists(PathArqMun) then
  begin
    try
      List := TStringList.Create;
      List.LoadFromFile(PathArqMun);
      Codigo := IntToStr(AcMun);
      i := 0;
      while (i < list.count) and (result = '') do
      begin
        if pos(Codigo, List[i]) > 0 then
          result := Trim(StringReplace(list[i], codigo, '', []));
        inc(i);
      end;

    finally
      List.free;
    end;

  end;

end;

function ObterCodigoMunicipio(const AxMun, AxUF, APathArqMun: String): Integer;
var
  i: integer;
  PathArquivo: string;
  List: TstringList;
  UpMun, UpMunList: String;
begin
  result := 0;
  if EstaVazio(APathArqMun) then
    PathArquivo := ApplicationPath + 'MunIBGE'
  else
    PathArquivo := APathArqMun;

  PathArquivo := PathWithDelim(PathArquivo) + 'MunIBGE-UF' +
               InttoStr(UFparaCodigo(AxUF)) + '.txt';

  if FileExists(PathArquivo) then
  begin
    UpMun := UpperCase(TiraAcentos(AxMun));
    List := TstringList.Create;
    try
      List.LoadFromFile(PathArquivo);
      i := 0;
      while (i < list.count) and (result = 0) do
      begin
        UpMunList := UpperCase(TiraAcentos(List[i]));
        if pos(UpMun, UpMunList ) = 9 then
          result := StrToInt(Trim(copy(list[i],1,7)));

        inc(i);
      end;
    finally
      List.free;
    end;

  end;

end;

procedure LerIniArquivoOuString(const IniArquivoOuString: AnsiString;
  AMemIni: TMemIniFile);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if (pos(LF, IniArquivoOuString) = 0) and FilesExists(IniArquivoOuString) then  // É um rquivo válido ?
      SL.LoadFromFile(IniArquivoOuString)
    else
      SL.Text := StringToBinaryString( IniArquivoOuString );

    AMemIni.SetStrings(SL);
  finally
    SL.Free;
  end;
end;

end.
