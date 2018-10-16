////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnAuxiliar;

interface

uses
  SysUtils, Classes,
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  synautil;

type
  TTimeZoneModoDeteccao = (tzSistema, tzPCN, tzManual);

  { TTimeZoneConf }

  TTimeZoneConf = class(TPersistent)
  private
    FModoDeteccao: TTimeZoneModoDeteccao;
    FTimeZoneStr: String;
    procedure SetModoDeteccao(AValue: TTimeZoneModoDeteccao);
    procedure SetTimeZone(AValue: String);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property ModoDeteccao: TTimeZoneModoDeteccao read FModoDeteccao
      write SetModoDeteccao default tzSistema;
    property TimeZoneStr: String read FTimeZoneStr write SetTimeZone;
  end;

function CodigoParaUF(const codigo: integer): string;
function DateTimeTodh(DataHora: TDateTime): string;
function DateTimeToDataHora(DataHora: TDateTime): string;
function ExecutarAjusteTagNro(Corrigir: boolean; Nro: string): string;
function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String; RetirarAcentos: boolean = True; SubstituirQuebrasLinha: Boolean = True; const QuebraLinha: String = ';'): String;
function IIf(const condicao: Boolean; const Verdadeiro, Falso: Variant): Variant;
function IntToStrZero(const Numero: integer; const tamanho: integer): string;
function GerarDigito(out Digito: integer; chave: string): boolean;
function ReverterFiltroTextoXML(aTexto: String): String;
function UFparaCodigo(const UF: string): integer;
function ValidarAAMM(const AAMM: string): boolean;
function ValidarCListServ(const cListServ: integer): boolean;
function ValidarChave(const chave: string): boolean;
function ValidarCodigoPais(const iPais: integer): smallint;
function ValidarCodigoUF(const Codigo: integer): boolean;
function ValidarCNPJ(const numero: string): boolean;
function ValidarCPF(const numero: string): boolean;
function ValidarMod(const modelo: integer; versao : Real): boolean;
function ValidarMunicipio(const Municipio: integer): boolean;
function ValidarNumeros(const s: string): boolean;
function ValidarUF(const UF: string): boolean;
function ValidarIE(IE, UF: string): boolean;
function ValidarISUF(const ISUF: string): boolean;
function ValidarGTIN(const numero: string): boolean;
function ValidarPrefixoGTIN(const numero: string): boolean;
function SubStrEmSubStr(const SubStr1: string; SubStr2: string): boolean;
function xml4line(texto: String): String;
function RetornarPosEx(const SubStr, S: String; Offset: Cardinal = 1): Integer;
function DateTimeTodhUTC(DataHora: TDateTime; TZD: string): string;
function GetUTC(UF: string; const dataHora: TDateTime): string;
function GetUTCSistema: String;
function IsHorarioDeVerao(const UF: string; const dataHora: TDateTime): Boolean;
function GetTerceiroDomingoDoMes(const ano, mes: Integer): TDateTime;
function GetInicioDoHorarioDeVerao(const ano: Integer): TDateTime;
function GetFimDoHorarioDeVerao(const ano: Integer): TDateTime;
function GetDataDoCarnaval(const ano: Integer): TDateTime;
function GetDataDaPascoa(const ano: Integer): TDateTime;

function ExtrairModeloChaveAcesso(AChave: String): String;
function ExtrairUFChaveAcesso(AChave: String): Integer;
function ExtrairCNPJChaveAcesso(AChave: String): String;
function ExtrairSerieChaveAcesso(AChave: String): Integer;
function ExtrairNumeroChaveAcesso(AChave: String): Integer;
function ExtrairCodigoChaveAcesso(AChave: String): Integer;
function ExtrairTipoEmissaoChaveAcesso(AChave: String): Integer;
function ExtrairDigitoChaveAcesso(AChave: string): Integer;

function TimeZoneConf: TTimeZoneConf;

var
  TimeZoneConfInstance: TTimeZoneConf;

implementation

uses
  DateUtils, ACBrUtil, ACBrValidador;

function CodigoParaUF(const codigo: integer): string;
const
  (**)UFS = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.DF.DF.';
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.90.91.';
begin
  try
    result := copy(UFS, pos('.' + IntToStr(Codigo) + '.', CODIGOS) + 1, 2);
  except
    result := '';
  end;
  if not ValidarCodigoUF(Codigo) then
    result := '';
end;

function DateTimeTodh(DataHora: TDateTime): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wAno, 4) + '-' +
            IntToStrZero(wMes, 2) + '-' +
            IntToStrZero(wDia, 2) + 'T' +
            IntToStrZero(wHor, 2) + ':' +
            IntToStrZero(wMin, 2) + ':' +
            IntToStrZero(wSeg, 2);
end;

function DateTimeToDataHora(DataHora: TDateTime): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wDia, 2) +
            IntToStrZero(wMes, 2) +
            IntToStrZero(wAno, 4) +
            IntToStrZero(wHor, 2) +
            IntToStrZero(wMin, 2) +
            IntToStrZero(wSeg, 2);
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

function FiltrarTextoXML(const RetirarEspacos: boolean; aTexto: String;
  RetirarAcentos: boolean; SubstituirQuebrasLinha: Boolean; const QuebraLinha: String): String;
begin
  if RetirarAcentos then
     aTexto := TiraAcentos(aTexto);

  aTexto := ParseText(aTexto, False );

  if RetirarEspacos then
  begin
    while pos('  ', aTexto) > 0 do
      aTexto := StringReplace(aTexto, '  ', ' ', [rfReplaceAll]);
  end;

  if SubstituirQuebrasLinha then
    aTexto := ChangeLineBreak( aTexto, QuebraLinha);

  Result := Trim(aTexto);
end;

function IIf(const condicao: Boolean; const Verdadeiro, Falso: Variant): Variant;
begin
  if condicao then
    Result := Verdadeiro
  else
    Result := Falso;
end;

function IntToStrZero(const Numero: integer; const tamanho: integer): string;
begin
  result := StringOfChar('0', tamanho) + IntToStr(Numero);
  result := copy(result, length(result) - tamanho + 1, tamanho);
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

function HexToAscii(Texto: string): String;
var i : integer;
   function HexToInt(Hex: string): integer;
   begin
     Result := StrToInt('$' + Hex);
   end;
begin
  result := '';
  for i := 1 to Length(texto) do begin
    if i mod 2 <> 0 then
       result := result + chr(HexToInt(copy(texto,i,2)));
  end;
end;

function ReverterFiltroTextoXML(aTexto: String): String;
var p1,p2:Integer;
    vHex,vStr:String;
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
    vStr:=HexToAscii(vStr);
    aTexto:=StringReplace(aTexto,vHex,vStr,[rfReplaceAll]);
    p1:=Pos('&#x',aTexto);
  end;
  result := Trim(aTexto);
end;

function UFparaCodigo(const UF: string): integer;
const
  (**)UFS = '.AC.AL.AP.AM.BA.CE.DF.ES.GO.MA.MT.MS.MG.PA.PB.PR.PE.PI.RJ.RN.RS.RO.RR.SC.SP.SE.TO.';
  CODIGOS = '.12.27.16.13.29.23.53.32.52.21.51.50.31.15.25.41.26.22.33.24.43.11.14.42.35.28.17.';
begin
  try
    result := StrToInt(copy(CODIGOS, pos('.' + UF + '.', UFS) + 1, 2));
  except
    result := 0;
  end;
  if (not ValidarUF(UF)) or (UF = 'EX') then
    result := 0;
end;

function ValidarChave(const chave: string): boolean;
var
  i: integer;
  aChave: String;
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
      result := ValidarCodigoUF(StrToInt(copy(aChave, 1, 2)));

    if result then
      result := ValidarAAMM(copy(aChave, 3, 4));

    if result then
    begin
      case StrToInt(copy(aChave, 23, 3)) of
        // serie reservada para DFe eCPF emitida por aplicativo da Empresa Emitente
        920..969: result := ValidarCPF(copy(aChave, 10, 11));
      else
        // serie (001-889) reservada para DFe eCNPJ
        result := ValidarCNPJ(copy(aChave, 7, 14));
      end;
    end;
  except
    result := false;
  end;
end;

function ValidarAAMM(const AAMM: string): boolean;
begin
  result := (length(AAMM) = 4);
  if not validarNumeros(AAMM) then
    result := false;
  if (result) and (not (StrToInt(copy(AAMM, 3, 2)) in [01..12])) then
    result := false;
end;

function ValidarCodigoPais(const iPais: integer): smallint;
var
  i, soma: integer;
  sPais: string;
const
  MAXIMO = 4;
  PESO = '432';
  CODIGO = '|0132|7560|0175|0230|0370|0400|0418|0434|0477|0531|0590|0639|0647' +
    '|0655|0698|0728|0736|0779|0809|0817|0833|0850|0876|0884|2291|0906' +
    '|0973|0981|1015|1058|1082|1112|0310|1155|1198|1279|1457|1414|1490' +
    '|1511|1546|1376|1538|7889|1589|1600|1635|5118|7412|1651|1694|1732' +
    '|1775|8885|1830|1872|1902|1937|1961|1988|1953|1996|2321|7838|2356' +
    '|2402|6874|2445|2399|2437|2470|2461|2453|2496|2518|2534|2550|2593' +
    '|8702|2674|2712|1619|2755|2810|2852|2895|2917|2933|2976|3018|3050' +
    '|3093|3131|3174|1504|3379|3255|3298|3344|3310|3417|3450|3514|3557' +
    '|3573|3611|3654|3727|3697|3751|3794|3832|3867|3913|3999|1508|3964' +
    '|4030|4111|4200|4235|4260|4278|4316|4340|4383|4405|4421|4456|4472' +
    '|4499|4502|4525|4553|4588|4618|4642|4677|3595|4723|4740|4766|4774' +
    '|4855|4880|4936|0930|4995|4901|5053|4944|4952|4979|4985|5010|5070' +
    '|5088|5177|5215|5258|5282|5312|5355|5380|5428|5487|5568|5665|5738' +
    '|5754|5800|5452|5762|5860|5894|5932|5991|6033|6114|6076|6238|6254' +
    '|6289|6408|6475|6602|6700|6750|6769|6858|6781|6777|6904|6912|6971' +
    '|7102|7153|6955|7005|7056|7285|7358|7370|7315|7447|7480|7501|7544' +
    '|7595|7641|7676|7706|7722|7765|7803|7919|7820|7951|8001|8109|8052' +
    '|8150|8206|8230|8249|8273|8281|8311|8338|8451|8478|5517|8486|8508' +
    '|8583|8630|8664|8737|8907|6653|';
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

function ValidarGTIN(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarGTIN(numero) = '');
end;

function ValidarPrefixoGTIN(const numero: string): boolean;
begin
  result := (ACBrValidador.ValidarPrefixoGTIN(numero) = '');
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

function ValidarModelo(s: string): boolean;
const
  MODELO = '|01|';
begin
  result := pos('|' + s + '|', MODELO) > 0;
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

function ValidarIE(IE, UF: string): boolean;
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

function SubStrEmSubStr(const SubStr1: string; SubStr2: string): boolean;
var
  s: string;
  i: integer;
begin
  i := 0;
  while (i = 0) and (length(SubStr2) > 0) do
  begin
    SubStr2 := copy(SubStr2, 2, maxInt);
    s := copy(SubStr2, 1, pos('|', SubStr2) - 1);
    SubStr2 := copy(SubStr2, pos('|', SubStr2), maxInt);
    if s <> '' then
      i := i + pos('|' + s, '|' + SubStr1);
  end;
  result := i > 0;
end;

function xml4line(texto: String): String;
var
  xml: TStringList;
  i: integer;
begin
  (* Esta função insere um quebra de linha entre os caracteres >< do xml *)
  (* Usada para facilitar os teste de comparação de arquivos             *)
  Texto := Texto + '<';
  Texto := stringreplace(Texto, #$D#$A, '', [rfReplaceAll]);
  Xml := TStringList.create;
  Result := '';
  while length(texto) > 1 do
  begin
    i := pos('><', Texto);
    Xml.Add(copy(Texto, 1, i));
    Texto := copy(Texto, i + 1, maxInt);
  end;
  Result := Xml.Text;
  Xml.Free;
end;

function RetornarPosEx(const SubStr, S: String; Offset: Cardinal = 1): Integer;
var
  I, X: Integer;
  Len, LenSubStr: Integer;
begin
  if Offset = 1 then
    Result := Pos(SubStr, S)
  else
  begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S) - LenSubStr + 1;
    while I <= Len do
    begin
      if S[I] = SubStr[1] then
      begin
        X := 1;
        while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
          Inc(X);
        if (X = LenSubStr) then
        begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;

function DateTimeTodhUTC(DataHora: TDateTime; TZD: string): string;
var
  wAno, wMes, wDia, wHor, wMin, wSeg, wMil: word;
begin
  DecodeDate(DataHora, wAno, wMes, wDia);
  DecodeTime(DataHora, wHor, wMin, wSeg, wMil);
  Result := IntToStrZero(wAno, 4) + '-' +
            IntToStrZero(wMes, 2) + '-' +
            IntToStrZero(wDia, 2) + 'T' +
            IntToStrZero(wHor, 2) + ':' +
            IntToStrZero(wMin, 2) + ':' +
            IntToStrZero(wSeg, 2) +
            TZD;
end;

function GetUTC(UF: string; const dataHora: TDateTime): string;
const
  UTC5 = '.AC.';
  UTC4 = '.AM.RR.RO.MT.MS.';
  UTC3 = '.AP.PA.MA.PI.TO.GO.CE.RN.PB.PE.AL.SE.BA.MG.ES.RJ.SP.PR.SC.RS.DF.';
var
  HorarioDeVerao: Boolean;
begin
  case TimeZoneConf.ModoDeteccao of
    tzSistema:
        Result := GetUTCSistema;

    tzPCN:
      begin
        if (UF = '90') or (UF = '91') or (UF = '') then
           UF := 'DF';

        HorarioDeVerao := IsHorarioDeVerao(UF, dataHora);
        Result := '-03:00';  // TimeZone de Brasília

        if AnsiPos('.' + UF + '.', UTC4) > 0 then
        begin
          Result := '-04:00';
          if HorarioDeVerao then
            Result := '-03:00';
        end
        else
        if AnsiPos('.' + UF + '.', UTC3) > 0 then
        begin
          Result := '-03:00';
          if HorarioDeVerao then
            Result := '-02:00';
        end
        else
        if AnsiPos('.' + UF + '.', UTC5) > 0 then
        begin
          Result := '-05:00';
        end;
      end;

    tzManual:
      Result := TimeZoneConf.TimeZoneStr;
  end;
end;

function GetUTCSistema: String;
begin
  Result := synautil.TimeZone;
  Insert(':', Result, 4);
end;

function IsHorarioDeVerao(const UF: string; const dataHora: TDateTime): Boolean;
const
  UFHV = '.MT.MS.GO.MG.ES.RJ.SP.PR.SC.RS.DF.';
var
  dia: word;
  mes: word;
  ano: word;
  anoInicio: integer;
  anoFim: integer;
begin
  DecodeDate(dataHora, ano, mes, dia);

  { Mês de inicio do horário de verão: Outubro;
    Mês de fim do horário de verão: Fevereiro;

   * Se a data verificada for de um mês menor que outubro: Ex: 10/08/2010 (Agosto)
       O inicio do horário de verão será OUTUBRO do ano anterior (10/2009);
       O fim do horário de verão será FEVEREIRO do mesmo ano (02/2010);

   * Se a data verificada for de um mês maior ou igual outubro: Ex: 10/11/2010 (Novembro)
       O inicio do horário de verão será OUTUBRO do mesmo ano (10/2010);
       O fim do horário de verão será FEVEREIRO do ano seguinte (02/2011);      }

  anoInicio := ano;
  anoFim := ano;
  if mes < 10 then
    anoInicio := ano - 1
  else
    anoFim := ano + 1;

  Result := False;
  if (GetInicioDoHorarioDeVerao(anoInicio) <= dataHora) and
     (GetFimDoHorarioDeVerao(anoFim) >= dataHora) and
     (AnsiPos(UF, UFHV) > 0) then
    Result := True;
  
end;

function GetInicioDoHorarioDeVerao(const ano: Integer): TDateTime;
begin
  {O inicio do horário de verão é no terceiro domingo do mes de outubro}
  Result := GetTerceiroDomingoDoMes(ano, 10);
end;

function GetTerceiroDomingoDoMes(const ano, mes: Integer): TDateTime;
var
  i: integer;
begin
  {O laço vai até 7 pois até o dia 7 tem que ter passado pelo menos um domingo.
   Achado o primeiro domingo, é somado a ele 14 dias para encontrar o terceiro domingo.}
  result := 0;
  for i := 1 to 7 do begin
    if DayOfWeek(EncodeDate(ano, mes, i)) = 1 then
     begin
       result := EncodeDate(ano, mes, i + 14);
       break;
     end;
  end;
end;

function GetFimDoHorarioDeVerao(const ano: Integer): TDateTime;
var
  domingoCarnaval: TDateTime;
  terceiroDomingoFevereiro: TDateTime;
begin
  domingoCarnaval := getDataDoCarnaval(ano) - 2; //Carnaval é na terça - 2 = Domingo
  terceiroDomingoFevereiro := getTerceiroDomingoDoMes(ano, 2);
  if domingoCarnaval <> terceiroDomingoFevereiro then
    result := terceiroDomingoFevereiro
  else
    result := IncDay(terceiroDomingoFevereiro, 7);
end;

function GetDataDoCarnaval(const ano: Integer): TDateTime;
var
  pascoa: TDateTime;
begin
  pascoa := getDataDaPascoa(ano);
  result := IncDay(pascoa, -47);
end;

function GetDataDaPascoa(const ano: Integer): TDateTime;
var
  x: integer;
  y: integer;
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  dia: word;
  mes: word;
begin
  x := 24;
  y := 5;
  a := ano MOD 19;
  b := ano MOD 4;
  c := ano MOD 7;
  d := (19 * a + x) MOD 30;
  e := (2 * b + 4 * c + 6 * d + y) MOD 7;
  if (d + e) > 9 then
   begin
    dia := (d + e - 9);
    mes := 4;
   end
  else
   begin
    dia := (d + e + 22);
    mes := 3;
   end;
  result :=  EncodeDate(ano, mes, dia);
end;

function ExtrairModeloChaveAcesso(AChave: String): String;
begin
  AChave := OnlyNumber(AChave);
  if ValidarChave(AChave) then
    Result := copy(AChave, 21, 2)
  else
    Result := '';
end;

function ExtrairUFChaveAcesso(AChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);
  Result := StrToIntDef(Copy(AChave,1,2), 0);
end;

function ExtrairCNPJChaveAcesso(AChave: String): String;
begin
  AChave := OnlyNumber(AChave);
  Result := copy(AChave,7,14);
end;

function ExtrairSerieChaveAcesso(AChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(AChave) = '59' then  //SAT
    Result := StrToIntDef(Copy(AChave, 23, 9), 0)
  else
    Result := StrToIntDef(Copy(AChave, 23, 3), 0);
end;

function ExtrairNumeroChaveAcesso(AChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(AChave) = '59' then  //SAT
    Result := StrToIntDef(Copy(AChave, 32, 6), 0)
  else
    Result := StrToIntDef(Copy(AChave, 26, 9), 0);
end;

function ExtrairCodigoChaveAcesso(AChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);
  if ExtrairModeloChaveAcesso(AChave) = '59' then  //SAT
    Result := StrToIntDef(Copy(AChave, 38, 6), 0)
  else
    Result := StrToIntDef(Copy(AChave, 36, 8), 0);
end;

function ExtrairTipoEmissaoChaveAcesso(aChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);

  if (pos(ExtrairModeloChaveAcesso(AChave), '55,65') = 0) then
    Result := 0
  else
    Result := StrToIntDef(Copy(AChave, 35, 1), 0);
end;

function ExtrairDigitoChaveAcesso(AChave: String): Integer;
begin
  AChave := OnlyNumber(AChave);

  Result := StrToInt(AChave[length(AChave)]);
end;

{ TTimeZoneConf }

function TimeZoneConf: TTimeZoneConf;
begin
  if not Assigned(TimeZoneConfInstance) then
    TimeZoneConfInstance := TTimeZoneConf.Create;

  Result := TimeZoneConfInstance;
end;

constructor TTimeZoneConf.Create;
begin
  inherited;

  FTimeZoneStr := '';
  FModoDeteccao := tzSistema;
end;

procedure TTimeZoneConf.Assign(Source: TPersistent);
begin
 if Source is TTimeZoneConf then
 begin
   FModoDeteccao := TTimeZoneConf(Source).ModoDeteccao;
   FTimeZoneStr  := TTimeZoneConf(Source).TimeZoneStr;
 end;
end;

procedure TTimeZoneConf.SetTimeZone(AValue: String);
var
  Hora, Minuto: Integer;
begin
  if FTimeZoneStr = AValue then Exit;

  if (FModoDeteccao <> tzManual) then
  begin
    FTimeZoneStr := '';
    Exit;
  end;

  if (Trim(AValue) = '') then
  begin
    FTimeZoneStr := GetUTCSistema;
    Exit;
  end;

  if (Length(AValue) <> 6) then
    raise Exception.Create('Tamanho de TimeZone deve ser 6. Ex: -03:00');

  if not CharInSet(AValue[1], ['-','+']) then
    raise Exception.Create('Primeiro caractere deve ser "+,-". Ex: -03:00');

  if not (AValue[4] = ':') then
    raise Exception.Create('Quarto caractere deve ser ":". Ex: -03:00');

  Hora := StrToIntDef(copy(AValue,2,2), -99);
  if ((Hora < -11) or (Hora > 14)) then
    raise Exception.Create('Hora deve estar entre -11 a +14. Ex: -03:00');

  Minuto := StrToIntDef(copy(AValue,5,2), -99);
  if ((Minuto < 0) or (Minuto > 60)) then
    raise Exception.Create('Minuto deve estar entre 0 a 59. Ex: -03:00');

  FTimeZoneStr := AValue;
end;

procedure TTimeZoneConf.SetModoDeteccao(AValue: TTimeZoneModoDeteccao);
begin
  if FModoDeteccao = AValue then Exit;
  FModoDeteccao := AValue;

  if FModoDeteccao <> tzManual then
    FTimeZoneStr := ''
  else
  begin
    if FTimeZoneStr = '' then
      FTimeZoneStr := GetUTCSistema;
  end;
end;

initialization
  TimeZoneConfInstance := nil;

finalization;
  if Assigned( TimeZoneConfInstance ) then
    TimeZoneConfInstance.Free;

end.

