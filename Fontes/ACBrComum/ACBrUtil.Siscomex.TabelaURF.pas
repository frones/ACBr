{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores no arquivo : Daniel Ostoic                                     }
{                            Victor H Gonzales - Pandaaa                       }
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
//incluido em 23/05/2024

{$I ACBr.inc}

unit ACBrUtil.Siscomex.TabelaURF;

interface

function CodigoURFToUF(const ADI_XLOCDESEMB: string): string;
function DescricaoOrgaoToCodigoURF(const ADescricaoOrgaoJurisdicao: string): string;

implementation

uses
  SysUtils,
  AnsiStrings;


function DescricaoOrgaoToCodigoURF(const ADescricaoOrgaoJurisdicao: string): string;
var
  I: Integer;
const
  TABELA_URF: array[0..1,0..169] of string = (

  //================== Tabela URF despacho na exportação ====================================================

  // Código de Órgão de Jurisdição/URF de despacho
  ('1010200','1010400','1010300','1017700','1010600','1010700','1015600','1010955','1010851','1011100','1010900','1010954','1010951',
  '1015500','1017600','1015400','1010253','1010852','1010953','1010252','0120100','0120200','0130100','0140100','0145352','0150100',
  '0117600','0145100','0145200','0145300','0130151','0145351','0210200','0210251','0210300','0227603','0817900','0230100','0230151',
  '0240100','0250100','0260100','0260151','0260152','0217602','0217600','0227600','0227700','0250151','0217700','0230154','0317800',
  '0330100','0317600','0317700','0327600','0420152','0410251','0420252','0430151','0440151','0415100','0417700','0417800','0520100',
  '0510552','0517602','0510551','0517600','0517700','0610300','0610400','0610800','0610500','0610900','0610600','0615100','0710200',
  '0710300','0710400','0710500','0717700','0717600','0711000','0727600','0717800','0812200','0810200','0810300','0812051','0811200',
  '0811800','0810900','0810700','0812000','0811000','0812400','0810800','0812500','0812300','0817600','0817700','0817800','0927800',
  '0917800','0920400','0910200','0920500','0910300','0910351','0910352','0910400','0910500','0910600','0910652','0920200','0920300',
  '0910651','0925200','0920351','0925251','0927700','0915200','0617700','0217800','0147600','0917900','0927900','0317900','0917500',
  '0147700','0147800','1017800','0417900','0517800','1017900','1017500','0147851','0147852','0217801','0927501','0517801','0417901',
  '0317901','1017801','1017701','0230155','0917501','0927502','1017501','0417902','0320151','1017502','0917502','1017503','0420154',
  '0611200','0240152','0811203','0240151','0317903','0510352','0510353','0610613','0710251','0710252','0812003','1010351','1017504','1017505'),

  // Descrição de Órgão de Jurisdição/URF de despacho
  ('PELOTAS','PASSO FUNDO','SANTA MARIA','PORTO DE RIO GRANDE','CAXIAS DO SUL','NOVO HAMBURGO','SANTANA DO LIVRAMENTO','QUARAI','PORTO MAUA','SANTA CRUZ DO SUL',
  'URUGUAIANA','BARRA DO QUARAI','ITAQUI','CHUI','AEROPORTO SALGADO FILHO - PORTO ALEGRE','PORTO ALEGRE','BAGE','PORTO XAVIER',' SAO BORJA',' JAGUARAO',' GOIANIA',
  'ANAPOLIS',' CUIABA',' CAMPO GRANDE',' PORTO MURTINHO',' PALMAS',' AEROPORTO INTERNACIONAL DE BRASILIA',' MUNDO NOVO',' CORUMBA',' PONTA PORA',' CACERES',' BELA VISTA',
  'SANTAREM',' OBIDOS',' MARABA',' TABATINGA',' SAO PAULO',' RIO BRANCO',' BRASILEIA',' MACAPA',' PORTO VELHO',' BOA VISTA',' PACARAIMA',' BONFIM',' BARCARENA',' PORTO DE BELEM',
  'PORTO DE MANAUS',' AEROPORTO EDUARDO GOMES',' GUAJARA-MIRIM',' AEROPORTO INTERNACIONAL DE BELEM',' ASSIS BRASIL',' PORTO DE PECEM',' TERESINA',' PORTO DE FORTALEZA',
  'AEROPORTO INTERNACIONAL PINTO MARTINS',' PORTO DE SAO LUIS',' PARNAMIRIM',' PETROLINA',' AREIA BRANCA',' CABEDELO',' MACEIO',' IRF/RECIFE',' AEROPORTO GUARARAPES',
  'PORTO DE SUAPE',' ARACAJU',' ILHEUS',' PORTO DE ARATU',' PORTO SEGURO',' PORTO DE SALVADOR',' AEROPORTO INTERNACIONAL DE SALVADOR DEP. LUIS EDUARDO',' GOVERNADOR VALADARES',
  'JUIZ DE FORA',' MONTES CLAROS',' UBERABA',' UBERLANDIA',' VARGINHA',' IRF/BELO HORIZONTE - MG',' NITEROI',' NOVA IGUACU',' CAMPOS DOS GOITACAZES',' VOLTA REDONDA',
  'AEROPORTO INTERNACIONAL DO RIO DE JANEIRO',' PORTO DO RIO DE JANEIRO',' MACAE',' PORTO DE VITORIA',' PORTO DE ITAGUAI',' ARARAQUARA',' ARACATUBA',' BAURU',
  'SAO SEBASTIAO',' LIMEIRA',' MARILIA',' RIBEIRAO PRETO',' SAO JOSE DO RIO PRETO',' SAO JOSE DOS CAMPOS',' SOROCABA',' JUNDIAI',' TAUBATE',' PIRACICABA',' FRANCA',
  'AEROPORTO INTERNACIONAL DE SAO PAULO/GUARULHOS',' AEROPORTO INTERNACIONAL DE VIRACOPOS',' PORTO DE SANTOS',' ITAJAI',' PORTO DE PARANAGUA',' BLUMENAU',
  'LONDRINA',' LAGES',' CASCAVEL',' SANTO ANTONIO DO SUDOESTE',' CAPANEMA',' PONTA GROSSA',' MARINGA',' FOZ DO IGUACU',' SANTA HELENA',' JOINVILLE',' JOACABA',' GUAIRA',
  'FLORIANOPOLIS',' DIONISIO CERQUEIRA',' IMBITUBA',' PORTO DE SAO FRANCISCO DO SUL',' CURITIBA',' ALF - BELO HORIZONTE',' ALF - BELÉM',' ALF - CORUMBÁ',' ALF - CURITIBA',
  'ALF - DIONÍSIO CERQUEIRA',' ALF - FORTALEZA',' ALF - FOZ DO IGUAÇU',' ALF - MUNDO NOVO',' ALF - PONTA PORÃ',' ALF - PORTO ALEGRE',' ALF - RECIFE',' ALF - SALVADOR',
  'ALF - SANTANA DO LIVRAMENTO',' ALF - URUGUAIANA',' ARF - BELA VISTA',' ARF - PORTO MURTINHO',' IRF - AEROPORTO INTERNACIONAL DE BELÉM',
  'IRF - AEROPORTO INTERNACIONAL DE FLORIANÓPOLIS',' IRF - AEROPORTO INTERNACIONAL DE SALVADOR',' IRF - AEROPORTO INTERNACIONAL DOS GUARARAPES',
  'IRF - AEROPORTO INTERNACIONAL PINTO MARTINS',' IRF - AEROPORTO INTERNACIONAL SALGADO FILHO',' IRF - CHUÍ',' IRF - EPITACIOLÂNDIA',' IRF - GUAÍRA',
  'IRF - IMBITUBA',' IRF - ITAQUI',' IRF - PORTO DE SUAPE',' IRF - PORTO DE SÃO LUÍS',' IRF - QUARAÍ',' IRF - SANTA HELENA',' IRF - SÃO BORJA',' IRF NATAL',
  'POCOS DE CALDAS',' SANTANA',' PIRACICABA',' OIAPOQUE',' IRF SAO LUIS',' IRF PORTO SEGURO',' IRF ILHEUS',' DRF POCOS DE CALDAS',' IRF CAMPOS DOS GOYTACAZES',
  'IRF MACAE',' DRF TAUBATE',' IRF SANTANA DO LIVRAMENTO',' IRF PORTO MAUA',' IRF PORTO XAVIER'));

begin

  // Procura pela descrição do órgão
  for I := Low(TABELA_URF[1]) to High(TABELA_URF[1]) do begin
    if AnsiUpperCase(trim(TABELA_URF[1][I]))  =  AnsiUpperCase(trim(ADescricaoOrgaoJurisdicao)) then begin
      // Retorna o código de URF
      Result := TABELA_URF[0][I];
      Break;
    end;
  end;

end;



function CodigoURFToUF(const ADI_XLOCDESEMB: string): string;
var
  LCodigoURF,  LCodigoURF_UF: string;
begin

  // Retorna em banco se não achar a UF.
  Result := '';

  // Procura código do órgão pela descrição do mesmo, já que no ID só traz a descrição.
  LCodigoURF :=  DescricaoOrgaoToCodigoURF(ADI_XLOCDESEMB);

  // copia 3 primeros digitos para identificar a UF.
  LCodigoURF_UF := copy(LCodigoURF,1,3);

  case AnsiIndexStr(LCodigoURF_UF, ['101', '012','013','014','015','011','021','022','081','023','024','025',
                                    '026','031','033','032','042','041','043','044','052','051','061','071',
                                    '072','092','091']) of
    0: Result  := 'RS'; //101
    1: Result  := 'GO'; //012
    2: Result  := 'MT'; //013
    3: Result  := 'MS'; //014
    4: Result  := 'TO'; //015
    5: Result  := 'DF'; //011
    6: Result  := 'PA'; //021
    7: Result  := 'AM'; //022
    8: Result  := 'SP'; //081
    9: Result  := 'AC'; //023
    10: Result := 'AP'; //024
    11: Result := 'RO'; //025
    12: Result := 'RR'; //026
    13: Result := 'CE'; //031
    14: Result := 'PI'; //033
    15: Result := 'MA'; //032
    16: Result := 'RN'; //042
    17: Result := 'PE'; //041
    18: Result := 'PB'; //043
    19: Result := 'AL'; //044
    20: Result := 'SE'; //052
    21: Result := 'BA'; //051
    22: Result := 'MG'; //061
    23: Result := 'RJ'; //071
    24: Result := 'ES'; //072
    25: Result := 'SC'; //092
    26: Result := 'PR'; //091
  end;
end;

end.
