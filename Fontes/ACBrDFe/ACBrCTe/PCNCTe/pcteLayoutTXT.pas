{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit pcteLayoutTXT;

interface

uses
  SysUtils, Classes, pcnConversao;

function CarregarLayoutTXT(const versao: string): AnsiString;

const
  VERSOES_VALIDAS_LAYOUT_TXT = '|1.10|';

implementation

function CarregarLayoutTXT(const versao: string): AnsiString;
var
  Layout: AnsiString;
  procedure LoadLayout(s: string);
  begin
    Layout := Layout + UpperCase(s + #10 + #13);
  end;
begin

  if versao = '1.10' then
  begin
    LoadLayout('<B01>       NOTAFISCAL|1');
    LoadLayout('<B01>     A|1.10|^id^'); //ok
    LoadLayout('<B01>     B|cUF¨|cNF¨|NatOp¨|indPag¨|mod¨|serie¨|nNF¨|dEmi¨|dSaiEnt¨|tpNF¨|cMunFG¨|TpImp¨|TpEmis¨|CDV¨|TpAmb¨|FinNFe¨|ProcEmi¨|VerProc¨'); //ok
    LoadLayout('<B12a>  B13|refNFe¨'); //ok
    LoadLayout('<B14>   B14|cUF¨|AAMM¨|CNPJ¨|Mod¨|serie¨|nNF¨'); //ok
    LoadLayout('<C01>     C|XNome¨|XFant¨|IE¨|IEST¨|IM¨|CNAE¨'); //ok
    LoadLayout('<C01>   C02|CNPJ¨'); //ok
    LoadLayout('<C01>  C02a|CPF¨'); //ok
    LoadLayout('<C05>   C05|XLgr¨|Nro¨|xCpl¨|xBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨'); //ok
    LoadLayout('<D01>     D|CNPJ¨|XOrgao¨|Matr¨|XAgente¨|Fone¨|UF¨|NDAR¨|DEmi¨|VDAR¨|RepEmi¨|DPag¨'); //ok
    LoadLayout('<E01>     E|XNome¨|IE¨|ISUF¨'); //ok
    LoadLayout('<E01>   E02|CNPJ¨'); //ok
    LoadLayout('<E01>   E03|CPF¨'); //ok
    LoadLayout('<E05>   E05|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨'); //ok
    LoadLayout('<F01>     F|CNPJ¨|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨'); //ok
    LoadLayout('<G01>     G|CNPJ¨|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨'); //ok
    LoadLayout('<H01>     H|NItem¨|InfAdProd¨'); //ok
    LoadLayout('<I01>     I|CProd¨|CEAN¨|XProd¨|NCM¨|EXTIPI¨|Genero¨|CFOP¨|UCom¨|QCom¨|VUnCom¨|VProd¨|CEANTrib¨|UTrib¨|QTrib¨|VUnTrib¨|VFrete¨|VSeg¨|VDesc¨'); //ok
    LoadLayout('<I18>   I18|NDI¨|DDI¨|XLocDesemb¨|UFDesemb¨|DDesemb¨|CExportador¨'); //ok
    LoadLayout('<I25>   I25|NAdicao¨|NSeqAdic¨|CFabricante¨|VDescDI¨'); //ok
    LoadLayout('<J01>     J|TpOp¨|Chassi¨|CCor¨|XCor¨|Pot¨|CM3¨|PesoL¨|PesoB¨|NSerie¨|TpComb¨|NMotor¨|CMKG¨|Dist¨|RENAVAM¨|AnoMod¨|AnoFab¨|TpPint¨|TpVeic¨|EspVeic¨|VIN¨|CondVeic¨|CMod¨'); //ok
    LoadLayout('<K01>     K|NLote¨|QLote¨|DFab¨|DVal¨|VPMC¨'); //ok
    LoadLayout('<L00>     L|TpArma¨|NSerie¨|NCano¨|Descr¨'); //ok
    LoadLayout('<L01>   L01|CProdANP¨|CODIF¨|QTemp¨'); //ok
    LoadLayout('<L105> L105|QBCProd¨|VAliqProd¨|VCIDE¨'); //ok
    LoadLayout('<L109> L109|VBCICMS¨|VICMS¨|VBCICMSST¨|VICMSST¨'); //ok
    LoadLayout('<L114> L114|VBCICMSSTDest¨|VICMSSTDest¨'); //ok
    LoadLayout('<L117> L117|VBCICMSSTCons¨|VICMSSTCons¨|UFCons¨'); //ok
    LoadLayout('<M01>     M'); //ok
    LoadLayout('<N01>     N'); //ok
    LoadLayout('<N02>   N02|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N03>   N03|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N04>   N04|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N05>   N05|Orig¨|CST¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N06>   N06|Orig¨|CST¨'); //ok
    LoadLayout('<N07>   N07|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N08>   N08|Orig¨|CST¨|VBCST¨|VICMSST¨'); //ok
    LoadLayout('<N09>   N09|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N10>   N10|Orig¨|CST¨|ModBC¨|VBC¨|PRedBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<O01>     O|ClEnq¨|CNPJProd¨|CSelo¨|QSelo¨|CEnq¨'); //ok
    LoadLayout('<O07>   O07|CST¨|VIPI¨'); //ok
    LoadLayout('<O07>   O10|VBC¨|PIPI¨'); //ok
    LoadLayout('<O07>   O11|QUnid¨|VUnid¨'); //ok
    LoadLayout('<O08>   O08|CST¨'); //ok
    LoadLayout('<P01>     P|VBC¨|VDespAdu¨|VII¨|VIOF¨'); //ok
    LoadLayout('<Q01>     Q'); //ok
    LoadLayout('<Q02>   Q02|CST¨|VBC¨|PPIS¨|VPIS¨'); //ok
    LoadLayout('<Q03>   Q03|CST¨|QBCProd¨|VAliqProd¨|VPIS¨'); //ok
    LoadLayout('<Q04>   Q04|CST¨'); //ok
    LoadLayout('<Q05>   Q05|CST¨|VPIS¨'); //ok
    LoadLayout('<Q05>   Q07|VBC¨|PPIS¨'); //ok
    LoadLayout('<Q05>   Q10|QBCProd¨|VAliqProd¨'); //ok
    LoadLayout('<R01>     R|VPIS¨'); //ok
    LoadLayout('<R01>   R02|VBC¨|PPIS¨'); //ok
    LoadLayout('<R01>   R04|QBCProd¨|VAliqProd¨'); //ok
    LoadLayout('<S01>     S'); //ok
    LoadLayout('<S02>   S02|CST¨|VBC¨|PCOFINS¨|VCOFINS¨'); //ok
    LoadLayout('<S03>   S03|CST¨|QBCProd¨|VAliqProd¨|VCOFINS¨'); //ok
    LoadLayout('<S04>   S04|CST¨'); //ok
    LoadLayout('<S05>   S05|CST¨|VCOFINS¨'); //ok
    LoadLayout('<S05>   S07|VBC¨|PCOFINS¨'); //ok
    LoadLayout('<S05>   S09|QBCProd¨|VAliqProd¨'); //ok
    LoadLayout('<T01>     T|VCOFINS¨'); //ok
    LoadLayout('<T01>   T02|VBC¨|PCOFINS¨'); //ok
    LoadLayout('<T01>   T04|QBCProd¨|VAliqProd¨'); //ok
    LoadLayout('<U01>     U|VBC¨|VAliq¨|VISSQN¨|CMunFG¨|CListServ¨'); //ok
    LoadLayout('<W01>     W'); //ok
    LoadLayout('<W02>   W02|VBC¨|VICMS¨|VBCST¨|VST¨|VProd¨|VFrete¨|VSeg¨|VDesc¨|VII¨|VIPI¨|VPIS¨|VCOFINS¨|VOutro¨|VNF¨'); //ok
    LoadLayout('<W17>   W17|VServ¨|VBC¨|VISS¨|VPIS¨|VCOFINS¨'); //ok
    LoadLayout('<W23>   W23|VRetPIS¨|VRetCOFINS¨|VRetCSLL¨|VBCIRRF¨|VIRRF¨|VBCRetPrev¨|VRetPrev¨'); //ok
    LoadLayout('<X01>     X|ModFrete¨'); //ok
    LoadLayout('<X03>   X03|XNome¨|IE¨|XEnder¨|UF¨|XMun¨'); //ok
    LoadLayout('<X03>   X04|CNPJ¨'); //ok
    LoadLayout('<X03>   X05|CPF¨'); //ok
    LoadLayout('<X11>   X11|VServ¨|VBCRet¨|PICMSRet¨|VICMSRet¨|CFOP¨|CMunFG¨'); //ok
    LoadLayout('<X18>   X18|Placa¨|UF¨|RNTC¨'); //ok
    LoadLayout('<X22>   X22|Placa¨|UF¨|RNTC¨'); //ok
    LoadLayout('<X26>   X26|QVol¨|Esp¨|Marca¨|NVol¨|PesoL¨|PesoB¨'); //ok
    LoadLayout('<X33>   X33|NLacre¨'); //ok
    LoadLayout('<Y01>     Y'); //ok
    LoadLayout('<Y02>   Y02|NFat¨|VOrig¨|VDesc¨|VLiq¨'); //ok
    LoadLayout('<Y07>   Y07|NDup¨|DVenc¨|VDup¨'); //ok
    LoadLayout('<Z01>     Z|InfAdFisco¨|InfCpl¨'); //ok
    LoadLayout('<Z04>   Z04|XCampo¨|XTexto¨'); //ok
    //adLayout('<Z07>   Z07|XCampo¨|XTexto¨'); //ok - ?
    LoadLayout('<Z10>   Z10|NProc¨|IndProc¨'); //ok
    LoadLayout('<ZA01>   ZA|UFEmbarq¨|XLocEmbarq¨'); //ok
    LoadLayout('<ZB01>   ZB|XNEmp¨|XPed¨|XCont¨'); //ok
  end;
  Result := Layout;
end;

end.

