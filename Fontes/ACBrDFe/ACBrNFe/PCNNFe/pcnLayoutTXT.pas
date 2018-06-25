////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
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

unit pcnLayoutTXT;

interface

uses
  SysUtils, Classes;

function CarregarLayoutTXT(const versao: string): AnsiString;

const
  VERSOES_VALIDAS_LAYOUT_TXT = '|1.10|2.00|3.10|4.00|';

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
    LoadLayout('<B01>       NOTA FISCAL|1');
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
    LoadLayout('<M01>     M|VTotTrib¨'); //ok
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
    LoadLayout('<W02>   W02|VBC¨|VICMS¨|VBCST¨|VST¨|VProd¨|VFrete¨|VSeg¨|VDesc¨|VII¨|VIPI¨|VPIS¨|VCOFINS¨|VOutro¨|VNF¨|VTotTrib¨'); //ok
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
  end
  else if versao = '2.00' then
  begin
    LoadLayout('<B01>       NOTA FISCAL|1');
    LoadLayout('<B01>     A|2.00|^id^'); //ok
    LoadLayout('<B01>     B|cUF¨|cNF¨|NatOp¨|indPag¨|mod¨|serie¨|nNF¨|dEmi¨|dSaiEnt¨|hSaiEnt¨|tpNF¨|cMunFG¨|TpImp¨|TpEmis¨|CDV¨|TpAmb¨|FinNFe¨|ProcEmi¨|VerProc¨|dhCont¨|xJust¨'); //ok
    LoadLayout('<B12a>  B13|refNFe¨'); //ok
    LoadLayout('<B14>   B14|cUF¨|AAMM¨|CNPJ¨|Mod¨|serie¨|nNF¨'); //ok
    LoadLayout('<B20a> B20a|cUF¨|AAMM¨|IE¨|Mod¨|serie¨|nNF¨'); //
    LoadLayout('<B20d> B20d|CNPJ¨'); //
    LoadLayout('<B20e> B20e|CPF¨'); //
    LoadLayout('<B20i> B20i|refCTe¨'); //
    LoadLayout('<B20j> B20j|mod¨|nECF¨|nCOO¨'); //
    LoadLayout('<C01>     C|XNome¨|XFant¨|IE¨|IEST¨|IM¨|CNAE¨|CRT¨'); //ok
    LoadLayout('<C01>   C02|CNPJ¨'); //ok
    LoadLayout('<C01>  C02a|CPF¨'); //ok
    LoadLayout('<C05>   C05|XLgr¨|Nro¨|xCpl¨|xBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨'); //ok
    LoadLayout('<D01>     D|CNPJ¨|XOrgao¨|Matr¨|XAgente¨|Fone¨|UF¨|NDAR¨|DEmi¨|VDAR¨|RepEmi¨|DPag¨'); //ok
    LoadLayout('<E01>     E|XNome¨|IE¨|ISUF¨|EMAIL¨'); //ok
    LoadLayout('<E01>   E02|CNPJ¨'); //ok
    LoadLayout('<E01>   E03|CPF¨'); //ok
    LoadLayout('<E05>   E05|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨'); //ok
    LoadLayout('<F01>     F|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨'); //ok
    LoadLayout('<F01>   F02|CNPJ¨'); //ok
    LoadLayout('<F01>  F02a|CPF¨');  //ok
    LoadLayout('<G01>     G|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨'); //ok
    LoadLayout('<G01>   G02|CNPJ¨'); //ok
    LoadLayout('<G01>  G02a|CPF¨');  //ok
    LoadLayout('<H01>     H|NItem¨|InfAdProd¨'); //ok
    LoadLayout('<I01>     I|CProd¨|CEAN¨|XProd¨|NCM¨|EXTIPI¨|CFOP¨|UCom¨|QCom¨|VUnCom¨|VProd¨|CEANTrib¨|UTrib¨|QTrib¨|VUnTrib¨|VFrete¨|VSeg¨|VDesc¨|VOutro¨|indTot¨|xPed¨|nItemPed¨|nFCI¨'); //ok
    LoadLayout('<I18>   I18|NDI¨|DDI¨|XLocDesemb¨|UFDesemb¨|DDesemb¨|CExportador¨'); //ok
    LoadLayout('<I25>   I25|NAdicao¨|NSeqAdic¨|CFabricante¨|VDescDI¨'); //ok
    LoadLayout('<J01>     J|tpOp¨|chassi¨|cCor¨|xCor¨|pot¨|Cilin¨|pesoL¨|pesoB¨|NSerie¨|TpComb¨|NMotor¨|CMT¨|Dist¨|AnoMod¨|AnoFab¨|TpPint¨|TpVeic¨|EspVeic¨|VIN¨|CondVeic¨|CMod¨|cCorDENATRAN¨|lota¨|tpRest¨'); //NFe 2.0
    LoadLayout('<K01>     K|NLote¨|QLote¨|DFab¨|DVal¨|VPMC¨'); //ok
    LoadLayout('<L00>     L|TpArma¨|NSerie¨|NCano¨|Descr¨'); //ok
    LoadLayout('<L01>   L01|CProdANP¨|CODIF¨|QTemp¨|UFCons¨'); //ok
    LoadLayout('<L105> L105|QBCProd¨|VAliqProd¨|VCIDE¨'); //ok
    LoadLayout('<L109> L109|VBCICMS¨|VICMS¨|VBCICMSST¨|VICMSST¨'); //ok
    LoadLayout('<L114> L114|VBCICMSSTDest¨|VICMSSTDest¨'); //ok
    LoadLayout('<L117> L117|VBCICMSSTCons¨|VICMSSTCons¨|UFCons¨'); //ok
    LoadLayout('<M01>     M|VTotTrib¨'); //ok
    LoadLayout('<N01>     N'); //ok
    LoadLayout('<N02>   N02|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N03>   N03|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N04>   N04|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N05>   N05|Orig¨|CST¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N06>   N06|Orig¨|CST¨'); //ok
    LoadLayout('<N07>   N07|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨'); //ok
    LoadLayout('<N08>   N08|Orig¨|CST¨|VBCSTRet¨|VICMSSTRet¨'); //ok
    LoadLayout('<N09>   N09|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N10>   N10|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨'); //ok
    LoadLayout('<N10a> N10a|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|pBCOp¨|UFST¨');
    LoadLayout('<N10b> N10b|Orig¨|CST¨|vBCSTRet¨|vICMSSTRet¨|vBCSTDest¨|vICMSSTDest¨');
    LoadLayout('<N10c> N10c|Orig¨|CSOSN¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10d> N10d|Orig¨|CSOSN¨');
    LoadLayout('<N10e> N10e|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10f> N10f|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨');
    LoadLayout('<N10g> N10g|Orig¨|CSOSN¨|modBCST¨|vBCSTRet¨|vICMSSTRet¨');
    LoadLayout('<N10h> N10h|Orig¨|CSOSN¨|modBC¨|vBC¨|pRedBC¨|pICMS¨|vICMS¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|pCredSN¨|vCredICMSSN¨');
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
    LoadLayout('<U01>     U|VBC¨|VAliq¨|VISSQN¨|CMunFG¨|CListServ¨|CSitTrib¨'); //ok
    LoadLayout('<W01>     W'); //ok
    LoadLayout('<W02>   W02|VBC¨|VICMS¨|VBCST¨|VST¨|VProd¨|VFrete¨|VSeg¨|VDesc¨|VII¨|VIPI¨|VPIS¨|VCOFINS¨|VOutro¨|VNF¨|VTotTrib¨'); //ok
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
    LoadLayout('<Z07>   Z07|XCampo¨|XTexto¨'); //ok - ?
    LoadLayout('<Z10>   Z10|NProc¨|IndProc¨'); //ok
    LoadLayout('<ZA01>   ZA|UFEmbarq¨|XLocEmbarq¨'); //ok
    LoadLayout('<ZB01>   ZB|XNEmp¨|XPed¨|XCont¨'); //ok
  end
  else if versao = '3.10' then
  begin
    LoadLayout('<B>       NOTA FISCAL|1');
    LoadLayout('<B01>     A|3.10|^id^');
    LoadLayout('<B01>     B|cUF¨|cNF¨|NatOp¨|indPag¨|mod¨|serie¨|nNF¨|dhEmi¨|dhSaiEnt¨|tpNF¨|idDest¨|cMunFG¨|tpImp¨|tpEmis¨|CDV¨|tpAmb¨|finNFe¨|indFinal¨|indPres¨|procEmi¨|verProc¨|dhCont¨|xJust¨');
    LoadLayout('<BA>   BA02|refNFe¨');
    LoadLayout('<BA03> BA03|cUF¨|AAMM¨|CNPJ¨|Mod¨|serie¨|nNF¨');
    LoadLayout('<BA10> BA10|cUF¨|AAMM¨|IE¨|Mod¨|serie¨|nNF¨|refCTe¨');
    LoadLayout('<BA13> BA13|CNPJ¨');
    LoadLayout('<BA14> BA14|CPF¨');
    LoadLayout('<BA20> BA20|mod¨|nECF¨|nCOO¨');
    LoadLayout('<C01>     C|XNome¨|XFant¨|IE¨|IEST¨|IM¨|CNAE¨|CRT¨');
    LoadLayout('<C01>   C02|CNPJ¨');
    LoadLayout('<C01>  C02a|CPF¨');
    LoadLayout('<C05>   C05|XLgr¨|Nro¨|xCpl¨|xBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨');
    LoadLayout('<D01>     D|CNPJ¨|XOrgao¨|Matr¨|XAgente¨|Fone¨|UF¨|NDAR¨|DEmi¨|VDAR¨|RepEmi¨|DPag¨');
    LoadLayout('<E01>     E|XNome¨|indIEDest¨|IE¨|ISUF¨|IM¨|EMAIL¨');
    LoadLayout('<E01>   E02|CNPJ¨');
    LoadLayout('<E01>   E03|CPF¨');
    LoadLayout('<E01> E03a|idEstrangeiro¨');
    LoadLayout('<E05>   E05|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨');
    LoadLayout('<F01>     F|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨');
    LoadLayout('<F01>   F02|CNPJ¨');
    LoadLayout('<F01>  F02a|CPF¨');
    LoadLayout('<G01>     G|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨');
    LoadLayout('<G01>   G02|CNPJ¨');
    LoadLayout('<G01>  G02a|CPF¨');
    LoadLayout('<G01>  GA02|CNPJ¨');
    LoadLayout('<G01>  GA03|CPF¨');
    LoadLayout('<H01>     H|NItem¨|InfAdProd¨');
    LoadLayout('<I01>     I|CProd¨|CEAN¨|XProd¨|NCM¨|EXTIPI¨|CFOP¨|UCom¨|QCom¨|VUnCom¨|VProd¨|CEANTrib¨|UTrib¨|QTrib¨|VUnTrib¨|VFrete¨|VSeg¨|VDesc¨|VOutro¨|indTot¨|xPed¨|nItemPed¨|nFCI¨');
    LoadLayout('<I05A> I05a|NVE¨');
    LoadLayout('<I05c> I05c|CEST¨');
    LoadLayout('<I18>   I18|NDI¨|DDI¨|XLocDesemb¨|UFDesemb¨|DDesemb¨|tpViaTransp¨|vAFRMM¨|tpIntermedio¨|CNPJ¨|UFTerceiro¨|CExportador¨');
    LoadLayout('<I25>   I25|NAdicao¨|NSeqAdic¨|CFabricante¨|VDescDI¨|nDraw¨');
    LoadLayout('<I50>   I50|nDraw¨');
    LoadLayout('<I52>   I52|nRE¨|chNFe¨|qExport¨');
    LoadLayout('<J01>    JA|tpOp¨|chassi¨|cCor¨|xCor¨|pot¨|Cilin¨|pesoL¨|pesoB¨|NSerie¨|TpComb¨|NMotor¨|CMT¨|Dist¨|AnoMod¨|AnoFab¨|TpPint¨|TpVeic¨|EspVeic¨|VIN¨|CondVeic¨|CMod¨|cCorDENATRAN¨|lota¨|tpRest¨');
    LoadLayout('<K01>     K|NLote¨|QLote¨|DFab¨|DVal¨|VPMC¨');
    LoadLayout('<L01>     L|TpArma¨|NSerie¨|NCano¨|Descr¨');
    LoadLayout('<LA>     LA|CProdANP¨|PpMixGN¨|CODIF¨|QTemp¨|UFCons¨');
    LoadLayout('<LA07> LA07|QBCProd¨|VAliqProd¨|VCIDE¨');
    LoadLayout('<LB>     LB|nRECOPI¨');
    LoadLayout('<M01>     M|VTotTrib¨');
    LoadLayout('<N01>     N'); //ok
    LoadLayout('<N02>   N02|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨');
    LoadLayout('<N03>   N03|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨');
    LoadLayout('<N04>   N04|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N05>   N05|Orig¨|CST¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N06>   N06|Orig¨|CST¨|vICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N07>   N07|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMSOp¨|PDif¨|VICMSDif¨|VICMS¨');
    LoadLayout('<N08>   N08|Orig¨|CST¨|VBCSTRet¨|VICMSSTRet¨');
    LoadLayout('<N09>   N09|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N10>   N10|Orig¨|CST¨|ModBC¨|VBC¨|PRedBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N10a> N10a|Orig¨|CST¨|ModBC¨|VBC¨|PRedBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|pBCOp¨|UFST¨');
    LoadLayout('<N10b> N10b|Orig¨|CST¨|vBCSTRet¨|vICMSSTRet¨|vBCSTDest¨|vICMSSTDest¨');
    LoadLayout('<N10c> N10c|Orig¨|CSOSN¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10d> N10d|Orig¨|CSOSN¨');
    LoadLayout('<N10e> N10e|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10f> N10f|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨');
    LoadLayout('<N10g> N10g|Orig¨|CSOSN¨|vBCSTRet¨|vICMSSTRet¨');
    LoadLayout('<N10h> N10h|Orig¨|CSOSN¨|modBC¨|vBC¨|pRedBC¨|pICMS¨|vICMS¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<NA>     NA|vBCUFDest¨|pFCPUFDest¨|pICMSUFDest¨|pICMSInter¨|pICMSInterPart¨|vFCPUFDest¨|vICMSUFDest¨|vICMSUFRemet¨');
    LoadLayout('<O01>     O|ClEnq¨|CNPJProd¨|CSelo¨|QSelo¨|CEnq¨');
    LoadLayout('<O07>   O07|CST¨|VIPI¨');
    LoadLayout('<O07>   O10|VBC¨|PIPI¨|VIPI¨');
    LoadLayout('<O07>   O11|QUnid¨|VUnid¨|VIPI¨');
    LoadLayout('<O08>   O08|CST¨');
    LoadLayout('<P01>     P|VBC¨|VDespAdu¨|VII¨|VIOF¨');
    LoadLayout('<Q01>     Q'); //ok
    LoadLayout('<Q02>   Q02|CST¨|VBC¨|PPIS¨|VPIS¨');
    LoadLayout('<Q03>   Q03|CST¨|QBCProd¨|VAliqProd¨|VPIS¨');
    LoadLayout('<Q04>   Q04|CST¨');
    LoadLayout('<Q05>   Q05|CST¨|VPIS¨');
    LoadLayout('<Q05>   Q07|VBC¨|PPIS¨|VPIS¨');
    LoadLayout('<Q05>   Q10|QBCProd¨|VAliqProd¨');
    LoadLayout('<R01>     R|VPIS¨');
    LoadLayout('<R01>   R02|VBC¨|PPIS¨');
    LoadLayout('<R01>   R04|QBCProd¨|VAliqProd¨');
    LoadLayout('<S01>     S'); //ok
    LoadLayout('<S02>   S02|CST¨|VBC¨|PCOFINS¨|VCOFINS¨');
    LoadLayout('<S03>   S03|CST¨|QBCProd¨|VAliqProd¨|VCOFINS¨');
    LoadLayout('<S04>   S04|CST¨');
    LoadLayout('<S05>   S05|CST¨|VCOFINS¨');
    LoadLayout('<S05>   S07|VBC¨|PCOFINS¨');
    LoadLayout('<S05>   S09|QBCProd¨|VAliqProd¨');
    LoadLayout('<T01>     T|VCOFINS¨');
    LoadLayout('<T01>   T02|VBC¨|PCOFINS¨');
    LoadLayout('<T01>   T04|QBCProd¨|VAliqProd¨');
    LoadLayout('<U01>     U|VBC¨|VAliq¨|VISSQN¨|CMunFG¨|CListServ¨|VDeducao¨|VOutro¨|VDescIncond¨|VDescCond¨|VISSRet¨|IndISS¨|CServico¨|CMun¨|CPais¨|NProcesso¨|IndIncentivo¨'); //ok
    LoadLayout('<UA>     UA|ImpostoDevol¨|PDevol¨|IPI¨|VIPIDevol¨');  //VERIFICAR
    LoadLayout('<W01>     W'); //ok
    LoadLayout('<W02>   W02|vBC¨|vICMS¨|vICMSDeson¨|vBCST¨|vST¨|vProd¨|vFrete¨|vSeg¨|vDesc¨|vII¨|vIPI¨|vPIS¨|vCOFINS¨|vOutro¨|vNF¨|vTotTrib¨');
    LoadLayout('<W04c> W04c|vFCPUFDest¨');
    LoadLayout('<W04e> W04e|vICMSUFDest¨');
    LoadLayout('<W04g> W04g|vICMSUFRemet¨');
    LoadLayout('<W17>   W17|VServ¨|VBC¨|VISS¨|VPIS¨|VCOFINS¨|dCompet¨|vDeducao¨|vOutro¨|vDescIncond¨|vDescCond¨|vISSRet¨|cRegTrib¨');
    LoadLayout('<W23>   W23|VRetPIS¨|VRetCOFINS¨|VRetCSLL¨|VBCIRRF¨|VIRRF¨|VBCRetPrev¨|VRetPrev¨');
    LoadLayout('<X01>     X|ModFrete¨');
    LoadLayout('<X03>   X03|XNome¨|IE¨|XEnder¨|XMun¨|UF¨');
    LoadLayout('<X03>   X04|CNPJ¨');
    LoadLayout('<X03>   X05|CPF¨');
    LoadLayout('<X11>   X11|VServ¨|VBCRet¨|PICMSRet¨|VICMSRet¨|CFOP¨|CMunFG¨');
    LoadLayout('<X18>   X18|Placa¨|UF¨|RNTC¨');
    LoadLayout('<X22>   X22|Placa¨|UF¨|RNTC¨|Vagao¨|Balsa¨');  //VERIFICAR
    LoadLayout('<X26>   X26|QVol¨|Esp¨|Marca¨|NVol¨|PesoL¨|PesoB¨');
    LoadLayout('<X26>   X33|NLacre¨');
    LoadLayout('<Y01>     Y'); //ok
    LoadLayout('<Y02>   Y02|NFat¨|VOrig¨|VDesc¨|VLiq¨');
    LoadLayout('<Y07>   Y07|NDup¨|DVenc¨|VDup¨');
    LoadLayout('<YA>     YA|TPag¨|VPag¨|Card¨|CNPJ¨|TBand¨|CAut¨');
    LoadLayout('<Z01>     Z|InfAdFisco¨|InfCpl¨');
    LoadLayout('<Z04>   Z04|XCampo¨|XTexto¨');
    LoadLayout('<Z07>   Z07|XCampo¨|XTexto¨');
    LoadLayout('<Z10>   Z10|NProc¨|IndProc¨');
    LoadLayout('<ZA>     ZA|UFSaidaPais¨|XLocExporta¨|XLocDespacho¨');
    LoadLayout('<ZB>     ZB|XNEmp¨|XPed¨|XCont¨');
    LoadLayout('<ZC>     ZB|Safra¨|Ref¨|QTotMes¨|QTotAnt¨|QTotGer¨|VFor¨|VTotDed¨|VLiqFor¨');
    LoadLayout('<ZC04> ZC04|Dia¨|Qtde¨');
    LoadLayout('<ZC10> ZC10|XDed¨|VDed¨');
  end
  else if versao = '4.00' then
  begin
    LoadLayout('<B>       NOTA FISCAL|1');
    LoadLayout('<B01>     A|4.00|^id^');
    LoadLayout('<B01>     B|cUF¨|cNF¨|NatOp¨|mod¨|serie¨|nNF¨|dhEmi¨|dhSaiEnt¨|tpNF¨|idDest¨|cMunFG¨|tpImp¨|tpEmis¨|CDV¨|tpAmb¨|finNFe¨|indFinal¨|indPres¨|procEmi¨|verProc¨|dhCont¨|xJust¨');
    LoadLayout('<BA>   BA02|refNFe¨');
    LoadLayout('<BA03> BA03|cUF¨|AAMM¨|CNPJ¨|Mod¨|serie¨|nNF¨');
    LoadLayout('<BA10> BA10|cUF¨|AAMM¨|IE¨|Mod¨|serie¨|nNF¨|refCTe¨');
    LoadLayout('<BA13> BA13|CNPJ¨');
    LoadLayout('<BA14> BA14|CPF¨');
    LoadLayout('<BA20> BA20|mod¨|nECF¨|nCOO¨');
    LoadLayout('<C01>     C|XNome¨|XFant¨|IE¨|IEST¨|IM¨|CNAE¨|CRT¨');
    LoadLayout('<C01>   C02|CNPJ¨');
    LoadLayout('<C01>  C02a|CPF¨');
    LoadLayout('<C05>   C05|XLgr¨|Nro¨|xCpl¨|xBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨');
    LoadLayout('<D01>     D|CNPJ¨|XOrgao¨|Matr¨|XAgente¨|Fone¨|UF¨|NDAR¨|DEmi¨|VDAR¨|RepEmi¨|DPag¨');
    LoadLayout('<E01>     E|XNome¨|indIEDest¨|IE¨|ISUF¨|IM¨|EMAIL¨');
    LoadLayout('<E01>   E02|CNPJ¨');
    LoadLayout('<E01>   E03|CPF¨');
    LoadLayout('<E01> E03a|idEstrangeiro¨');
    LoadLayout('<E05>   E05|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨|CEP¨|CPais¨|XPais¨|Fone¨');
    LoadLayout('<F01>     F|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨');
    LoadLayout('<F01>   F02|CNPJ¨');
    LoadLayout('<F01>  F02a|CPF¨');
    LoadLayout('<G01>     G|XLgr¨|Nro¨|XCpl¨|XBairro¨|CMun¨|XMun¨|UF¨');
    LoadLayout('<G01>   G02|CNPJ¨');
    LoadLayout('<G01>  G02a|CPF¨');
    LoadLayout('<G01>  GA02|CNPJ¨');
    LoadLayout('<G01>  GA03|CPF¨');
    LoadLayout('<H01>     H|NItem¨|InfAdProd¨');
    LoadLayout('<I01>     I|CProd¨|CEAN¨|XProd¨|NCM¨|CEST¨|indEscala¨|CNPJFab¨|cBenef¨|EXTIPI¨|CFOP¨|UCom¨|QCom¨|VUnCom¨|VProd¨|CEANTrib¨|UTrib¨|QTrib¨|VUnTrib¨|VFrete¨|VSeg¨|VDesc¨|VOutro¨|indTot¨|xPed¨|nItemPed¨|nFCI¨');
    LoadLayout('<I05A> I05a|NVE¨');
    LoadLayout('<I05c> I05c|CEST¨|indEscala¨|CNPJFab¨');
    LoadLayout('<I18>   I18|NDI¨|DDI¨|XLocDesemb¨|UFDesemb¨|DDesemb¨|tpViaTransp¨|vAFRMM¨|tpIntermedio¨|CNPJ¨|UFTerceiro¨|CExportador¨');
    LoadLayout('<I25>   I25|NAdicao¨|NSeqAdic¨|CFabricante¨|VDescDI¨|nDraw¨');
    LoadLayout('<I50>   I50|nDraw¨');
    LoadLayout('<I52>   I52|nRE¨|chNFe¨|qExport¨');
    LoadLayout('<I80>   I80|nLote¨|qLote¨|dFab¨|dVal¨|cAgreg¨');
    LoadLayout('<J01>    JA|tpOp¨|chassi¨|cCor¨|xCor¨|pot¨|Cilin¨|pesoL¨|pesoB¨|NSerie¨|TpComb¨|NMotor¨|CMT¨|Dist¨|AnoMod¨|AnoFab¨|TpPint¨|TpVeic¨|EspVeic¨|VIN¨|CondVeic¨|CMod¨|cCorDENATRAN¨|lota¨|tpRest¨');
    LoadLayout('<K01>     K|cProdANVISA¨|vPMC¨');
    LoadLayout('<L01>     L|TpArma¨|NSerie¨|NCano¨|Descr¨');
    LoadLayout('<LA>     LA|cProdANP¨|descANP¨|pGLP¨|pGNn¨|pGNi¨|vPart¨|CODIF¨|qTemp¨|UFCons¨');
    LoadLayout('<LA07> LA07|QBCProd¨|VAliqProd¨|VCIDE¨');
    LoadLayout('<LA11> LA11|nBico¨|nBomba¨|nTanque¨|vEncIni¨|vEncFin¨');
    LoadLayout('<LB>     LB|nRECOPI¨');
    LoadLayout('<M01>     M|VTotTrib¨');
    LoadLayout('<N01>     N'); //ok
    LoadLayout('<N02>   N02|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨|pFCP¨|vFCP¨');
    LoadLayout('<N03>   N03|Orig¨|CST¨|ModBC¨|VBC¨|PICMS¨|VICMS¨|vBCFCP¨|pFCP¨|vFCP¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨');
    LoadLayout('<N04>   N04|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|vBCFCP¨|pFCP¨|vFCP¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N05>   N05|Orig¨|CST¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N06>   N06|Orig¨|CST¨|vICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N07>   N07|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMSOp¨|PDif¨|VICMSDif¨|VICMS¨|vBCFCP¨|pFCP¨|vFCP¨');
    LoadLayout('<N08>   N08|Orig¨|CST¨|VBCSTRet¨|pST¨|VICMSSTRet¨|vICMSSTRet¨|vBCFCPSTRet¨|pFCPSTRet¨|vFCPSTRet¨|pRedBCEfet¨|vBCEfet¨|pICMSEfet¨|vICMSEfet¨');
    LoadLayout('<N09>   N09|Orig¨|CST¨|ModBC¨|PRedBC¨|VBC¨|PICMS¨|VICMS¨|vBCFCP¨|pFCP¨|vFCP¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N10>   N10|Orig¨|CST¨|ModBC¨|VBC¨|PRedBC¨|PICMS¨|VICMS¨|vBCFCP¨|pFCP¨|vFCP¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨|VICMSDeson¨|MotDesICMS¨');
    LoadLayout('<N10a> N10a|Orig¨|CST¨|ModBC¨|VBC¨|PRedBC¨|PICMS¨|VICMS¨|ModBCST¨|PMVAST¨|PRedBCST¨|VBCST¨|PICMSST¨|VICMSST¨|pBCOp¨|UFST¨');
    LoadLayout('<N10b> N10b|Orig¨|CST¨|vBCSTRet¨|vICMSSTRet¨|vBCSTDest¨|vICMSSTDest¨');
    LoadLayout('<N10c> N10c|Orig¨|CSOSN¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10d> N10d|Orig¨|CSOSN¨');
    LoadLayout('<N10e> N10e|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<N10f> N10f|Orig¨|CSOSN¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨');
    LoadLayout('<N10g> N10g|Orig¨|CSOSN¨|vBCSTRet¨|pST¨|vICMSSTRet¨|vBCFCPSTRet¨|pFCPSTRet¨|vFCPSTRet¨|pRedBCEfet¨|vBCEfet¨|pICMSEfet¨|vICMSEfet¨');
    LoadLayout('<N10h> N10h|Orig¨|CSOSN¨|modBC¨|vBC¨|pRedBC¨|pICMS¨|vICMS¨|modBCST¨|pMVAST¨|pRedBCST¨|vBCST¨|pICMSST¨|vICMSST¨|vBCFCPST¨|pFCPST¨|vFCPST¨|pCredSN¨|vCredICMSSN¨');
    LoadLayout('<NA>     NA|vBCUFDest¨|vBCFCPUFDest¨|pFCPUFDest¨|pICMSUFDest¨|pICMSInter¨|pICMSInterPart¨|vFCPUFDest¨|vICMSUFDest¨|vICMSUFRemet¨');
    LoadLayout('<O01>     O|CNPJProd¨|CSelo¨|QSelo¨|CEnq¨');
    LoadLayout('<O07>   O07|CST¨|VIPI¨');
    LoadLayout('<O07>   O10|VBC¨|PIPI¨|VIPI¨');
    LoadLayout('<O07>   O11|QUnid¨|VUnid¨|VIPI¨');
    LoadLayout('<O08>   O08|CST¨');
    LoadLayout('<P01>     P|VBC¨|VDespAdu¨|VII¨|VIOF¨');
    LoadLayout('<Q01>     Q'); //ok
    LoadLayout('<Q02>   Q02|CST¨|VBC¨|PPIS¨|VPIS¨');
    LoadLayout('<Q03>   Q03|CST¨|QBCProd¨|VAliqProd¨|VPIS¨');
    LoadLayout('<Q04>   Q04|CST¨');
    LoadLayout('<Q05>   Q05|CST¨|VPIS¨');
    LoadLayout('<Q05>   Q07|VBC¨|PPIS¨|VPIS¨');
    LoadLayout('<Q05>   Q10|QBCProd¨|VAliqProd¨');
    LoadLayout('<R01>     R|VPIS¨');
    LoadLayout('<R01>   R02|VBC¨|PPIS¨|VPIS¨');
    LoadLayout('<R01>   R04|QBCProd¨|VAliqProd¨|VPIS¨');
    LoadLayout('<S01>     S'); //ok
    LoadLayout('<S02>   S02|CST¨|VBC¨|PCOFINS¨|VCOFINS¨');
    LoadLayout('<S03>   S03|CST¨|QBCProd¨|VAliqProd¨|VCOFINS¨');
    LoadLayout('<S04>   S04|CST¨');
    LoadLayout('<S05>   S05|CST¨|VCOFINS¨');
    LoadLayout('<S05>   S07|VBC¨|PCOFINS¨');
    LoadLayout('<S05>   S09|QBCProd¨|VAliqProd¨');
    LoadLayout('<T01>     T|VCOFINS¨');
    LoadLayout('<T01>   T02|VBC¨|PCOFINS¨');
    LoadLayout('<T01>   T04|QBCProd¨|VAliqProd¨');
    LoadLayout('<U01>     U|VBC¨|VAliq¨|VISSQN¨|CMunFG¨|CListServ¨|VDeducao¨|VOutro¨|VDescIncond¨|VDescCond¨|VISSRet¨|IndISS¨|CServico¨|CMun¨|CPais¨|NProcesso¨|IndIncentivo¨'); //ok
    LoadLayout('<UA>     UA|ImpostoDevol¨|PDevol¨|IPI¨|VIPIDevol¨');  //VERIFICAR
    LoadLayout('<W01>     W'); //ok
    LoadLayout('<W02>   W02|vBC¨|vICMS¨|vICMSDeson¨|vFCP¨|vFCPUFDest¨|vICMSUFDest¨|vICMSUFRemet¨|vBCST¨|vST¨|vFCPST¨|vFCPSTRet¨|vProd¨|vFrete¨|vSeg¨|vDesc¨|vII¨|vIPI¨|vIPIDevol¨|vPIS¨|vCOFINS¨|vOutro¨|vNF¨|vTotTrib¨');
    LoadLayout('<W17>   W17|VServ¨|VBC¨|VISS¨|VPIS¨|VCOFINS¨|dCompet¨|vDeducao¨|vOutro¨|vDescIncond¨|vDescCond¨|vISSRet¨|cRegTrib¨');
    LoadLayout('<W23>   W23|VRetPIS¨|VRetCOFINS¨|VRetCSLL¨|VBCIRRF¨|VIRRF¨|VBCRetPrev¨|VRetPrev¨');
    LoadLayout('<X01>     X|ModFrete¨');
    LoadLayout('<X03>   X03|XNome¨|IE¨|XEnder¨|XMun¨|UF¨');
    LoadLayout('<X03>   X04|CNPJ¨');
    LoadLayout('<X03>   X05|CPF¨');
    LoadLayout('<X11>   X11|VServ¨|VBCRet¨|PICMSRet¨|VICMSRet¨|CFOP¨|CMunFG¨');
    LoadLayout('<X18>   X18|Placa¨|UF¨|RNTC¨');
    LoadLayout('<X22>   X22|Placa¨|UF¨|RNTC¨|Vagao¨|Balsa¨');  //VERIFICAR
    LoadLayout('<X26>   X26|QVol¨|Esp¨|Marca¨|NVol¨|PesoL¨|PesoB¨');
    LoadLayout('<X26>   X33|NLacre¨');
    LoadLayout('<Y01>     Y'); //ok
    LoadLayout('<Y02>   Y02|NFat¨|VOrig¨|VDesc¨|VLiq¨');
    LoadLayout('<Y07>   Y07|NDup¨|DVenc¨|VDup¨');
    LoadLayout('<YA>     YA|indPag¨|TPag¨|VPag¨|Card¨|CNPJ¨|TBand¨|CAut¨');
    LoadLayout('<YA09> YA09|vTroco¨');
    LoadLayout('<Z01>     Z|InfAdFisco¨|InfCpl¨');
    LoadLayout('<Z04>   Z04|XCampo¨|XTexto¨');
    LoadLayout('<Z07>   Z07|XCampo¨|XTexto¨');
    LoadLayout('<Z10>   Z10|NProc¨|IndProc¨');
    LoadLayout('<ZA>     ZA|UFSaidaPais¨|XLocExporta¨|XLocDespacho¨');
    LoadLayout('<ZB>     ZB|XNEmp¨|XPed¨|XCont¨');
    LoadLayout('<ZC>     ZB|Safra¨|Ref¨|QTotMes¨|QTotAnt¨|QTotGer¨|VFor¨|VTotDed¨|VLiqFor¨');
    LoadLayout('<ZC04> ZC04|Dia¨|Qtde¨');
    LoadLayout('<ZC10> ZC10|XDed¨|VDed¨');
  end;

  Result := Layout;
end;

end.

