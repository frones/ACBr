{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wiliam Zacarias da Silva Rosa          }
{                                       Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{*******************************************************************************
|* Historico
|*
|* 16/12/2008: Wemerson Souto
|*  - Doação do componente para o Projeto ACBr
|* 10/02/2009: André Ferreira de Moraes
|*  - Adicionado URL de todos os estados
|* 18/02/2009: André Ferreira de Moraes
|*  - Criado Assinatura baseado em código passado por Daniel Simões
|*  - Criado Validação do XML da CTe baseado em código passado por Daniel Simões
|* 07/08/2009 : Wiliam Zacarias da Silva Rosa
|*  - Adicionado URL do estado de MT
|* 08/03/2010 : Bruno - Rhythmus Informatica
|* - Function GetURL:
|*  Incluida instrução 33: Result := CTeUtil.GetURLRS(AAmbiente, ALayOut); //RJ
|*  RJ usa os WebServices do RS
*******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeUtil;

interface

uses
{$IFNDEF ACBrCTeOpenSSL}
  ACBrCAPICOM_TLB, ACBrMSXML2_TLB, JwaWinCrypt,
{$ENDIF}
  Classes, Forms,
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}
  StrUtils, Activex,
{$ENDIF}
  ACBrCTeConfiguracoes, pcnConversao, pcteCTe, ACBrDFeUtil;

{$IFDEF ACBrCTeOpenSSL}
const
  cDTD     = '<!DOCTYPE test [<!ATTLIST infCte Id ID #IMPLIED>]>';
  cDTDCanc = '<!DOCTYPE test [<!ATTLIST infCanc Id ID #IMPLIED>]>';
  cDTDInut = '<!DOCTYPE test [<!ATTLIST infInut Id ID #IMPLIED>]>';
  cDTDEven = '<!DOCTYPE test [<!ATTLIST infEvento Id ID #IMPLIED>]>';
{$ELSE}
const
  DSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
{$ENDIF}
{$IFNDEF ACBrCTeOpenSSL}
var
  CertStore    : IStore3;
  CertStoreMem : IStore3;
  PrivateKey   : IPrivateKey;
  Certs        : ICertificates2;
  Cert         : ICertificate2;
  NumCertCarregado : String;
{$ENDIF}

type
  CTeUtil = class
  private
    // Estados Emissores pela Sefaz Virtual RS (Rio Grande do Sul):
    // AC, AL, AM, BA, CE, DF, ES, GO, MA, PA, PB, PI, RJ, RN, RO, SC, SE, TO.
    class function GetURLSVRS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;

    class function GetURLAC(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLAL(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLAP(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLAM(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLBA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLCE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLDF(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLES(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLGO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLMA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLPA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLPB(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLPE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLPI(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLRJ(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLRN(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLRO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLRR(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLSC(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLSE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLTO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;

    class function GetURLMG(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLRS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLSP(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLMT(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLMS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
    class function GetURLPR(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;

  protected

  public
{$IFDEF ACBrCTeOpenSSL}
//    class function sign_file(const Axml: PAnsiChar; const key_file: PChar; const senha: PChar): AnsiString;
    class function sign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
    class function sign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
    class procedure InitXmlSec;
    class procedure ShutDownXmlSec;
{$ENDIF}
    class function GetURL(const AUF, AAmbiente, FormaEmissao: Integer; ALayOut: TLayOutCTe): WideString;
    class function Valida(const AXML: AnsiString; var AMsg: AnsiString; const APathSchemas: String = ''): Boolean;

    class function FormatarChaveAcesso(AValue: String; Mascara: Boolean = False ): String;
    class function FormatarNumCTe(const AValue: Integer): String;
    class function FormatarValor(mask: TpcteMask; const AValue: real): String;
    class function GerarChaveContingencia(FCTe:TCTe): String;
    class function FormatarChaveContingencia(AValue: String): String;
    class function ValidaAssinatura(const AXML: AnsiString;  var AMsg: AnsiString): Boolean;

{$IFDEF ACBrCTeOpenSSL}
    class function Assinar(const AXML, ArqPFX, PFXSenha: AnsiString; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ELSE}
    class function Assinar(const AXML: AnsiString; Certificado: ICertificate2; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ENDIF}

    class function UFtoCUF(UF: String): Integer;
    class function IdentificaTipoSchema(Const AXML: AnsiString; var I: Integer): Integer;
    class function CstatProcessado(AValue: Integer): Boolean;
  end;

implementation

uses
 {$IFDEF ACBrCTeOpenSSL}
  libxml2, libxmlsec, libxslt,
 {$ELSE}
  ComObj,
 {$ENDIF}
  Sysutils, Variants, ACBrUtil, ACBrConsts, pcnAuxiliar;

{ CTeUtil }

{$IFDEF ACBrCTeOpenSSL}
class procedure CTeUtil.InitXmlSec;
begin
  { Init libxml and libxslt libraries }
  xmlInitParser();
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  xmlSubstituteEntitiesDefault(1);
  __xmlIndentTreeOutput^ := 1;

  { Init xmlsec library }
  if (xmlSecInit() < 0) then
    raise Exception.Create('Error: xmlsec initialization failed.');

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 8, xmlSecCheckVersionABICompatible) <> 1) then
    raise Exception.Create('Error: loaded xmlsec library version is not compatible.');

  (* Load default crypto engine if we are supporting dynamic
   * loading for xmlsec-crypto libraries. Use the crypto library
   * name ("openssl", "nss", etc.) to load corresponding
   * xmlsec-crypto library.
   *)
  if (xmlSecCryptoDLLoadLibrary('openssl') < 0) then
    raise Exception.Create('Error: unable to load default xmlsec-crypto library. Make sure'#10 +
      'that you have it installed and check shared libraries path'#10 +
      '(LD_LIBRARY_PATH) environment variable.');

  { Init crypto library }
  if (xmlSecCryptoAppInit(nil) < 0) then
    raise Exception.Create('Error: crypto initialization failed.');

  { Init xmlsec-crypto library }
  if (xmlSecCryptoInit() < 0) then
    raise Exception.Create('Error: xmlsec-crypto initialization failed.');
end;

class procedure CTeUtil.ShutDownXmlSec;
begin
  { Shutdown xmlsec-crypto library }
  xmlSecCryptoShutdown();

  { Shutdown crypto library }
  xmlSecCryptoAppShutdown();

  { Shutdown xmlsec library }
  xmlSecShutdown();

  { Shutdown libxslt/libxml }
  xsltCleanupGlobals();
  xmlCleanupParser();
end;
{$ENDIF}

class function CTeUtil.GetURL(const AUF, AAmbiente, FormaEmissao: Integer;
  ALayOut: TLayOutCTe): WideString;
begin
  //  (AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,RJ,RN,RS,RO,RR,SC,SP,SE,TO);
  //  (12,27,16,13,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,14,42,35,28,17);

 case FormaEmissao of
  1, 4, 5 : begin // 1 = Normal, 4 = EPEC (Envio Prévio de Emissão em Contingência), 5 = Contingência FSDA
             case ALayOut of
              LayCTeEventoEPEC : begin
                                   case AUF of
                                    11, // Rondônia
                                    12, // Acre
                                    13, // Amazonas
                                    14, // Roraima
                                    15, // Pará
                                    16, // Amapá
                                    17, // Tocantins
                                    21, // Maranhão
                                    22, // Piauí
                                    23, // Ceará
                                    24, // Rio Grande do Norte
                                    25, // Paraibá
                                    27, // Alagoas
                                    28, // Sergipe
                                    29, // Bahia
                                    31, // Minas Gerais
                                    32, // Espirito Santo
                                    33, // Rio de Janeiro
                                    41, // Paraná
                                    42, // Santa Catarina
                                    43, // Rio Grande do Sul
                                    52, // Goiás
                                    53: // Distrito Federal
                                        Result := IfThen(AAmbiente=1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcaoEvento.asmx', 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcaoEvento.asmx');
                                    26, // Pernanbuco
                                    35, // São Paulo
                                    50, // Mato Grosso do Sul
                                    51: // Mato Grosso
                                        // Alterado por Italo em 23/04/2013 conforme NT2013/003
                                        Result := IfThen(AAmbiente=1, 'https://cte.sefaz.rs.gov.br/ws/CteRecepcaoEvento/CteRecepcaoEvento.asmx', 'https://homologacao.cte.sefaz.rs.gov.br/ws/CteRecepcaoEvento/CteRecepcaoEvento.asmx');
                                   end;
                                 end;
              else begin
                     case AUF of
                      12: Result := CTeUtil.GetURLAC(AAmbiente, ALayOut);       //AC - Acre
                      27: Result := CTeUtil.GetURLAL(AAmbiente, ALayOut);       //AL - Alagoas
                      16: Result := CTeUtil.GetURLAP(AAmbiente, ALayOut);       //AP - Amapá
                      13: Result := CTeUtil.GetURLAM(AAmbiente, ALayOut);       //AM - Amazonas
                      29: Result := CTeUtil.GetURLBA(AAmbiente, ALayOut);       //BA - Bahia
                      23: Result := CTeUtil.GetURLCE(AAmbiente, ALayOut);       //CE - Ceará
                      53: Result := CTeUtil.GetURLDF(AAmbiente, ALayOut);       //DF - Distrito Federal
                      32: Result := CTeUtil.GetURLES(AAmbiente, ALayOut);       //ES - Espirito Santo
                      52: Result := CTeUtil.GetURLGO(AAmbiente, ALayOut);       //GO - Goiás
                      21: Result := CTeUtil.GetURLMA(AAmbiente, ALayOut);       //MA - Maranhão
                      51: Result := CTeUtil.GetURLMT(AAmbiente, ALayOut);       //MT - Mato Grosso
                      50: Result := CTeUtil.GetURLMS(AAmbiente, ALayOut);       //MS - Mato Grosso do Sul
                      31: Result := CTeUtil.GetURLMG(AAmbiente, ALayOut);       //MG - Minas Gerais
                      15: Result := CTeUtil.GetURLPA(AAmbiente, ALayOut);       //PA - Pará
                      25: Result := CTeUtil.GetURLPB(AAmbiente, ALayOut);       //PB - Paraibá
                      41: Result := CTeUtil.GetURLPR(AAmbiente, ALayOut);       //PR - Paraná
                      26: Result := CTeUtil.GetURLPE(AAmbiente, ALayOut);       //PE - Pernambuco
                      22: Result := CTeUtil.GetURLPI(AAmbiente, ALayOut);       //PI - Piauí
                      33: Result := CTeUtil.GetURLRJ(AAmbiente, ALayOut);       //RJ - Rio de Janeiro
                      24: Result := CTeUtil.GetURLRN(AAmbiente, ALayOut);       //RN - Rio Grande do Norte
                      43: Result := CTeUtil.GetURLRS(AAmbiente, ALayOut);       //RS - Rio Grande do Sul
                      11: Result := CTeUtil.GetURLRO(AAmbiente, ALayOut);       //RO - Rondônia
                      14: Result := CTeUtil.GetURLRR(AAmbiente, ALayOut);       //RR - Roraima
                      42: Result := CTeUtil.GetURLSC(AAmbiente, ALayOut);       //SC - Santa Catarina
                      35: Result := CTeUtil.GetURLSP(AAmbiente, ALayOut);       //SP - São Paulo
                      28: Result := CTeUtil.GetURLSE(AAmbiente, ALayOut);       //SE - Sergipe
                      17: Result := CTeUtil.GetURLTO(AAmbiente, ALayOut);       //TO - Tocantins
                     end;
                   end;
             end;
            end;
        7 : begin // Autorização pela SVC-RS (SEFAZ Vitual de Contingência do RS)
             case ALayOut of                                                                                                                          // Removido a palavra virtual das URLs de Homologação: sefazvirtual -> sefaz
               LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeRecepcao/CTeRecepcao.asmx'            , 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeRecepcao/CTeRecepcao.asmx');
               LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeRetRecepcao/CTeRetRecepcao.asmx'      , 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeRetRecepcao/CTeRetRecepcao.asmx');
//               LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeCancelamento/CTeCancelamento.asmx'    , 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeCancelamento/CTeCancelamento.asmx');
               LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeConsulta/CTeConsulta.asmx'            , 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeConsulta/CTeConsulta.asmx');
               LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeStatusServico/CTeStatusServico.asmx'  , 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeStatusServico/CTeStatusServico.asmx');
               LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/CTeRecepcaoEvento/CTeRecepcaoEvento.asmx', 'https://homologacao.cte.sefaz.rs.gov.br/ws/CTeRecepcaoEvento/CTeTecepcaoEvento.asmx');
             end;
            end;
        8 : begin // Autorização pela SVC-SP (SEFAZ Vitual de Contingência de SP)
             case ALayOut of
               LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcao.asmx'      , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcao.asmx');
               LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteRetRecepcao.asmx'   , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteRetRecepcao.asmx');
//               LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteCancelamento.asmx'  , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteCancelamento.asmx');
               LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteConsulta.asmx'      , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteConsulta.asmx');
               LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteStatusServico.asmx' , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteStatusServico.asmx');
               LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcaoEvento.asmx', 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/CteRecepcaoEvento.asmx');
             end;
            end;

 end;
 if Result = '' then
   raise Exception.Create('URL não disponível para o Estado solicitado.');
end;

// SVRS = SEFAZ Virtual do RS
class function CTeUtil.GetURLSVRS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cterecepcao/CteRecepcao.asmx'            , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cterecepcao/CteRecepcao.asmx');
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteretrecepcao/CteRetRecepcao.asmx'      , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteretrecepcao/CteRetRecepcao.asmx');
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/ctecancelamento/ctecancelamento.asmx'    , 'https://homologacao.cte.sefaz.rs.gov.br/ws/ctecancelamento/ctecancelamento.asmx');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteinutilizacao/cteinutilizacao.asmx'    , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteinutilizacao/cteinutilizacao.asmx');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteconsulta/cteconsulta.asmx'            , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteconsulta/cteconsulta.asmx');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/ctestatusservico/ctestatusservico.asmx'  , 'https://homologacao.cte.sefaz.rs.gov.br/ws/ctestatusservico/ctestatusservico.asmx');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, '', '');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteRecepcaoEvento/cteRecepcaoEvento.asmx', 'https://homologacao.cte.sefaz.rs.gov.br/ws/cterecepcaoevento/cterecepcaoevento.asmx');
  end;
end;

class function CTeUtil.GetURLAC(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLAL(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLAP(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSP(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLAM(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.am.gov.br/services2/services/cadconsultacadastro2', 'https://homnfe.sefaz.am.gov.br/services2/services/cadconsultacadastro2');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLBA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.ba.gov.br/webservices/nfenw/CadConsultaCadastro2.asmx', 'https://hnfe.sefaz.ba.gov.br/webservices/nfenw/CadConsultaCadastro2.asmx');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLCE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.ce.gov.br/nfe2/services/CadConsultaCadastro2', 'https://nfeh.sefaz.ce.gov.br/nfe2/services/CadConsultaCadastro2');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLDF(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLES(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLGO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.go.gov.br/nfe/services/v2/CadConsultaCadastro2', 'https://homolog.sefaz.go.gov.br/nfe/services/v2/CadConsultaCadastro2');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLMA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLPA(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLPB(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLPE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.pe.gov.br/nfe-service/services/CadConsultaCadastro2', '');
   else Result := CTeUtil.GetURLSP(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLPI(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLRJ(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLRN(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLRO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLRR(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSP(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLSC(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLSE(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLTO(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
   LayCTeCadastro: Result := IfThen(AAmbiente = 1, '', '');
   else Result := CTeUtil.GetURLSVRS(AAmbiente, ALayOut);
  end;
end;

class function CTeUtil.GetURLMG(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteRecepcao'          , 'https://hcte.fazenda.mg.gov.br/cte/services/CteRecepcao'); //?WSDL
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteRetRecepcao'       , 'https://hcte.fazenda.mg.gov.br/cte/services/CteRetRecepcao'); //?WSDL
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteCancelamento'      , 'https://hcte.fazenda.mg.gov.br/cte/services/CteCancelamento');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteInutilizacao'      , 'https://hcte.fazenda.mg.gov.br/cte/services/CteInutilizacao');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteConsulta'          , 'https://hcte.fazenda.mg.gov.br/cte/services/CteConsulta');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/CteStatusServico'     , 'https://hcte.fazenda.mg.gov.br/cte/services/CteStatusServico');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.mg.gov.br/nfe2/services/cadconsultacadastro2', 'https://hnfe.fazenda.mg.gov.br/nfe2/services/cadconsultacadastro2');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.mg.gov.br/cte/services/RecepcaoEvento'       , 'https://hcte.fazenda.mg.gov.br/cte/services/RecepcaoEvento');
  end;
end;

class function CTeUtil.GetURLRS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cterecepcao/CteRecepcao.asmx'                 , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cterecepcao/CteRecepcao.asmx');
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteretrecepcao/cteRetRecepcao.asmx'           , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteretrecepcao/cteRetRecepcao.asmx'); //CteRetRecepcao.asmx
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/ctecancelamento/ctecancelamento.asmx'         , 'https://homologacao.cte.sefaz.rs.gov.br/ws/ctecancelamento/ctecancelamento.asmx');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteinutilizacao/cteinutilizacao.asmx'         , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteinutilizacao/cteinutilizacao.asmx');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteconsulta/CteConsulta.asmx'                 , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cteconsulta/CteConsulta.asmx');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/ctestatusservico/CteStatusServico.asmx'       , 'https://homologacao.cte.sefaz.rs.gov.br/ws/ctestatusservico/CteStatusServico.asmx');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://sef.sefaz.rs.gov.br/ws/cadconsultacadastro/cadconsultacadastro2.asmx', 'https://sef.sefaz.rs.gov.br/ws/cadconsultacadastro/cadconsultacadastro2.asmx');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.rs.gov.br/ws/cteRecepcaoEvento/cteRecepcaoEvento.asmx'     , 'https://homologacao.cte.sefaz.rs.gov.br/ws/cterecepcaoevento/cterecepcaoevento.asmx');
  end;
end;

// As URLs da SEFAZ de SP são as mesmas para o SVSP = SEFAZ Virtual de SP
class function CTeUtil.GetURLSP(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteRecepcao.asmx'         , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteRecepcao.asmx');
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteRetRecepcao.asmx'      , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteRetRecepcao.asmx');
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteCancelamento.asmx'     , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteCancelamento.asmx');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteInutilizacao.asmx'     , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteInutilizacao.asmx');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteConsulta.asmx'         , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteConsulta.asmx');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteStatusServico.asmx'    , 'https://homologacao.nfe.fazenda.sp.gov.br/cteWEB/services/cteStatusServico.asmx');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/nfeweb/services/cadconsultacadastro2.asmx', 'https://homologacao.nfe.fazenda.sp.gov.br/nfeweb/services/cadconsultacadastro2.asmx');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://nfe.fazenda.sp.gov.br/cteWEB/services/cteRecepcaoEvento.asmx'   , 'https://homologacao.nfe.fazenda.sp.gov.br/cteweb/services/cteRecepcaoEvento.asmx');
  end;
end;

class function CTeUtil.GetURLMS(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteRecepcao.asmx'        , 'https://homologacao.cte.ms.gov.br/cteWEB/CteRecepcao.asmx');
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteRetRecepcao.asmx'     , 'https://homologacao.cte.ms.gov.br/cteWEB/CteRetRecepcao.asmx');
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteCancelamento.asmx'    , 'https://homologacao.cte.ms.gov.br/cteWEB/CteCancelamento.asmx');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteInutilizacao.asmx'    , 'https://homologacao.cte.ms.gov.br/cteWEB/CteInutilizacao.asmx');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteConsulta.asmx'        , 'https://homologacao.cte.ms.gov.br/cteWEB/CteConsulta.asmx');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CteStatusServico.asmx'   , 'https://homologacao.cte.ms.gov.br/cteWEB/CteStatusServico.asmx');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/CadConsultaCadastro.asmx', 'https://homologacao.cte.ms.gov.br/cteWEB/CadConsultaCadastro.asmx');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://producao.cte.ms.gov.br/cteWEB/cteRecepcaoEvento.asmx'  , 'https://homologacao.cte.ms.gov.br/cteWEB/CteRecepcaoEvento.asmx');
  end;
end;

class function CTeUtil.GetURLMT(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteRecepcao'            , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteRecepcao'); //?WSDL
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteRetRecepcao'         , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteRetRecepcao'); //?WSDL
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteCancelamento'        , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteCancelamento');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteInutilizacao'        , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteInutilizacao');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteConsulta'            , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteConsulta');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews/services/CteStatusServico'       , 'https://homologacao.sefaz.mt.gov.br/ctews/services/CteStatusServico'); //?WSDL
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://nfe.sefaz.mt.gov.br/nfews/v2/services/CadConsultaCadastro2', 'https://homologacao.sefaz.mt.gov.br/nfews/v2/services/CadConsultaCadastro2');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.sefaz.mt.gov.br/ctews2/services/CteRecepcaoEvento'     , 'https://homologacao.sefaz.mt.gov.br/ctews2/services/CteRecepcaoEvento');
  end;
end;

class function CTeUtil.GetURLPR(AAmbiente: Integer; ALayOut: TLayOutCTe): WideString;
begin
  case ALayOut of
    LayCTeRecepcao:      Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteRecepcao'          , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteRecepcao'); //?wsdl
    LayCTeRetRecepcao:   Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteRetRecepcao'       , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteRetRecepcao');
    LayCTeCancelamento:  Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteCancelamento'      , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteCancelamento');
    LayCTeInutilizacao:  Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteInutilizacao'      , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteInutilizacao');
    LayCTeConsultaCT:    Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteConsulta'          , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteConsulta');
    LayCTeStatusServico: Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteStatusServico'     , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteStatusServico');
    LayCTeCadastro:      Result := IfThen(AAmbiente = 1, 'https://nfe2.fazenda.pr.gov.br/nfe/CadConsultaCadastro2', 'https://homologacao.nfe2.fazenda.pr.gov.br/nfe/CadConsultaCadastro2');
    LayCTeEvento:        Result := IfThen(AAmbiente = 1, 'https://cte.fazenda.pr.gov.br/cte/CteRecepcaoEvento'    , 'https://homologacao.cte.fazenda.pr.gov.br/cte/CteRecepcaoEvento');
  end;
end;

class function CTeUtil.FormatarNumCTe(const AValue: Integer): String;
begin
  result := FormatFloat('000000000', AValue);
end;

class function CTeUtil.FormatarValor(mask: TpcteMask; const AValue: real): String;
begin
  result := FormatFloat(TpMaskToStrText(mask), AValue);
end;

class function CTeUtil.FormatarChaveAcesso(AValue: String; Mascara: Boolean = False ): String;
begin
  AValue := OnlyNumber(AValue);
  if Mascara
   then Result := copy(AValue,1,2)  + '-' + copy(AValue,3,2)  + '/' +
                  copy(AValue,5,2)  + '-' + copy(AValue,7,2)  + '.' +
                  copy(AValue,9,3)  + '.' + copy(AValue,12,3) + '/' +
                  copy(AValue,15,4) + '-' + copy(AValue,19,2) + '-' +
                  copy(AValue,21,2) + '-' + copy(AValue,23,3) + '-' +
                  copy(AValue,26,3) + '.' + copy(AValue,29,3) + '.' +
                  copy(AValue,32,3) + '-' + copy(AValue,35,1) + '-' +
                  copy(AValue,36,2) + '.' + copy(AValue,38,3) + '.' +
                  copy(AValue,41,3) + '-' + copy(AValue,44,1)
   else Result := copy(AValue,1,4)  + ' ' + copy(AValue,5,4)  + ' ' +
                  copy(AValue,9,4)  + ' ' + copy(AValue,13,4) + ' ' +
                  copy(AValue,17,4) + ' ' + copy(AValue,21,4) + ' ' +
                  copy(AValue,25,4) + ' ' + copy(AValue,29,4) + ' ' +
                  copy(AValue,33,4) + ' ' + copy(AValue,37,4) + ' ' +
                  copy(AValue,41,4);
end;

{$IFDEF ACBrCTeOpenSSL}
function ValidaLibXML(const AXML: AnsiString;
  var AMsg: AnsiString; const APathSchemas: String = ''): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  schema_filename: PChar;
  Tipo, I: Integer;
begin
  Tipo := CTeUtil.IdentificaTipoSchema(AXML, I);

  if not DirectoryExists(IfThen(EstaVazio(APathSchemas),
                 PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                 PathWithDelim(APathSchemas))) then
    raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                           IfThen(EstaVazio(APathSchemas),
                           PathWithDelim(ExtractFileDir(application.ExeName))+
                           'Schemas',PathWithDelim(APathSchemas)));

{$IFDEF PL_103}
  case Tipo of
   1: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\cte_v1.03.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'cte_v1.03.xsd');
      end;
   2: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\canccte_v1.03.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'canccte_v1.03.xsd');
      end;
   3: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\inutcte_v1.03.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'inutcte_v1.03.xsd');
      end;
   else schema_filename := '';
  end;
{$ENDIF}
{$IFNDEF PL_103}
  case Tipo of
   1: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\cte_v' + CTeenviCTe + '.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'cte_v' + CTeenviCTe + '.xsd');
      end;
   2: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\canccte_v' + CTecancCTe + '.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'canccte_v' + CTecancCTe + '.xsd');
      end;
   3: begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\inutcte_v' + CTeinutCTe + '.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'inutcte_v' + CTeinutCTe + '.xsd');
      end;
   4: begin
       {
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\envDPEC_v1.04.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'envDPEC_v1.04.xsd');
       }
       schema_filename := '';
      end;
   5..11:
      begin
       if EstaVazio(APathSchemas) then
         schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\eventoCTe_v' + CTeEventoCTe + '.xsd')
        else
         schema_filename := pchar(PathWithDelim(APathSchemas)+'eventoCTe_v' + CTeEventoCTe + '.xsd');
      end;
   else schema_filename := '';
  end;
{$ENDIF}

  if not FileExists(schema_filename) then
    raise Exception.Create('Arquivo de Schema não encontrado' + sLineBreak + schema_filename);

//  doc         := nil;
//  schema_doc  := nil;
//  parser_ctxt := nil;
//  schema      := nil;
//  valid_ctxt  := nil;
  doc         := xmlParseDoc(PAnsiChar(Axml));

  if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
  begin
    AMsg := 'Erro: unable to parse';
    Result := False;
    exit;
  end;

  schema_doc := xmlReadFile(Pansichar(AnsiString(ACBrStr(schema_filename))), nil, XML_DETECT_IDS);

  //  the schema cannot be loaded or is not well-formed
  if (schema_doc = nil) then
  begin
    AMsg := 'Erro: Schema não pode ser carregado ou está corrompido';
    Result := False;
    exit;
  end;

  parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
  // unable to create a parser context for the schema */
  if (parser_ctxt = nil) then
  begin
    xmlFreeDoc(schema_doc);
    AMsg := 'Erro: unable to create a parser context for the schema';
    Result := False;
    exit;
  end;

  schema := xmlSchemaParse(parser_ctxt);
  // the schema itself is not valid
  if (schema = nil) then
  begin
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    AMsg := 'Error: the schema itself is not valid';
    Result := False;
    exit;
  end;

  valid_ctxt := xmlSchemaNewValidCtxt(schema);
  //   unable to create a validation context for the schema */
  if (valid_ctxt = nil) then
  begin
    xmlSchemaFree(schema);
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    AMsg := 'Error: unable to create a validation context for the schema';
    Result := False;
    exit;
  end;

  if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
  begin
    schemError := xmlGetLastError();
    AMsg := IntToStr(schemError^.code) + ' - ' + schemError^.message;
    Result := False;
    exit;
  end;

  xmlSchemaFreeValidCtxt(valid_ctxt);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(parser_ctxt);
  xmlFreeDoc(schema_doc);
  Result := True;
end;

function ValidaModalLibXML(XML: AnsiString; var Msg: AnsiString;
 const APathSchemas: String = ''): Boolean;
{$IFNDEF PL_103}
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  schema_filename: String; // PChar;
  Tipo: Integer;
  AXML: AnsiString;
{$ENDIF}
begin
{$IFDEF PL_103}
  Result := True;
{$ENDIF}

{$IFNDEF PL_103}
  Tipo := 0;
  AXML := XML;

  XML := SeparaDados( XML, 'infModal' );

  if pos( '<aereo>', XML ) <> 0
   then begin
    Tipo := 1;
    XML := SeparaDados( XML, 'aereo' );
    XML := '<aereo xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</aereo>';
   end;
  if pos( '<aquav>', XML) <> 0
   then begin
    Tipo := 2;
    XML := SeparaDados( XML, 'aquav' );
    XML := '<aquav xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</aquav>';
   end;
  if pos( '<duto>', XML) <> 0
   then begin
    Tipo := 3;
    XML := SeparaDados( XML, 'duto' );
    XML := '<duto xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</duto>';
   end;
  if pos( '<ferrov>', XML) <> 0
   then begin
    Tipo := 4;
    XML := SeparaDados( XML, 'ferrov' );
    XML := '<ferrov xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</ferrov>';
   end;
  if pos( '<rodo>', XML) <> 0
   then begin
    Tipo := 5;
    XML := SeparaDados( XML, 'rodo' );
    XML := '<rodo xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</rodo>';
   end;
  if pos( '<multimodal>', XML) <> 0
   then begin
    Tipo := 6;
    XML := SeparaDados( XML, 'multimodal' );
    XML := '<multimodal xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</multimodal>';
   end;

  // Eventos
  if Tipo = 0
   then begin
    XML := AXML;
    if pos( '<evEPECCTe>', XML) <> 0
     then begin
      Tipo := 7;
      XML := SeparaDados( XML, 'evEPECCTe' );
      XML := '<evEPECCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evEPECCTe>';
     end;
    if pos( '<evCancCTe>', XML) <> 0
     then begin
      Tipo := 8;
      XML := SeparaDados( XML, 'evCancCTe' );
      XML := '<evCancCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evCancCTe>';
     end;
    if pos( '<evRegMultimodal>', XML) <> 0
     then begin
      Tipo := 9;
      XML := SeparaDados( XML, 'evRegMultimodal' );
      XML := '<evRegMultimodal xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evRegMultimodal>';
     end;
    if pos( '<evCCeCTe>', XML) <> 0
     then begin
      Tipo := 10;
      XML := SeparaDados( XML, 'evCCeCTe' );
      XML := '<evCCeCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evCCeCTe>';
     end;
   end;

  XML := '<?xml version="1.0" encoding="UTF-8" ?>' + XML;

  if Tipo = 0 then
    raise Exception.Create('Modal não encontrado no XML.');


  if not DirectoryExists(IfThen(EstaVazio(APathSchemas),
                 PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                 PathWithDelim(APathSchemas))) then
    raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                           IfThen(EstaVazio(APathSchemas),
                           PathWithDelim(ExtractFileDir(application.ExeName))+
                           'Schemas',PathWithDelim(APathSchemas)));

  case Tipo of
   1: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteModalAereo_v' + CTeModalAereo + '.xsd');
      end;
   2: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteModalAquaviario_v' + CTeModalAqua + '.xsd');
      end;
   3: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteModalDutoviario_v' + CTeModalDuto + '.xsd');
      end;
   4: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteModalFerroviario_v' + CTeModalFerro + '.xsd');
      end;
   5: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteModalRodoviario_v' + CTeModalRodo + '.xsd');
      end;
   6: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cteMultiModal_v' + CTeMultiModal + '.xsd');
      end;
   7: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evEPECCTe_v' + CTeEventoCTe + '.xsd');
      end;
   8: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evCancCTe_v' + CTeEventoCTe + '.xsd');
      end;
   9: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evRegMultimodal_v' + CTeEventoCTe + '.xsd');
      end;
  10: begin
       schema_filename := {pchar}(IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evCCeCTe_v' + CTeEventoCTe + '.xsd');
      end;
  end;

  doc         := nil;
  schema_doc  := nil;
  parser_ctxt := nil;
  schema      := nil;
  valid_ctxt  := nil;
  doc         := xmlParseDoc(PAnsiChar(XML));

  if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
  begin
    Msg := 'Erro: unable to parse';
    Result := False;
    exit;
  end;

  schema_doc := xmlReadFile(Pansichar(AnsiString(ACBrStr(schema_filename))), nil, XML_DETECT_IDS);

  //  the schema cannot be loaded or is not well-formed
  if (schema_doc = nil) then
  begin
    Msg := 'Erro: Schema não pode ser carregado ou está corrompido';
    Result := False;
    exit;
  end;

  parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
  // unable to create a parser context for the schema */
  if (parser_ctxt = nil) then
  begin
    xmlFreeDoc(schema_doc);
    Msg := 'Erro: unable to create a parser context for the schema';
    Result := False;
    exit;
  end;

  schema := xmlSchemaParse(parser_ctxt);
  // the schema itself is not valid
  if (schema = nil) then
  begin
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    Msg := 'Error: the schema itself is not valid';
    Result := False;
    exit;
  end;

  valid_ctxt := xmlSchemaNewValidCtxt(schema);
  //   unable to create a validation context for the schema */
  if (valid_ctxt = nil) then
  begin
    xmlSchemaFree(schema);
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    Msg := 'Error: unable to create a validation context for the schema';
    Result := False;
    exit;
  end;

  if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
  begin
    schemError := xmlGetLastError();
    Msg := IntToStr(schemError^.code) + ' - ' + schemError^.message;
    Result := False;
    exit;
  end;

  xmlSchemaFreeValidCtxt(valid_ctxt);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(parser_ctxt);
  xmlFreeDoc(schema_doc);
  Result := True;
{$ENDIF}
end;

function ValidaAssinaturaLibXML(const Axml: PAnsiChar; out Msg: AnsiString): Boolean;
begin
  Result := False;
end;
{$ELSE}
function ValidaMSXML(XML: AnsiString; out Msg: AnsiString;
 const APathSchemas: String = ''): Boolean;
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache;
  Tipo, I: Integer;
  ArqSchema: String;
begin
  CoInitialize(nil);
  try
    Tipo := CTeUtil.IdentificaTipoSchema(XML, I);

    DOMDocument                  := CoDOMDocument50.Create;
    DOMDocument.async            := False;
    DOMDocument.resolveExternals := False;
    DOMDocument.validateOnParse  := True;
    DOMDocument.loadXML(XML);

    Schema := CoXMLSchemaCache50.Create;

    if not DirectoryExists(IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                    PathWithDelim(APathSchemas))) then
      raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                              IfThen(EstaVazio(APathSchemas),
                              PathWithDelim(ExtractFileDir(application.ExeName))+
                              'Schemas',PathWithDelim(APathSchemas)));

{$IFDEF PL_103}
    Schema.remove('http://www.portalfiscal.inf.br/cte');

    case Tipo of
     1: begin
         Schema.add('http://www.portalfiscal.inf.br/cte',
          IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cte_v1.03.xsd')
        end;
     2: begin
         Schema.add('http://www.portalfiscal.inf.br/cte',
          IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'cancCte_v1.03.xsd')
        end;
     3: begin
         Schema.add('http://www.portalfiscal.inf.br/cte',
          IfThen(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'inutCte_v1.03.xsd')
        end;
    end;
{$ENDIF}

{$IFNDEF PL_103}
    Schema.remove('http://www.portalfiscal.inf.br/cte');

    case Tipo of
     1: begin
         ArqSchema := IfThen(EstaVazio(APathSchemas),
                      PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                      PathWithDelim(APathSchemas)) + 'cte_v' + CTeenviCTe + '.xsd';
        end;
     2: begin
         ArqSchema := IfThen(EstaVazio(APathSchemas),
                      PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                      PathWithDelim(APathSchemas)) + 'cancCte_v' + CTecancCTe + '.xsd';
        end;
     3: begin
         ArqSchema := IfThen(EstaVazio(APathSchemas),
                      PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                      PathWithDelim(APathSchemas)) + 'inutCte_v' + CTeinutCTe + '.xsd';
        end;
      5..11:
        begin
         ArqSchema := IfThen(EstaVazio(APathSchemas),
                      PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                      PathWithDelim(APathSchemas)) + 'eventoCTe_v' + CTeEventoCTe + '.xsd';
        end;
    end;

    if not FileExists(ArqSchema) then
      raise Exception.Create('Arquivo de Schema não encontrado:' + sLineBreak + ArqSchema);

    Schema.add('http://www.portalfiscal.inf.br/cte', ArqSchema);
{$ENDIF}

    DOMDocument.schemas := Schema;
    ParseError          := DOMDocument.validate;
    Result              := (ParseError.errorCode = 0);
    Msg                 := ParseError.reason;
    DOMDocument         := nil;
    ParseError          := nil;
    Schema              := nil;
  finally
    CoUninitialize;
  end;
end;

function ValidaModalMSXML(XML: AnsiString; out Msg: AnsiString;
 const APathSchemas: String = ''): Boolean;
{$IFNDEF PL_103}
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache;
  Tipo: Integer;
  AXML: AnsiString;
  ArqSchema: String;
{$ENDIF}
begin
  CoInitialize(nil);
{$IFDEF PL_103}
  Result := True;
{$ENDIF}

{$IFNDEF PL_103}
  Tipo := 0;
  AXML := XML;

  XML := SeparaDados( XML, 'infModal' );

  if pos( '<aereo>', XML ) <> 0
   then begin
    Tipo := 1;
    XML := SeparaDados( XML, 'aereo' );
    XML := '<aereo xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</aereo>';
   end;
  if pos( '<aquav>', XML) <> 0
   then begin
    Tipo := 2;
    XML := SeparaDados( XML, 'aquav' );
    XML := '<aquav xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</aquav>';
   end;
  if pos( '<duto>', XML) <> 0
   then begin
    Tipo := 3;
    XML := SeparaDados( XML, 'duto' );
    XML := '<duto xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</duto>';
   end;
  if pos( '<ferrov>', XML) <> 0
   then begin
    Tipo := 4;
    XML := SeparaDados( XML, 'ferrov' );
    XML := '<ferrov xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</ferrov>';
   end;
  if pos( '<rodo>', XML) <> 0
   then begin
    Tipo := 5;
    XML := SeparaDados( XML, 'rodo' );
    XML := '<rodo xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</rodo>';
   end;
  if pos( '<multimodal>', XML) <> 0
   then begin
    Tipo := 6;
    XML := SeparaDados( XML, 'multimodal' );
    XML := '<multimodal xmlns="http://www.portalfiscal.inf.br/cte">' +
            XML +
           '</multimodal>';
   end;

  // Eventos
  if Tipo = 0
   then begin
    XML := AXML;
    if pos( '<evEPECCTe>', XML) <> 0
     then begin
      Tipo := 7;
      XML := SeparaDados( XML, 'evEPECCTe' );
      XML := '<evEPECCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evEPECCTe>';
     end;
    if pos( '<evCancCTe>', XML) <> 0
     then begin
      Tipo := 8;
      XML := SeparaDados( XML, 'evCancCTe' );
      XML := '<evCancCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evCancCTe>';
     end;
    if pos( '<evRegMultimodal>', XML) <> 0
     then begin
      Tipo := 9;
      XML := SeparaDados( XML, 'evRegMultimodal' );
      XML := '<evRegMultimodal xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evRegMultimodal>';
     end;
    if pos( '<evCCeCTe>', XML) <> 0
     then begin
      Tipo := 10;
      XML := SeparaDados( XML, 'evCCeCTe' );
      XML := '<evCCeCTe xmlns="http://www.portalfiscal.inf.br/cte">' +
              XML +
             '</evCCeCTe>';
     end;
   end;

  XML := '<?xml version="1.0" encoding="UTF-8" ?>' + XML;

  if Tipo = 0 then
    raise Exception.Create('Modal não encontrado no XML.');

  DOMDocument                  := CoDOMDocument50.Create;
  DOMDocument.async            := False;
  DOMDocument.resolveExternals := False;
  DOMDocument.validateOnParse  := True;
  DOMDocument.loadXML(XML);

  Schema := CoXMLSchemaCache50.Create;

  if not DirectoryExists(IfThen(EstaVazio(APathSchemas),
                  PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                  PathWithDelim(APathSchemas))) then
    raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                            IfThen(EstaVazio(APathSchemas),
                            PathWithDelim(ExtractFileDir(application.ExeName))+
                            'Schemas',PathWithDelim(APathSchemas)));

  Schema.remove('http://www.portalfiscal.inf.br/cte');

  case Tipo of
   1: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteModalAereo_v' + CTeModalAereo + '.xsd';
      end;
   2: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteModalAquaviario_v' + CTeModalAqua + '.xsd';
      end;
   3: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteModalDutoviario_v' + CTeModalDuto + '.xsd';
      end;
   4: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteModalFerroviario_v' + CTeModalFerro + '.xsd';
      end;
   5: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteModalRodoviario_v' + CTeModalRodo + '.xsd';
      end;
   6: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'cteMultiModal_v' + CTeMultiModal + '.xsd';
      end;
   7: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'evEPECCTe_v' + CTeEventoCTe + '.xsd';
      end;
   8: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'evCancCTe_v' + CTeEventoCTe + '.xsd';
      end;
   9: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'evRegMultimodal_v' + CTeEventoCTe + '.xsd';
      end;
  10: begin
       ArqSchema := IfThen(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                    PathWithDelim(APathSchemas)) + 'evCCeCTe_v' + CTeEventoCTe + '.xsd';
      end;
  end;

  if not FileExists(ArqSchema) then
    raise Exception.Create('Arquivo de Schema não encontrado:' + sLineBreak + ArqSchema);

  Schema.add('http://www.portalfiscal.inf.br/cte', ArqSchema);

  DOMDocument.schemas := Schema;
  ParseError          := DOMDocument.validate;
  Result              := (ParseError.errorCode = 0);
  Msg                 := ParseError.reason;
  DOMDocument         := nil;
  ParseError          := nil;
  Schema              := nil;
{$ENDIF}
  CoUninitialize;
end;

function ValidaAssinaturaMSXML(XML: AnsiString; out Msg: AnsiString): Boolean;
var
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  pKeyInfo: IXMLDOMNode;
  pKey, pKeyOut: IXMLDSigKey;
begin
  CoInitialize(nil);
  xmldoc := CoDOMDocument50.Create;
  xmldsig := CoMXDigitalSignature50.Create;

  xmldoc.async              := False;
  xmldoc.validateOnParse    := False;
  xmldoc.preserveWhiteSpace := True;

   if (not xmldoc.loadXML(XML) ) then
      raise Exception.Create('Não foi possível carregar o arquivo: '+XML);
  try
    xmldoc.setProperty('SelectionNamespaces', DSIGNS);
    xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

   if (xmldsig.signature = nil ) then
      raise Exception.Create('Não foi possível carregar o ler a assinatura: '+XML);

    pKeyInfo := xmldoc.selectSingleNode('.//ds:KeyInfo/ds:X509Data');

    pKey := xmldsig.createKeyFromNode(pKeyInfo);

    try
      pKeyOut := xmldsig.verify(pKey);
    except
       on E: Exception do
          Msg := 'Erro ao verificar assinatura do arquivo: '+ E.Message;
    end;
  finally
    Result := (pKeyOut <> nil );

    pKeyOut := nil;
    pKey := nil;
    pKeyInfo := nil;
    xmldsig := nil;
    xmldoc := nil;
  end;
  CoUninitialize;
end;
{$ENDIF}

class function CTeUtil.Valida(const AXML: AnsiString;
  var AMsg: AnsiString; const APathSchemas: String = ''): Boolean;
begin
{$IFDEF ACBrCTeOpenSSL}
//  Result := ValidaLibXML(AXML, AMsg, APathSchemas);
  if (pos('<infCTeNorm>', AXML) <> 0) or (pos('<infEvento', AXML) <> 0)
   then Result := ValidaLibXML(AXML, AMsg, APathSchemas) and
                  ValidaModalLibXML(AXML, AMsg, APathSchemas)
   else Result := ValidaLibXML(AXML, AMsg, APathSchemas);
{$ELSE}
  if (pos('<infCTeNorm>', AXML) <> 0) or (pos('<infEvento', AXML) <> 0)
   then Result := ValidaMSXML(AXML, AMsg, APathSchemas) and
                  ValidaModalMSXML(AXML, AMsg, APathSchemas)
   else Result := ValidaMSXML(AXML, AMsg, APathSchemas);
{$ENDIF}
end;

class function CTeUtil.ValidaAssinatura(const AXML: AnsiString;
  var AMsg: AnsiString): Boolean;
begin
{$IFDEF ACBrCTeOpenSSL}
  Result := ValidaAssinaturaLibXML(PAnsiChar(AXML),AMsg);
{$ELSE}
  Result := ValidaAssinaturaMSXML(AXML,AMsg);
{$ENDIF}
end;

{$IFDEF ACBrCTeOpenSSL}
function AssinarLibXML(const AXML, ArqPFX, PFXSenha: AnsiString;
  out AXMLAssinado, FMensagem: AnsiString): Boolean;
var
  I, J, PosIni, PosFim: Integer;
  URI, AStr, XmlAss: AnsiString;
  Tipo: Integer;  // 1 - CTe 2 - Cancelamento 3 - Inutilizacao
  Cert: TMemoryStream;
  Cert2: TStringStream;
begin
  AStr := AXML;

  //// Encontrando o URI ////
  Tipo := CTeUtil.IdentificaTipoSchema(AStr, I);

  I := PosEx('Id=', AStr, I + 6);
  if I = 0 then
    raise Exception.Create('Não encontrei inicio do URI: Id=');
  I := PosEx('"', AStr, I + 2);
  if I = 0 then
    raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
  J := PosEx('"', AStr, I + 1);
  if J = 0 then
    raise Exception.Create('Não encontrei inicio do URI: aspas final');

  URI := copy(AStr, I + 1, J - I - 1);

  //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
  I := pos('?>', AStr);

  case Tipo of
   1: AStr := copy(AStr, 1, StrToInt(VarToStr(IfThen(I > 0, I + 1, I))))
         + cDTD + Copy(AStr, StrToInt(VarToStr(IfThen(I > 0, I + 2, I))),
          Length(AStr));
   2: AStr := copy(AStr, 1, StrToInt(VarToStr(IfThen(I > 0, I + 1, I))))
         + cDTDCanc + Copy(AStr, StrToInt(VarToStr(IfThen(I > 0, I + 2, I))),
          Length(AStr));
   3: AStr := copy(AStr, 1, StrToInt(VarToStr(IfThen(I > 0, I + 1, I))))
         + cDTDInut + Copy(AStr, StrToInt(VarToStr(IfThen(I > 0, I + 2, I))),
          Length(AStr));
   {
   4: AStr := copy(AStr,1,StrToInt(VarToStr(IfThen(I>0,I+1,I))))
         + cDTDDpec + Copy(AStr,StrToInt(VarToStr(IfThen(I>0,I+2,I))),
          Length(AStr));
   }
   5..11: AStr := Copy(AStr, 1, StrToInt(VarToStr(IfThen(I > 0, I + 1, I))))
             + cDTDEven + Copy(AStr, StrToInt(VarToStr(IfThen(I > 0, I + 2, I))),
              Length(AStr));
   else AStr := '';
  end;

  //// Inserindo Template da Assinatura digital ////
  case Tipo of
   1: begin
       I := pos('</CTe>', AStr);
       if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </CTe>');
      end;
   2: begin
       I := pos('</cancCTe>', AStr);
       if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </cancCTe>');
      end;
   3: begin
       I := pos('</inutCTe>', AStr);
       if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </inutCTe>');
      end;
   5..11:
      begin
       I := pos('</eventoCTe>', AStr) ;
       if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </eventoCTe>') ;
      end;
   else
      raise Exception.Create('Template de Tipo não implementado.') ;
  end;

  if pos('<Signature', AStr) > 0 then
    I := pos('<Signature', AStr);

  AStr := copy(AStr, 1, I - 1) +
    '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
      '<SignedInfo>' +
        '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
        '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
        '<Reference URI="#' + URI + '">' +
          '<Transforms>' +
            '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
            '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
          '</Transforms>' +
          '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
          '<DigestValue></DigestValue>' +
        '</Reference>' +
      '</SignedInfo>' +
      '<SignatureValue></SignatureValue>' +
      '<KeyInfo>' +
        '<X509Data>' +
          '<X509Certificate></X509Certificate>' +
        '</X509Data>' +
      '</KeyInfo>' +
    '</Signature>';

  case Tipo of
    1: AStr := AStr + '</CTe>';
    2: AStr := AStr + '</cancCTe>';
    3: AStr := AStr + '</inutCTe>';
    5..11: AStr := AStr + '</eventoCTe>';
    else AStr := '';
  end;

  if FileExists(ArqPFX) then
    XmlAss := CTeUtil.sign_file(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha))
  else
   begin
    Cert := TMemoryStream.Create;
    Cert2 := TStringStream.Create(ArqPFX);
    try
      Cert.LoadFromStream(Cert2);
      XmlAss := CTeUtil.sign_memory(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha), Cert.Size, Cert.Memory) ;
    finally
      Cert2.Free;
      Cert.Free;
    end;
  end;

  // Removendo quebras de linha //
  XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
  XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

  // Removendo DTD //
  case Tipo of
    1: XmlAss := StringReplace( XmlAss, cDTD, '', [] );
    2: XmlAss := StringReplace( XmlAss, cDTDCanc, '', [] );
    3: XmlAss := StringReplace( XmlAss, cDTDInut, '', [] );
    5..11: XmlAss := StringReplace( XmlAss, cDTDEven, '', [] );
    else XmlAss := '';
  end;

  PosIni := Pos('<X509Certificate>', XmlAss) - 1;
  PosFim := PosLast('<X509Certificate>', XmlAss);

  XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));

  AXMLAssinado := XmlAss;

  Result := True;
end;
{$ELSE}
function AssinarMSXML(XML: AnsiString; Certificado: ICertificate2; out XMLAssinado: AnsiString): Boolean;
var
  I, J, PosIni, PosFim: Integer;
  URI: String;
  Tipo: Integer;
  xmlHeaderAntes, xmlHeaderDepois: AnsiString;
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  signedKey: IXMLDSigKey;
begin
  CoInitialize(nil);
  try
   if Pos('<Signature', XML) <= 0 then
   begin
     Tipo := CTeUtil.IdentificaTipoSchema(XML,I);

     I := PosEx('Id=', XML, 6);
     if I = 0 then
       raise Exception.Create('Não encontrei inicio do URI: Id=');
     I := PosEx('"', XML, I + 2);
     if I = 0 then
       raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
     J := PosEx('"', XML, I + 1);
     if J = 0 then
       raise Exception.Create('Não encontrei inicio do URI: aspas final');

     URI := copy(XML, I + 1, J - I - 1);

     case Tipo of
       1: XML := copy(XML, 1, pos('</CTe>', XML) - 1);
       2: XML := copy(XML, 1, pos('</cancCTe>', XML) - 1);
       3: XML := copy(XML, 1, pos('</inutCTe>', XML) - 1);
       5..11: XML := copy(XML, 1, pos('</eventoCTe>', XML) - 1);
       else XML := '';
     end;

     XML := XML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
                    '<SignedInfo>' +
                      '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
                      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
                      '<Reference URI="#' + URI + '">' +
                        '<Transforms>' +
                          '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
                          '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
                        '</Transforms>' +
                        '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
                        '<DigestValue></DigestValue>' +
                      '</Reference>' +
                    '</SignedInfo>' +
                    '<SignatureValue></SignatureValue>' +
                    '<KeyInfo></KeyInfo>' +
                  '</Signature>';

     case Tipo of
       1: XML := XML + '</CTe>';
       2: XML := XML + '</cancCTe>';
       3: XML := XML + '</inutCTe>';
       5..11: XML := XML + '</eventoCTe>';
       else XML := '';
     end;
   end;

   // Lendo Header antes de assinar //
   xmlHeaderAntes := '';
   I := pos('?>', XML);
   if I > 0 then
     xmlHeaderAntes := copy(XML, 1, I + 1);

   xmldoc := CoDOMDocument50.Create;

   xmldoc.async := False;
   xmldoc.validateOnParse := False;
   xmldoc.preserveWhiteSpace := True;

   xmldsig := CoMXDigitalSignature50.Create;

   if (not xmldoc.loadXML(XML)) then
     raise Exception.Create('Não foi possível carregar o arquivo: ' + XML);

   xmldoc.setProperty('SelectionNamespaces', DSIGNS);

   xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

   if (xmldsig.signature = nil) then
     raise Exception.Create('Falha ao setar assinatura.');

   if (xmldsig.signature = nil) then
     raise Exception.Create('É preciso carregar o template antes de assinar.');

    if NumCertCarregado <> Certificado.SerialNumber then
      CertStoreMem := nil;

   if CertStoreMem = nil then
   begin
     CertStore := CoStore.Create;
     CertStore.Open(CAPICOM_CURRENT_USER_STORE, 'My', CAPICOM_STORE_OPEN_READ_ONLY);

     CertStoreMem := CoStore.Create;
     CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'Memoria', CAPICOM_STORE_OPEN_READ_ONLY);

     Certs := CertStore.Certificates as ICertificates2;
     for i := 1 to Certs.Count do
     begin
       Cert := IInterface(Certs.Item[i]) as ICertificate2;
       if Cert.SerialNumber = Certificado.SerialNumber then
        begin
          CertStoreMem.Add(Cert);
          NumCertCarregado := Certificado.SerialNumber;
        end;
     end;
   end;

   OleCheck(IDispatch(Certificado.PrivateKey).QueryInterface(IPrivateKey, PrivateKey));
   xmldsig.store := CertStoreMem;

   dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
   if (dsigKey = nil) then
     raise Exception.Create('Erro ao criar a chave do CSP.');

   signedKey := xmldsig.sign(dsigKey, $00000002);
   if (signedKey <> nil) then
   begin
     XMLAssinado := xmldoc.xml;
     XMLAssinado := StringReplace(XMLAssinado, #10, '', [rfReplaceAll]);
     XMLAssinado := StringReplace(XMLAssinado, #13, '', [rfReplaceAll]);
     PosIni := Pos('<SignatureValue>', XMLAssinado) + length('<SignatureValue>');
     XMLAssinado := copy(XMLAssinado, 1, PosIni - 1) + StringReplace(copy(XMLAssinado, PosIni, length(XMLAssinado)), ' ', '', [rfReplaceAll]);
     PosIni := Pos('<X509Certificate>', XMLAssinado) - 1;
     PosFim := PosLast('<X509Certificate>', XMLAssinado);

     XMLAssinado := copy(XMLAssinado, 1, PosIni) + copy(XMLAssinado, PosFim, length(XMLAssinado));
   end
   else
     raise Exception.Create('Assinatura Falhou.');

   if xmlHeaderAntes <> '' then
   begin
     I := pos('?>', XMLAssinado);
     if I > 0 then
     begin
       xmlHeaderDepois := copy(XMLAssinado, 1, I + 1);
       if xmlHeaderAntes <> xmlHeaderDepois then
         XMLAssinado := StuffString(XMLAssinado, 1, length(xmlHeaderDepois), xmlHeaderAntes);
     end
     else
       XMLAssinado := xmlHeaderAntes + XMLAssinado;
   end;

   dsigKey := nil;
   signedKey := nil;
   xmldoc := nil;
   xmldsig := nil;

   Result := True;
  finally
   CoUninitialize;
  end;
end;
{$ENDIF}

{$IFDEF ACBrCTeOpenSSL}
class function CTeUtil.sign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: Integer;
label
  done;
begin
  doc := nil;
  // node := nil;
  dsigCtx := nil;
  result := '';

  if (Axml = nil) or (key_file = nil) then Exit;

  try
    { load template }
    doc := xmlParseDoc(Axml);
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
      raise Exception.Create('Error: unable to parse');

    { find start node }
    node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
    if (node = nil) then
      raise Exception.Create('Error: start node not found');

    { create signature context, we don't need keys manager in this example }
    dsigCtx := xmlSecDSigCtxCreate(nil);
    if (dsigCtx = nil) then
      raise Exception.Create('Error :failed to create signature context');

    // { load private key}
    dsigCtx^.signKey := xmlSecCryptoAppKeyLoad(key_file, xmlSecKeyDataFormatPkcs12, senha, nil, nil);
    if (dsigCtx^.signKey = nil) then
      raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

//    dsigCtx^.signKey := xmlSecCryptoAppKeyLoad(PAnsiChar(key_file), xmlSecKeyDataFormatPkcs12, PAnsiChar(senha), nil, nil);
//    if (dsigCtx^.signKey = nil) then
//      raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

    { set key name to the file name, this is just an example! }
    if (xmlSecKeySetName(dsigCtx^.signKey, PAnsiChar(key_file)) < 0) then
      raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

    { sign the template }
    if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
      raise Exception.Create('Error: signature failed');

    { print signed document to stdout }
    // xmlDocDump(stdout, doc);
    // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
    buffer := nil;
    xmlDocDumpMemory(doc, @buffer, @bufSize);
    if (buffer <> nil) then
      { success }
      result := buffer;
  finally
    { cleanup }
    if (dsigCtx <> nil) then
      xmlSecDSigCtxDestroy(dsigCtx);

    if (doc <> nil) then
      xmlFreeDoc(doc);
  end;
end;

class function CTeUtil.sign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: Integer;
label
 done;
begin
    doc := nil;
    //node := nil;
    dsigCtx := nil;
    result := '';

    if (Axml = nil) or (key_file = nil) then Exit;
    try
       { load template }
       doc := xmlParseDoc(Axml);
       if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
         raise Exception.Create('Error: unable to parse');

       { find start node }
       node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
       if (node = nil) then
         raise Exception.Create('Error: start node not found');

       { create signature context, we don't need keys manager in this example }
       dsigCtx := xmlSecDSigCtxCreate(nil);
       if (dsigCtx = nil) then
         raise Exception.Create('Error :failed to create signature context');

       // { load private key, assuming that there is not password }
       dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(Ponteiro, size, xmlSecKeyDataFormatPkcs12, senha, nil, nil);

       if (dsigCtx^.signKey = nil) then
          raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

       { set key name to the file name, this is just an example! }
       if (xmlSecKeySetName(dsigCtx^.signKey, key_file) < 0) then
         raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

       { sign the template }
       if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
         raise Exception.Create('Error: signature failed');

       { print signed document to stdout }
       // xmlDocDump(stdout, doc);
       // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
       buffer := nil;
       xmlDocDumpMemory(doc, @buffer, @bufSize);
       if (buffer <> nil) then
          { success }
          result := buffer ;
   finally
       { cleanup }
       if (dsigCtx <> nil) then
         xmlSecDSigCtxDestroy(dsigCtx);

       if (doc <> nil) then
         xmlFreeDoc(doc);
   end ;
end;
{$ENDIF}

{$IFDEF ACBrCTeOpenSSL}
class function CTeUtil.Assinar(const AXML, ArqPFX, PFXSenha: AnsiString; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ELSE}
class function CTeUtil.Assinar(const AXML: AnsiString; Certificado: ICertificate2; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ENDIF}
begin
{$IFDEF ACBrCTeOpenSSL}
  Result := AssinarLibXML(AXML, ArqPFX, PFXSenha, AXMLAssinado, FMensagem);
{$ELSE}
  Result := AssinarMSXML(AXML, Certificado, AXMLAssinado);
{$ENDIF}
end;

class function CTeUtil.UFtoCUF(UF: String): Integer;
var
  Codigo, i: Integer;
begin
  Codigo := -1 ;
  for i:= 0 to High(NFeUF) do
  begin
    if NFeUF[I] = UF then
      Codigo := NFeUFCodigo[I];
  end;

  if Codigo < 0 then
     Result := -1
  else
     Result := Codigo;
end;

class function CTeUtil.GerarChaveContingencia(FCTe:TCTe): String;

   function GerarDigito_Contingencia(var Digito: Integer; chave: String): Boolean;
   var
     i, j: Integer;
   const
     PESO = '43298765432987654329876543298765432';
   begin
     chave  := OnlyNumber(chave);
     j      := 0;
     Digito := 0;
     result := True;
     try
       for i := 1 to 35 do
         j := j + StrToInt(copy(chave, i, 1)) * StrToInt(copy(PESO, i, 1));
       Digito := 11 - (j mod 11);
       if (j mod 11) < 2 then
         Digito := 0;
     except
       result := False;
     end;
     if length(chave) <> 35 then
       result := False;
   end;

var
   wchave: String;
   wicms_s, wicms_p: String;
   wd,wm,wa: word;
   Digito: Integer;
begin
   // Alterado Conforme NT 2012/007
   // UF
   // TpcteTomador = ( tmRemetente, tmExpedidor, tmRecebedor, tmDestinatario, tmOutros);
   if FCTe.Ide.toma4.CNPJCPF<>''
    then begin
     if FCTe.Ide.toma4.enderToma.UF = 'EX'
      then wchave := '99' //exterior
      else wchave := copy(inttostr(FCTe.Ide.toma4.enderToma.cMun),1,2);
    end
    else begin
     case FCTe.Ide.toma03.Toma of
      tmRemetente: if FCTe.Rem.enderReme.UF = 'EX'
                    then wchave := '99' //exterior
                    else wchave := copy(inttostr(FCTe.Rem.enderReme.cMun),1,2);
      tmExpedidor: if FCTe.Exped.enderExped.UF = 'EX'
                    then wchave := '99' //exterior
                    else wchave := copy(inttostr(FCTe.Exped.enderExped.cMun),1,2);
      tmRecebedor: if FCTe.Receb.enderReceb.UF = 'EX'
                    then wchave := '99' //exterior
                    else wchave := copy(inttostr(FCTe.Receb.enderReceb.cMun),1,2);
      tmDestinatario: if FCTe.Dest.EnderDest.UF = 'EX'
                       then wchave := '99' //exterior
                       else wchave := copy(inttostr(FCTe.Dest.EnderDest.cMun),1,2);
     end;
    end;

   //TIPO DE EMISSAO
   case FCTe.Ide.tpEmis of
    teDPEC,
    teContingencia: wchave := wchave + '2';
    teFSDA:         wchave := wchave + '5';
    else            wchave := wchave + '0'; //esta valor caracteriza ERRO, valor tem q ser  2 ou 5
   end;

   //CNPJ OU CPF
   if FCTe.Ide.toma4.CNPJCPF<>''
    then begin
     if FCTe.Ide.toma4.enderToma.UF = 'EX'
      then wchave:=wchave+Poem_Zeros('0',14)
      else wchave:=wchave+Poem_Zeros(FCTe.Ide.toma4.CNPJCPF,14);
    end
    else begin
     case FCTe.Ide.toma03.Toma of
      tmRemetente: if (FCTe.Rem.enderReme.UF='EX')
                    then wchave:=wchave+Poem_Zeros('0',14)
                    else wchave:=wchave+Poem_Zeros(FCTe.Rem.CNPJCPF,14);
      tmExpedidor: if (FCTe.Exped.enderExped.UF='EX')
                    then wchave:=wchave+Poem_Zeros('0',14)
                    else wchave:=wchave+Poem_Zeros(FCTe.Exped.CNPJCPF,14);
      tmRecebedor: if (FCTe.Receb.enderReceb.UF='EX')
                    then wchave:=wchave+Poem_Zeros('0',14)
                    else wchave:=wchave+Poem_Zeros(FCTe.Receb.CNPJCPF,14);
      tmDestinatario: if (FCTe.Dest.EnderDest.UF='EX')
                       then wchave:=wchave+Poem_Zeros('0',14)
                       else wchave:=wchave+Poem_Zeros(FCTe.Dest.CNPJCPF,14);
     end;
    end;

   //VALOR DA CT-e
   wchave := wchave + Poem_Zeros(OnlyNumber(FloatToStrf(FCTe.vPrest.vTPrest, ffFixed,18,2)),14);

   //DESTAQUE ICMS PROPRIO E ST
   wicms_p := '2';
   wicms_s := '2';

   // Checar esse trecho

{$IFDEF PL_103}
   if (NaoEstaZerado(FCTe.Imp.ICMS.CST00.vICMS))
    then wicms_p := '1';
   if (NaoEstaZerado(FCTe.Imp.ICMS.CST80.vICMS))
    then wicms_s := '1';
{$ELSE}
   if (NaoEstaZerado(FCTe.Imp.ICMS.ICMS00.vICMS))
    then wicms_p := '1';
   if (NaoEstaZerado(FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF))
    then wicms_s := '1';
{$ENDIF}

   wchave := wchave + wicms_p + wicms_s;

   //DIA DA EMISSAO
   decodedate(FCTe.Ide.dhEmi, wa, wm, wd);
   wchave := wchave + Poem_Zeros(inttostr(wd), 2);

   //DIGITO VERIFICADOR
   GerarDigito_Contingencia(Digito, wchave);
   wchave := wchave + inttostr(digito);

   //RETORNA A CHAVE DE CONTINGENCIA
   result := wchave;
end;

class function CTeUtil.FormatarChaveContingencia(AValue: String): String;
begin
  AValue := OnlyNumber(AValue);
  Result := copy(AValue,1,4)  + ' ' + copy(AValue,5,4)  + ' ' +
            copy(AValue,9,4)  + ' ' + copy(AValue,13,4) + ' ' +
            copy(AValue,17,4) + ' ' + copy(AValue,21,4) + ' ' +
            copy(AValue,25,4) + ' ' + copy(AValue,29,4) + ' ' +
            copy(AValue,33,4) ;
end;

class function CTeUtil.IdentificaTipoSchema(const AXML: AnsiString; var I: Integer): Integer;
var
 lTipoEvento: String;
begin
  I := pos('<infCte',AXML) ;
  Result := 1;
  if I = 0  then
   begin
     I := pos('<infCanc',AXML) ;
     if I > 0 then
        Result := 2
     else
      begin
        I := pos('<infInut',AXML) ;
        if I > 0 then
           Result := 3
        else
         begin
          I := Pos('<infEvento', AXML);
          if I > 0 then
          begin
            lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));
            if lTipoEvento = '110111' then // Cancelamento
              Result := 6
            else if lTipoEvento = '110113' then // EPEC
              Result := 7
            else if lTipoEvento = '210200' then // Manif. Destinatario: Confirmação da Operação
              Result := 8
            else if lTipoEvento = '210210' then // Manif. Destinatario: Ciência da Operação Realizada
              Result := 9
            else if lTipoEvento = '210220' then // Manif. Destinatario: Desconhecimento da Operação
              Result := 10
            else if lTipoEvento = '210240' then // Manif. Destinatario: Operação não Realizada
              Result := 11
            else
              Result := 5; // Carta de Correção Eletrônica
          end
          else
            Result := 4; // Erro
         end;
     end;
   end;
end;

class function CTeUtil.CstatProcessado(AValue: Integer): Boolean;
begin
  case AValue of
     100: Result := True;
     110: Result := True;
     301: Result := True;
  else
     Result := False;
  end;
end;

end.

