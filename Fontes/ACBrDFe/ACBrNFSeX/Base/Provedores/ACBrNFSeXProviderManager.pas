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

unit ACBrNFSeXProviderManager;

interface

uses
  SysUtils, Classes, ACBrUtil,
  ACBrNFSeXInterface, ACBrNFSeXConversao, ACBrDFe;

type

  TACBrNFSeXProviderManager = class
  public
    class function GetProvider(ACBrNFSe: TACBrDFe): IACBrNFSeXProvider;
  end;

implementation

uses
  ACBrNFSeX,

  // Provedores que seguem a versão 1 do layout da ABRASF
  BHISS.Provider,
  CIGA.Provider,
  DBSeller.Provider,
  DSFSJC.Provider,
  FISSLex.Provider,
  geNFe.Provider,
  Ginfes.Provider,
  GovBr.Provider,
  ISSCuritiba.Provider,
  ISSFortaleza.Provider,
  ISSIntel.Provider,
  ISSNet.Provider,
  Lexsom.Provider,
  MetropolisWeb.Provider,
  Natal.Provider,
  NFSeBrasil.Provider,
  Publica.Provider,
  Recife.Provider,
  RJ.Provider,
  Salvador.Provider,
  SJP.Provider,
  SpeedGov.Provider,
  Thema.Provider,
  Tinus.Provider,

  // Provedores que seguem a versão 2 do layout da ABRASF
  ABase.Provider,
  Actcon.Provider,
  Adm.Provider,
  ADPM.Provider,
  AEG.Provider,
  Asten.Provider,
  Centi.Provider,
  Coplan.Provider,
  DataSmart.Provider,
  DeISS.Provider,
  Desenvolve.Provider,
  Digifred.Provider,
  DSF.Provider,
  EloTech.Provider,
  eReceita.Provider,
  fintelISS.Provider,
  Fiorilli.Provider,
  Futurize.Provider,
  Giss.Provider,
  Goiania.Provider,
  GovDigital.Provider,
  iiBrasil.Provider,
  ISSDigital.Provider,
  ISSe.Provider,
  ISSJoinville.Provider,
  Link3.Provider,
  MegaSoft.Provider,
  Mitra.Provider,
  ModernizacaoPublica.Provider,
  NEAInformatica.Provider,
  NotaInteligente.Provider,
  Prodata.Provider,
  PVH.Provider,
  RLZ.Provider,
  Saatri.Provider,
  SafeWeb.Provider,
  SH3.Provider,
  Siam.Provider,
  SiapNet.Provider,
  SiapSistemas.Provider,
  SigCorp.Provider,
  Sigep.Provider,
  SisPMJP.Provider,
  Sistemas4R.Provider,
  SystemPro.Provider,
  TcheInfo.Provider,
  Tecnos.Provider,
  Tributus.Provider,
  VersaTecnologia.Provider,
  Virtual.Provider,
  Vitoria.Provider,

  // Provedores que seguem a versão 1 e 2 do layout da ABRASF
  Abaco.Provider,
  Betha.Provider,
  Pronim.Provider,
  SilTecnologia.Provider,
  SimplISS.Provider,
  Tiplan.Provider,
  WebISS.Provider,

  // Provedores que tem layout próprio e também seguem a versão 1 ou 2 do
  // layout da ABRASF
  EL.Provider,
  Infisc.Provider,
  SmarAPD.Provider,

  // Provedores que tem layout próprio
  Agili.Provider,
  AssessorPublico.Provider,
  Conam.Provider,
  eGoverneISS.Provider,
  Equiplano.Provider,
  FGMaiss.Provider,
  GeisWeb.Provider,
  Giap.Provider,
  Governa.Provider,
  IPM.Provider,
  ISSDSF.Provider,
  Lencois.Provider,
  Siat.Provider,
  SigISS.Provider,
  SP.Provider,
  WebFisco.Provider;

  { TACBrNFSeXProviderManager }

class function TACBrNFSeXProviderManager.GetProvider(ACBrNFSe: TACBrDFe): IACBrNFSeXProvider;
begin
  case TACBrNFSeX(ACBrNFSe).Configuracoes.Geral.Provedor of
    // ABRASFv1
    proAbaco:    Result := TACBrNFSeProviderAbaco.Create(ACBrNFSe);
    proAbaco_A:  Result := TACBrNFSeProviderAbacoA.Create(ACBrNFSe);
    proBetha:    Result := TACBrNFSeProviderBetha.Create(ACBrNFSe);
    proBHISS:    Result := TACBrNFSeProviderBHISS.Create(ACBrNFSe);
    proCIGA:     Result := TACBrNFSeProviderCIGA.Create(ACBrNFSe);
    proDBSeller: Result := TACBrNFSeProviderDBSeller.Create(ACBrNFSe);
    proDSFSJC:   Result := TACBrNFSeProviderDSFSJC.Create(ACBrNFSe);
    proFISSLex:  Result := TACBrNFSeProviderFISSLex.Create(ACBrNFSe);
    progeNFe:    Result := TACBrNFSeProvidergeNFe.Create(ACBrNFSe);
    proGinfes:   Result := TACBrNFSeProviderGinfes.Create(ACBrNFSe);
    proGovBr:    Result := TACBrNFSeProviderGovBr.Create(ACBrNFSe);

    proISSCuritiba:
      Result := TACBrNFSeProviderISSCuritiba.Create(ACBrNFSe);

    proISSFortaleza:
      Result := TACBrNFSeProviderISSFortaleza.Create(ACBrNFSe);

    proISSIntel: Result := TACBrNFSeProviderISSIntel.Create(ACBrNFSe);
    proISSNet:   Result := TACBrNFSeProviderISSNet.Create(ACBrNFSe);
    proLexsom:   Result := TACBrNFSeProviderLexsom.Create(ACBrNFSe);

    proMetropolisWeb:
      Result := TACBrNFSeProviderMetropolisWeb.Create(ACBrNFSe);

    proNatal:    Result := TACBrNFSeProviderNatal.Create(ACBrNFSe);

    proNFSeBrasil:
      Result := TACBrNFSeProviderNFSeBrasil.Create(ACBrNFSe);

    proPronim:   Result := TACBrNFSeProviderPronim.Create(ACBrNFSe);
    proPublica:  Result := TACBrNFSeProviderPublica.Create(ACBrNFSe);
    proRecife:   Result := TACBrNFSeProviderRecife.Create(ACBrNFSe);
    proRJ:       Result := TACBrNFSeProviderRJ.Create(ACBrNFSe);
    proSalvador: Result := TACBrNFSeProviderSalvador.Create(ACBrNFSe);

    proSilTecnologia:
      Result := TACBrNFSeProviderSilTecnologia.Create(ACBrNFSe);

    proSimplISS: Result := TACBrNFSeProviderSimplISS.Create(ACBrNFSe);
    proSJP:      Result := TACBrNFSeProviderSJP.Create(ACBrNFSe);
    proSpeedGov: Result := TACBrNFSeProviderSpeedGov.Create(ACBrNFSe);
    proThema:    Result := TACBrNFSeProviderThema.Create(ACBrNFSe);
    proTinus,
    proTinus_A:  Result := TACBrNFSeProviderTinus.Create(ACBrNFSe);
    proTiplan:   Result := TACBrNFSeProviderTiplan.Create(ACBrNFSe);
    proWebISS:   Result := TACBrNFSeProviderWebISS.Create(ACBrNFSe);

    // ABRASFv2
    proSistemas4R:   Result := TACBrNFSeProvider4R.Create(ACBrNFSe);
    proAbaco_204:    Result := TACBrNFSeProviderAbacov204.Create(ACBrNFSe);
    proABase:        Result := TACBrNFSeProviderABase.Create(ACBrNFSe);
    proActcon_201:   Result := TACBrNFSeProviderActconv201.Create(ACBrNFSe);
    proActcon_202:   Result := TACBrNFSeProviderActconv202.Create(ACBrNFSe);
    proAdm:          Result := TACBrNFSeProviderAdm.Create(ACBrNFSe);
    proADPM:         Result := TACBrNFSeProviderADPM.Create(ACBrNFSe);
    proAEG:          Result := TACBrNFSeProviderAEG.Create(ACBrNFSe);
    proAsten:        Result := TACBrNFSeProviderAsten.Create(ACBrNFSe);
    proBetha_2:      Result := TACBrNFSeProviderBethav2.Create(ACBrNFSe);
    proCenti:        Result := TACBrNFSeProviderCenti.Create(ACBrNFSe);
    proCoplan:       Result := TACBrNFSeProviderCoplan.Create(ACBrNFSe);
    proDataSmart:    Result := TACBrNFSeProviderDataSmart.Create(ACBrNFSe);
    proDeISS:        Result := TACBrNFSeProviderDeISS.Create(ACBrNFSe);
    proDesenvolve:   Result := TACBrNFSeProviderDesenvolve.Create(ACBrNFSe);
    proDigifred:     Result := TACBrNFSeProviderDigifred.Create(ACBrNFSe);
    proDSF_2:        Result := TACBrNFSeProviderDSF.Create(ACBrNFSe);
    proEL_2:         Result := TACBrNFSeProviderELv2.Create(ACBrNFSe);
    proElotech:      Result := TACBrNFSeProviderEloTech.Create(ACBrNFSe);
    proeReceita:     Result := TACBrNFSeProvidereReceita.Create(ACBrNFSe);
    profintelISS:    Result := TACBrNFSeProviderfintelISS.Create(ACBrNFSe);
    proFiorilli:     Result := TACBrNFSeProviderFiorilli.Create(ACBrNFSe);
    proFuturize:     Result := TACBrNFSeProviderFuturize.Create(ACBrNFSe);
    proGiss:         Result := TACBrNFSeProviderGiss.Create(ACBrNFSe);
    proGoiania:      Result := TACBrNFSeProviderGoiania.Create(ACBrNFSe);
    proGovDigital:   Result := TACBrNFSeProviderGovDigital.Create(ACBrNFSe);
    proiiBrasil_2:   Result := TACBrNFSeProvideriiBrasil.Create(ACBrNFSe);
    proInfisc_2:     Result := TACBrNFSeProviderInfiscv2.Create(ACBrNFSe);
    proISSDigital:   Result := TACBrNFSeProviderISSDigital.Create(ACBrNFSe);
    proISSe:         Result := TACBrNFSeProviderISSe.Create(ACBrNFSe);
    proISSJoinville: Result := TACBrNFSeProviderISSJoinville.Create(ACBrNFSe);
    proLink3:        Result := TACBrNFSeProviderLink3.Create(ACBrNFSe);
    proMegaSoft:     Result := TACBrNFSeProviderMegaSoft.Create(ACBrNFSe);
    proMitra:        Result := TACBrNFSeProviderMitra.Create(ACBrNFSe);

    proModernizacaoPublica:
      Result := TACBrNFSeProviderModernizacaoPublica.Create(ACBrNFSe);
    proNEAInformatica:
      Result := TACBrNFSeProviderNEAInformatica.Create(ACBrNFSe);
    proNotaInteligente:
      Result := TACBrNFSeProviderNotaInteligente.Create(ACBrNFSe);

    proProdata:      Result := TACBrNFSeProviderProdata.Create(ACBrNFSe);
    proPronim_202:   Result := TACBrNFSeProviderPronimv202.Create(ACBrNFSe);
    proPronim_203:   Result := TACBrNFSeProviderPronimv203.Create(ACBrNFSe);
    proPVH:          Result := TACBrNFSeProviderPVH.Create(ACBrNFSe);
    proRLZ:          Result := TACBrNFSeProviderRLZ.Create(ACBrNFSe);
    proSaatri:       Result := TACBrNFSeProviderSaatri.Create(ACBrNFSe);
    proSafeWeb:      Result := TACBrNFSeProviderSafeWeb.Create(ACBrNFSe);
    proSH3:          Result := TACBrNFSeProviderSH3.Create(ACBrNFSe);
    proSiam:         Result := TACBrNFSeProviderSiam.Create(ACBrNFSe);
    proSiapNet:      Result := TACBrNFSeProviderSiapNet.Create(ACBrNFSe);
    proSiapSistemas: Result := TACBrNFSeProviderSiapSistemas.Create(ACBrNFSe);
    proSigCorp:      Result := TACBrNFSeProviderSigCorp.Create(ACBrNFSe);
    proSigep:        Result := TACBrNFSeProviderSigep.Create(ACBrNFSe);

    proSilTecnologia_203:
      Result := TACBrNFSeProviderSilTecnologiaV203.Create(ACBrNFSe);

    proSimplISS_2:   Result := TACBrNFSeProviderSimplISSv2.Create(ACBrNFSe);
    proSisPMJP:      Result := TACBrNFSeProviderSisPMJP.Create(ACBrNFSe);
    proSmarAPD_203:  Result := TACBrNFSeProviderSmarAPDv203.Create(ACBrNFSe);
    proSmarAPD_204:  Result := TACBrNFSeProviderSmarAPDv204.Create(ACBrNFSe);
    proSystemPro:    Result := TACBrNFSeProviderSystemPro.Create(ACBrNFSe);
    proTcheInfo_2:   Result := TACBrNFSeProviderTcheInfo.Create(ACBrNFSe);
    proTecnos:       Result := TACBrNFSeProviderTecnos.Create(ACBrNFSe);
    proTributus:     Result := TACBrNFSeProviderTributus.Create(ACBrNFSe);
    proTiplan_2:     Result := TACBrNFSeProviderTiplanv2.Create(ACBrNFSe);

    proVersaTecnologia_201:
      Result := TACBrNFSeProviderVersaTecnologiav201.Create(ACBrNFSe);

    proVersaTecnologia_202:
      Result := TACBrNFSeProviderVersaTecnologiav202.Create(ACBrNFSe);

    proVirtual:  Result := TACBrNFSeProviderVirtual.Create(ACBrNFSe);
    proVitoria:  Result := TACBrNFSeProviderVitoria.Create(ACBrNFSe);
    proWebISS_2: Result := TACBrNFSeProviderWebISSv2.Create(ACBrNFSe);

    // Layout Próprio
    proAgili:   Result := TACBrNFSeProviderAgili.Create(ACBrNFSe);

    proAssessorPublico:
      Result := TACBrNFSeProviderAssessorPublico.Create(ACBrNFSe);

    proConam:   Result := TACBrNFSeProviderConam.Create(ACBrNFSe);

    proeGoverneISS:
      Result := TACBrNFSeProvidereGoverneISS.Create(ACBrNFSe);

    proEL:      Result := TACBrNFSeProviderEL.Create(ACBrNFSe);

    proEquiplano:
      Result := TACBrNFSeProviderEquiplano.Create(ACBrNFSe);

    proFGMaiss: Result :=TACBrNFSeProviderFGMaiss.Create(ACBrNFSe);
    proGeisWeb: Result := TACBrNFSeProviderGeisWeb.Create(ACBrNFSe);
    proGiap:    Result := TACBrNFSeProviderGiap.Create(ACBrNFSe);
    proGoverna: Result := TACBrNFSeProviderGoverna.Create(ACBrNFSe);

    proInfisc_100:
      Result := TACBrNFSeProviderInfiscv100.Create(ACBrNFSe);

    proInfisc_110:
      Result := TACBrNFSeProviderInfiscv110.Create(ACBrNFSe);

    proIPM:     Result := TACBrNFSeProviderIPM.Create(ACBrNFSe);
    proIPM_110: Result := TACBrNFSeProviderIPMV110.Create(ACBrNFSe);
    proIPM_A:   Result := TACBrNFSeProviderIPMa.Create(ACBrNFSe);
    proISSDSF:  Result := TACBrNFSeProviderISSDSF.Create(ACBrNFSe);
    proLencois: Result := TACBrNFSeProviderLencois.Create(ACBrNFSe);
    proSiat:    Result := TACBrNFSeProviderSiat.Create(ACBrNFSe);
    proSigISS:  Result := TACBrNFSeProviderSigISS.Create(ACBrNFSe);

    proSigISS_103:
      Result := TACBrNFSeProviderSigISS_103.Create(ACBrNFSe);

    proSmarAPD: Result := TACBrNFSeProviderSmarAPD.Create(ACBrNFSe);
    proSP:      Result := TACBrNFSeProviderSP.Create(ACBrNFSe);

    proWebFisco:
      Result := TACBrNFSeProviderWebFisco.Create(ACBrNFSe);
  else
    Result := nil;
  end;
end;

end.
