{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
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

{$I ACBr.inc}

unit pnfsNFSeW;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}
  
{$ENDIF}
  SysUtils, Classes, StrUtils,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, ACBrConsts,
  ACBrNFSeConfiguracoes, synacode;

type

 TGeradorOpcoes   = class;

 TNFSeW = class(TPersistent)
  private
    FGerador: TGerador;
    FNFSe: TNFSe;
    FProvedor: TnfseProvedor;
    FOpcoes: TGeradorOpcoes;
    FAtributo: String;
    FPrefixo4: String;
    FIdentificador: String;
    FURL: String;
    FVersaoXML: String;
    FDefTipos: String;
    FServicoEnviar: String;
    FQuebradeLinha: String;

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarServicoValores_V1;
    procedure GerarServicoValores_V2;
    procedure GerarListaServicos;
    procedure GerarListaServicos2;
    procedure GerarValoresServico;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;
    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarXML_ABRASF_V1;
    procedure GerarXML_ABRASF_V2;

    procedure GerarServico_Provedor_IssDsf;
    procedure GerarXML_Provedor_IssDsf;
    procedure GerarXML_Provedor_Infisc;
    procedure GerarXML_Provedor_Equiplano;
    procedure GerarXML_Provedor_EgoverneISS;
    procedure GerarXML_Provedor_NFSEBrasil;
    procedure GerarXML_Provedor_EL;

  public
    constructor Create(AOwner: TNFSe);
    destructor Destroy; override;
    function GerarXml(ASincrono: Boolean = False): boolean; // Alterado por Nilton Olher - 11/02/2015
    function ObterNomeArquivo: string;
  published
    property Gerador: TGerador read FGerador write FGerador;
    property NFSe: TNFSe read FNFSe write FNFSe;
    property Provedor: TnfseProvedor read FProvedor write FProvedor;
    property Opcoes: TGeradorOpcoes read FOpcoes write FOpcoes;
    property Atributo: String read FAtributo write FAtributo;
    property Prefixo4: String read FPrefixo4 write FPrefixo4;
    property Identificador: String read FIdentificador write FIdentificador;
    property URL: String read FURL write FURL;
    property VersaoXML: String read FVersaoXML write FVersaoXML;
    property DefTipos: String read FDefTipos write FDefTipos;
    property ServicoEnviar: String read FServicoEnviar write FServicoEnviar;
    property QuebradeLinha: String read FQuebradeLinha write FQuebradeLinha;
  end;

 TGeradorOpcoes = class(TPersistent)
  private
    FAjustarTagNro: boolean;
    FNormatizarMunicipios: boolean;
    FGerarTagAssinatura: TnfseTagAssinatura;
    FPathArquivoMunicipios: string;
    FValidarInscricoes: boolean;
    FValidarListaServicos: boolean;
  published
    property AjustarTagNro: boolean read FAjustarTagNro write FAjustarTagNro;
    property NormatizarMunicipios: boolean read FNormatizarMunicipios write FNormatizarMunicipios;
    property GerarTagAssinatura: TnfseTagAssinatura read FGerarTagAssinatura write FGerarTagAssinatura;
    property PathArquivoMunicipios: string read FPathArquivoMunicipios write FPathArquivoMunicipios;
    property ValidarInscricoes: boolean read FValidarInscricoes write FValidarInscricoes;
    property ValidarListaServicos: boolean read FValidarListaServicos write FValidarListaServicos;
  end;

  ////////////////////////////////////////////////////////////////////////////////

implementation

uses
 ACBrUtil, ACBrNFSe;

{ TNFSeW }

constructor TNFSeW.Create(AOwner: TNFSe);
begin
 FNFSe                         := AOwner;
 FGerador                      := TGerador.Create;
 FGerador.FIgnorarTagNivel     := '|?xml version|NFSe xmlns|infNFSe versao|obsCont|obsFisco|';
 FOpcoes                       := TGeradorOpcoes.Create;
 FOpcoes.FAjustarTagNro        := True;
 FOpcoes.FNormatizarMunicipios := False;
 FOpcoes.FGerarTagAssinatura   := taSomenteSeAssinada;
 FOpcoes.FValidarInscricoes    := False;
 FOpcoes.FValidarListaServicos := False;
end;

destructor TNFSeW.Destroy;
begin
 FGerador.Free;
 FOpcoes.Free;

 inherited Destroy;
end;

////////////////////////////////////////////////////////////////////////////////

function TNFSeW.ObterNomeArquivo: string;
begin
 Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW.GerarXml(ASincrono: Boolean = False): boolean;  // Alterado por Nilton Olher - 11/02/2015
var
 Gerar : boolean;
begin
 Gerador.ArquivoFormatoXML := '';
 Gerador.Prefixo           := FPrefixo4;

 if (FProvedor in [proActcon, pro4R, proAgili, proBHISS, proCoplan, proDigifred,
                   profintelISS, proFiorilli, proGoiania, proGovDigital, {proGovBR,}  proIssDSF, proInfisc,
                   proISSDigital, proNatal, proProdata, proProdemge, proPVH,
                   proSaatri, proVirtual, proFreire, proLink3, proVitoria,
                   proTecnos, proPronim, proSystemPro, proSisPMJP, proNFSEBrasil,
                   proTinus])
   then FDefTipos := FServicoEnviar;

 if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
   then FDefTipos := '/' + FDefTipos;

 if Trim(FPrefixo4) <> ''
   then Atributo := ' xmlns:' + stringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
   else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

 // Jonatan ISS Nova Lima
 if (FProvedor = proISSDigital) and (NFSe.NumeroLote <> '')
   then Atributo := ' Id="' +  (NFSe.IdentificacaoRps.Numero) + '"';

 if not (FProvedor in [proIssDsf, proInfisc, proEquiplano, proEgoverneISS, proNFSEBrasil]) then
   if (FProvedor in [proGoiania, proProdata, proVitoria, proFiorilli, proVirtual, proPublica{, proSystemPro}])
      or ((FProvedor = proGovDigital) and (not ASincrono)) then   // Alterado por Nilton Olher - 11/02/2015
     begin
      Gerador.wGrupo('GerarNfseEnvio' + Atributo);
	    Gerador.wGrupo('Rps');
     end
     else if (FProvedor in [proBetha, proEL])
           then Gerador.wGrupo('Rps')
           else Gerador.wGrupo('Rps' + Atributo);

 case FProvedor of
  proISSDigital: FNFSe.InfID.ID := NotaUtil.ChaveAcesso(FNFSe.Prestador.cUF,
                         FNFSe.DataEmissao,
                         OnlyNumber(FNFSe.Prestador.Cnpj),
                         0, // Serie
                         StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)),
                         StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)));

  proTecnos: FNFSe.InfID.ID := '1' + //Fixo - Lote Sincrono
 //                        FormatDateTime('yyyy', FNFSe.DataEmissao) +
                         OnlyNumber(FNFSe.Prestador.Cnpj) +
//                         IntToStrZero(StrToIntDef(FNFSe.NumeroLote, 1), 16);
                         IntToStrZero(StrToIntDef(FNFSe.IdentificacaoRps.Numero, 1), 16);

  proIssDsf: FNFSe.InfID.ID := FNFSe.IdentificacaoRps.Numero;

  proInfisc: FNFSe.InfID.ID := FNFSe.Numero;

  proSystemPro: FNFSe.InfID.ID := FNFSe.InfID.ID;

  proRecife: FNFSe.InfID.ID := 'RPS' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

  proGovDigital: FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero);

  proEL: begin
           FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;
           FNFSe.InfID.ID := StringOfChar('0', 15) + FNFSe.InfID.ID;
           FNFSe.InfID.ID := copy(FNFSe.InfID.ID, length(FNFSe.InfID.ID) - 15 + 1, 15);
         end;

  else FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;
 end;

 case FProvedor of
  proABRASFv1,
  proAbaco,
  proBetha,
  proBetim,
  proBHISS,
  proDBSeller,
  proFISSLex,
  proGinfes,
  proGovBR,
  proISSCuritiba,
  proISSIntel,
  proISSNet,
  proLexsom,
  proNatal,
  proProdemge,
  proPronim,
  proPublica,
  proRecife,
  proRJ,
  proSalvador,
  proSimplISS,
  proSJP,
  proSpeedGov,
  proThema,
  proTinus,
  proTiplan,
  proWebISS: GerarXML_ABRASF_V1;

  proABRASFv2,
  pro4R,
  proActcon,
  proAgili,
  proCoplan,
  proDigifred,
  proFIntelISS,
  proFiorilli,
  proFreire,
  proGoiania,
  proGovDigital,
  proISSDigital,
  proISSe,
  proLink3,
  proMitra,
  proProdata,
  proPVH,
  proSaatri,
  proSisPMJP,
  proSystemPro,
  proTecnos,
  proVirtual,
  proVitoria: GerarXML_ABRASF_V2;

  proIssDsf:      GerarXML_Provedor_IssDsf;
  proInfisc:      GerarXML_Provedor_Infisc;
  proEquiplano:   GerarXML_Provedor_Equiplano;
  proEGoverneISS: GerarXML_Provedor_EgoverneISS;
  proNFSEBrasil:  GerarXML_Provedor_NFSEBrasil;
  proEL:          GerarXML_Provedor_EL;
 end;

 if FOpcoes.GerarTagAssinatura <> taNunca
   then begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada
      then Gerar := ((NFSe.signature.DigestValue <> '') and
                     (NFSe.signature.SignatureValue <> '') and
                     (NFSe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada
      then Gerar := ((NFSe.signature.DigestValue = '') and
                     (NFSe.signature.SignatureValue = '') and
                     (NFSe.signature.X509Certificate = ''));
    if Gerar
      then begin
       FNFSe.signature.URI := FNFSe.InfID.ID;
       FNFSe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
       FNFSe.signature.GerarXMLNFSe;
       Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML + FNFSe.signature.Gerador.ArquivoFormatoXML;
      end;
   end;

 if not (FProvedor in [proIssDsf, proInfisc, proEquiplano, proEgoverneISS]) then
   if (FProvedor in [proGoiania, proProdata, proVitoria, proFiorilli, proVirtual, proPublica{, proSystemPro}])
      or ((FProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
     begin
      Gerador.wGrupo('/Rps');
      Gerador.wGrupo('/GerarNfseEnvio');
     end
     else Gerador.wGrupo('/Rps');

 Gerador.gtAjustarRegistros(NFSe.InfID.ID);
 Result := (Gerador.ListaDeAlertas.Count = 0);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW.GerarIdentificacaoRPS;
begin
  case FProvedor of
   proSystemPro: begin
                   if StrToIntDef(NFSe.IdentificacaoRps.Numero,0) > 0 then
                   begin
                     Gerador.wGrupoNFSe('Rps');

                     Gerador.wGrupoNFSe('IdentificacaoRps');
                     Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
                     Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, '');
                     Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), '');
                     Gerador.wGrupoNFSe('/IdentificacaoRps');
                     Gerador.wCampoNFSe(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);
                     Gerador.wCampoNFSe(tcStr, '#9', 'Status     ', 01, 01, 1, StatusRPSToStr(NFSe.Status), '');

                     Gerador.wGrupoNFSe('/Rps');
                   end;
                 end;
   proNFSEBrasil: begin
                    Gerador.wGrupoNFSe('Rps');
                    Gerador.wGrupoNFSe('InfRps');
                    Gerador.wGrupoNFSe('IdentificacaoRps');
                    Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, SomenteNumeros(NFSe.IdentificacaoRps.Numero), '');
                    Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, '');
                    Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), '');
                    Gerador.wGrupoNFSe('/IdentificacaoRps');
                    Gerador.wCampoNFSe(tcDatHor, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);
                    Gerador.wCampoNFSe(tcStr, '#4','NaturezaOperacao     ', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
                    Gerador.wCampoNFSe(tcStr, '#9', 'Status     ', 01, 01, 1, StatusRPSToStr(NFSe.Status), '');
                  end;
   else begin
          Gerador.wGrupoNFSe('IdentificacaoRps');
          Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
          Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, '');
          Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), '');
          Gerador.wGrupoNFSe('/IdentificacaoRps');
        end;
  end;
end;

procedure TNFSeW.GerarRPSSubstituido;
begin
 if NFSe.RpsSubstituido.Numero <> ''
  then begin
   Gerador.wGrupoNFSe('RpsSubstituido');
    Gerador.wCampoNFSe(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), '');
    Gerador.wCampoNFSe(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, '');
    Gerador.wCampoNFSe(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), '');
   Gerador.wGrupoNFSe('/RpsSubstituido');
  end;
end;

procedure TNFSeW.GerarServicoValores_V1;
var
 i: Integer;
begin
  Gerador.wGrupoNFSe('Servico');
   Gerador.wGrupoNFSe('Valores');
    Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
    { Alterado por Cleiver em 25/08/2014 }
    if FProvedor in [proRecife, proFreire, proPronim, proISSNET, proNFSeBrasil, proGinfes]
      then begin
        Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
        Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis     ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
        Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins  ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
        Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss    ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
        Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr      ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
        Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll    ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
        Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido    ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
        Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss     ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '');
      end
      else begin
        Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes', 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, '');
        Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis     ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
        Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins  ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
        Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss    ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
        Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr      ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
        Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll    ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
        Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido    ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
        Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss     ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
      end;

    if not (FProvedor in [proPronim, proBetha, proGovBr])
      then Gerador.wCampoNFSe(tcDe2, '#22', 'ValorIssRetido', 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');

    if FProvedor in [proFreire, proPronim, proNFSeBrasil] then
       Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, '')
    else
       Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, '');

    if FProvedor <> proNFSeBrasil then
    begin
      if FProvedor = proPronim then
         Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo    ', 01, 15, 1, NFSe.Servico.Valores.BaseCalculo, '')
      else
         Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo    ', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');
    end;

    //Alterado por JuaumKiko em 05/02/2014
    case FProvedor of
     proGovBR,
     proPronim,
     proISSNet:   Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

     proRecife:   if NFSe.OptanteSimplesNacional = snSim
                    then Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '')
                    else Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

     proSimplISS: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');

     proGINFES:   Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, (NFSe.Servico.Valores.Aliquota / 100), '');

     proNFSEBrasil: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, (NFSe.Servico.Valores.Aliquota * 100), '');

     proSisPMJP:  Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');

     else         Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
    end;

    if FProvedor <> proNFSEBrasil then
    begin
      Gerador.wCampoNFSe(tcDe2, '#26', 'ValorLiquidoNfse', 01, 15, 0, NFSe.Servico.Valores.ValorLiquidoNfse, '');

      if (FProvedor in [proPronim, proBetha, proGovBr]) then
        Gerador.wCampoNFSe(tcDe2, '#22', 'ValorIssRetido', 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');
    end;

    if FProvedor in [proFreire, proNFSeBrasil] then
    begin
      Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
      Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, '');
    end
    else begin
      Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, '');
      Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, '');
    end;

   Gerador.wGrupoNFSe('/Valores');

   if FProvedor <> proNFSeBrasil then
   begin
     if FProvedor in [proISSNet, proWebISS, proIssCuritiba, proAbaco, proRecife, proBetha] then
       Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 1, OnlyNumber(NFSe.Servico.ItemListaServico), '')
     else
       Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 1, NFSe.Servico.ItemListaServico, '');
   end;

   if FProvedor = proNFSeBrasil then
   begin
     Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, NFSe.Servico.CodigoTributacaoMunicipio, '');
     Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 0007, 1, SomenteNumeros(NFSe.Servico.CodigoCnae), '');
   end;

   if not (FProvedor in [proPublica, proNFSeBrasil]) then
   begin
     Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');
     Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, NFSe.Servico.CodigoTributacaoMunicipio, '');
   end;

   Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                      StringReplace( FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');

   // Provedor Ginfes
   // Schema: tipos_v02 o nome da tag é MunicipioPrestacaoServico *** essa versão não esta em uso
   // Schema: tipos_v03 o nome da tag é CodigoMunicipio

   // No provedor ISSNet o nome da tag é MunicipioPrestacaoServico e os demais é CodigoMunicipio
   if FProvedor = proISSNet then
     Gerador.wCampoNFSe(tcStr, '#33', 'MunicipioPrestacaoServico', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), '')
   else
     Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), '');

   if FProvedor = proSimplISS
     then begin
      for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
         begin
          Gerador.wGrupo('ItensServico');
           Gerador.wCampo(tcStr, '#33a', 'Descricao    ', 01, 100, 1, NFSe.Servico.ItemServico[i].Descricao, '');
//           Gerador.wCampo(tcInt, '#33b', 'Quantidade   ', 01, 015, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
           Gerador.wCampo(tcDe2, '#33b', 'Quantidade   ', 01, 015, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
           Gerador.wCampo(tcDe2, '#33c', 'ValorUnitario', 01, 015, 1, NFSe.Servico.ItemServico[i].ValorUnitario, '');
          Gerador.wGrupo('/ItensServico');
         end;
      if NFSe.Servico.ItemServico.Count > 10
        then Gerador.wAlerta('#33a', 'ItensServico', 'Itens de Servico', ERR_MSG_MAIOR_MAXIMO + '10');
     end;

  Gerador.wGrupoNFSe('/Servico');
end;

procedure TNFSeW.GerarServicoValores_V2;
begin
  Gerador.wGrupoNFSe('Servico');

  if FProvedor in [proTecnos] then
    Gerador.wGrupoNFSe('tcDadosServico');

  Gerador.wGrupoNFSe('Valores');
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');

  case FProvedor of
   profintelISS: begin
                   Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
                   Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss       ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '');
                 end;
   proFreire,
   proVirtual,
   proActcon: begin
                Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
                Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
                Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
                Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
                Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
                Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
                Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, '');
              end;
   else begin
          Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, '');

          case FProvedor of
           proISSe,
           proSystemPro,
           proProdata,
           proVitoria,
           proTecnos: begin
                        Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
                        Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
                        Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
                        Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
                        Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
                        Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
                      end;
           else begin
                  Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
                  Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
                  Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
                  Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
                  Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
                  Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
                end;
          end;

          if not (FProvedor in [proProdata, proGoiania]) then
          begin
            if FProvedor in [pro4R, proISSDigital, proISSe, proSystemPro,
                             proFiorilli, proSaatri, proCoplan, proLink3,
                             proTecnos] then
              Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '')
            else
              Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
          end;
        end;
  end;

  if not (FProvedor in [pro4R, profinteliSS, proFiorilli, proGoiania, proISSDigital,
                        proISSe, proSystemPro, proProdata, proVitoria, proPVH,
                        proSaatri, proCoplan, proFreire, proLink3, proMitra,
                        proGovDigital, proVirtual, proSisPMJP, proDigifred]) then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');

  if FProvedor in [proActcon, proVirtual] then
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '');

  case FProvedor of
   pro4R,
   profintelISS,
   proISSDigital,
   proISSe,
   proSystemPro,
   proSaatri,
   proLink3,
   proVirtual,
   proGovDigital: Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

   proGoiania: begin
                 if NFSe.OptanteSimplesNacional = snSim then
                   Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
               end;

   proFreire: begin
                if NFSe.OptanteSimplesNacional = snSim then
                  Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');
              end;

   proTecnos,
   proProdata: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

   proFiorilli,
   proDigifred,
   proCoplan,
   proSisPMJP: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');

   else Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
  end;

  if FProvedor in [profintelISS] then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 1, NFSe.Servico.Valores.BaseCalculo, '');

  case FProvedor of
   proFreire,
   proTecnos,
   proVirtual,
   proActcon: begin
                Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
                Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, '');
              end;

   pro4R,
   proFiorilli,
   proGoiania,
   proISSDigital,
   proISSe,
   proSystemPro,
//   proProdata,
   proPVH,
   proSaatri,
   proCoplan,
   proLink3,
   proVitoria: begin
                 Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, '');
                 Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, '');
               end;

   proProdata: Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');

   else begin
          Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, '');
          Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, '');
        end;
  end;

  Gerador.wGrupoNFSe('/Valores');

  if FProvedor <> proGoiania then
   Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');

  if ((NFSe.Servico.Valores.IssRetido <> stNormal) and
     (FProvedor <> proGoiania)) or (FProvedor in [proProdata, proVirtual]) then
    Gerador.wCampoNFSe(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');

  if FProvedor <> proGoiania then
  begin
    case FProvedor of
      proVirtual: Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, OnlyNumber(NFSe.Servico.ItemListaServico), '');
      proVitoria: begin
                    if copy(NFSe.Servico.ItemListaServico, 1, 1) = '0'  then
                      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, copy(NFSe.Servico.ItemListaServico, 2, 4), '')
                    else
                      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, '');
                  end;
      else Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, '');
    end;
  end;

  if FProvedor <> proGoiania then
    Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 07, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');

  if FProvedor in [proGoiania, proVirtual] then
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '')
  else
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');

  Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                      StringReplace( FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');

  Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), '');

  if FProvedor = proVirtual then
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 1, NFSe.Servico.CodigoPais, '')
  else
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 0, NFSe.Servico.CodigoPais, '');

  if FProvedor <> proGoiania
   then Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '');

  if FProvedor <> proGoiania then
   begin
    if not (FProvedor in [proProdata, proVirtual]) then
      Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 0, NFSe.Servico.MunicipioIncidencia, '')
    else
      Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 1, NFSe.Servico.MunicipioIncidencia, '');
   end;

  Gerador.wCampoNFSe(tcStr, '#37', 'NumeroProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, '');

  if FProvedor in [proTecnos] then
    Gerador.wGrupoNFSe('/tcDadosServico');

  Gerador.wGrupoNFSe('/Servico');
end;

procedure TNFSeW.GerarListaServicos;
var
 i: Integer;
begin
  if FProvedor <> proSystemPro then
    Gerador.wGrupoNFSe('ListaServicos');

   for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
    begin
     Gerador.wGrupoNFSe('Servico');
      Gerador.wGrupoNFSe('Valores');
       Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorServicos, '');
       Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorDeducoes, '');
       Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss              ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIss, '');
       Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota              ', 01, 05, 1, NFSe.Servico.ItemServico[i].Aliquota, '');
       Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo           ', 01, 15, 1, NFSe.Servico.ItemServico[i].BaseCalculo, '');
       Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoIncondicionado, '');
       Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoCondicionado, '');

       if FProvedor=proSystemPro then
       begin
         Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis     ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorPis, '');
         Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCofins, '');
         Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorInss, '');
         Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr      ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIr, '');
         Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCsll, '');
       end;

      Gerador.wGrupoNFSe('/Valores');
      Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido                ', 01, 01,   1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico         ', 01, 0005, 1, NFSe.Servico.ItemListaServico, '');
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae               ', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');
      Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');
      Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                      StringReplace( NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');
//      Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao            ', 01, 2000, 1, NFSe.Servico.ItemServico[i].Discriminacao, '');
      Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), '');
      Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais               ', 04, 04,   0, NFSe.Servico.CodigoPais, '');
      Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS         ', 01, 01,   1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '');
      Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia      ', 07, 07,   0, NFSe.Servico.MunicipioIncidencia, '');
      Gerador.wCampoNFSe(tcStr, '#37', 'NumeroProcesso           ', 01, 30,   0, NFSe.Servico.NumeroProcesso, '');

      if (NFSe.Servico.Valores.IssRetido <> stNormal) and (FProvedor = proSystemPro) then
        Gerador.wCampoNFSe(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');
     Gerador.wGrupoNFSe('/Servico');
    end;

  if FProvedor <> proSystemPro then
    Gerador.wGrupoNFSe('/ListaServicos');
end;

procedure TNFSeW.GerarListaServicos2;
var
 i: Integer;
begin
  Gerador.wGrupoNFSe('Servicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
   Gerador.wGrupoNFSe('Servico');
    Gerador.wCampoNFSe(tcStr, '#59', 'CodigoCnae'             , 01, 007, 0, NFSe.Servico.CodigoCnae, '');
    Gerador.wCampoNFSe(tcStr, '#60', 'CodigoServico116'       , 01, 005, 1, NFSe.Servico.ItemListaServico, '');
    Gerador.wCampoNFSe(tcStr, '#61', 'CodigoServicoMunicipal' , 01, 020, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');
    Gerador.wCampoNFSe(tcInt, '#62', 'Quantidade'             , 01, 005, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
    Gerador.wCampoNFSe(tcStr, '#63', 'Unidade'                , 01, 020, 1, 'UN', '');
    Gerador.wCampoNFSe(tcStr, '#64', 'Descricao'              , 01, 255, 1, NFSe.Servico.ItemServico[i].Discriminacao, '');
    Gerador.wCampoNFSe(tcDe2, '#65', 'Aliquota'               , 01, 005, 1, NFSe.Servico.ItemServico[i].Aliquota, '');
    Gerador.wCampoNFSe(tcDe2, '#66', 'ValorServico'           , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorServicos, '');
    Gerador.wCampoNFSe(tcDe2, '#67', 'ValorIssqn'             , 01, 015, 1, NFSe.Servico.ItemServico[i].ValorIss, '');
    Gerador.wCampoNFSe(tcDe2, '#68', 'ValorDesconto'          , 01, 015, 0, NFSe.Servico.ItemServico[i].ValorDeducoes, '');
    Gerador.wCampoNFSe(tcStr, '#69', 'NumeroAlvara'           , 01, 015, 0, '', '');
   Gerador.wGrupoNFSe('/Servico');
  end;
  Gerador.wGrupoNFSe('/Servicos');

  Gerador.wGrupoNFSe('Valores');
   Gerador.wCampoNFSe(tcDe2, '#70', 'ValorServicos'       , 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
   Gerador.wCampoNFSe(tcDe2, '#71', 'ValorDeducoes'       , 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, '');
   Gerador.wCampoNFSe(tcDe2, '#72', 'ValorPis'            , 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
   Gerador.wCampoNFSe(tcDe2, '#73', 'ValorCofins'         , 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
   Gerador.wCampoNFSe(tcDe2, '#74', 'ValorInss'           , 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
   Gerador.wCampoNFSe(tcDe2, '#75', 'ValorIr'             , 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
   Gerador.wCampoNFSe(tcDe2, '#76', 'ValorCsll'           , 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
   Gerador.wCampoNFSe(tcDe2, '#77', 'ValorIss'            , 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
   Gerador.wCampoNFSe(tcDe2, '#78', 'ValorOutrasRetencoes', 01, 05, 0, NFSe.Servico.Valores.OutrasRetencoes, '');
   Gerador.wCampoNFSe(tcDe2, '#79', 'ValorLiquidoNfse'    , 01, 15, 0, NFSe.Servico.Valores.ValorLiquidoNfse, '');
   Gerador.wCampoNFSe(tcDe2, '#80', 'ValorIssRetido'      , 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, '');
  Gerador.wGrupoNFSe('/Valores');
end;

procedure TNFSeW.GerarValoresServico;
begin
 Gerador.wGrupoNFSe('ValoresServico');
  Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis        ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins     ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss       ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr         ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll       ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss        ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '');
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorLiquidoNfse', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos   ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
 Gerador.wGrupoNFSe('/ValoresServico');
end;

procedure TNFSeW.GerarPrestador;
var
  xMun: String;
begin
  if (FProvedor = proEL) then
  begin
    Gerador.wGrupoNFSe('DadosPrestador');

    Gerador.wGrupoNFSe('IdentificacaoPrestador');
    Gerador.wCampoNFSe(tcStr, '#10', 'CpfCnpj'                , 11, 014, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
    Gerador.wCampoNFSe(tcStr, '#11', 'IndicacaoCpfCnpj'       , 01, 001, 1, '2', '');
    Gerador.wCampoNFSe(tcStr, '#12', 'InscricaoMunicipal'     , 01, 015, 0, NFSe.Prestador.InscricaoMunicipal, '');
    Gerador.wGrupoNFSe('/IdentificacaoPrestador');

    Gerador.wCampoNFSe(tcStr, '#13', 'RazaoSocial'             , 01, 115, 0, NFSe.PrestadorServico.RazaoSocial, '');
    Gerador.wCampoNFSe(tcStr, '#14', 'IncentivadorCultural  '  , 01, 001, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
    Gerador.wCampoNFSe(tcStr, '#15', 'OptanteSimplesNacional'  , 01, 001, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
    Gerador.wCampoNFSe(tcStr, '#16', 'NaturezaOperacao'        , 01, 001, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
    Gerador.wCampoNFSe(tcStr, '#17', 'RegimeEspecialTributacao', 01, 001, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');

    Gerador.wGrupoNFSe('Endereco');
    Gerador.wCampoNFSe(tcStr, '#18', 'LogradouroTipo'         , 01, 125, 0, NFSe.PrestadorServico.Endereco.TipoLogradouro, '');
    Gerador.wCampoNFSe(tcStr, '#19', 'Logradouro'             , 01, 125, 0, NFSe.PrestadorServico.Endereco.Endereco, '');
    Gerador.wCampoNFSe(tcStr, '#20', 'LogradouroNumero'       , 01, 010, 0, NFSe.PrestadorServico.Endereco.Numero, '');
    Gerador.wCampoNFSe(tcStr, '#21', 'LogradouroComplemento'  , 01, 060, 0, NFSe.PrestadorServico.Endereco.Complemento, '');
    Gerador.wCampoNFSe(tcStr, '#22', 'Bairro'                 , 01, 060, 0, NFSe.PrestadorServico.Endereco.Bairro, '');
    Gerador.wCampoNFSe(tcStr, '#23', 'CodigoMunicipio'        , 07, 007, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CodigoMunicipio), '');
    if (Trim(NFSe.PrestadorServico.Endereco.xMunicipio) = '') then
    begin
      xMun := CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0));
      xMun := Copy(xMun,1,Length(xMun)-3);
      Gerador.wCampoNFSe(tcStr, '#24', 'Municipio'            , 01, 100, 0, UpperCase(xMun), '');
    end
    else
      Gerador.wCampoNFSe(tcStr, '#24', 'Municipio'            , 01, 100, 0, NFSe.PrestadorServico.Endereco.xMunicipio, '');

    Gerador.wCampoNFSe(tcStr, '#25', 'Uf'                     , 02, 002, 0, NFSe.PrestadorServico.Endereco.UF, '');
    Gerador.wCampoNFSe(tcStr, '#26', 'Cep'                    , 08, 008, 0, OnlyNumber(NFSe.PrestadorServico.Endereco.CEP), '');
    Gerador.wGrupoNFSe('/Endereco');

    if (NFSe.PrestadorServico.Contato.Telefone <> '') or
       (NFSe.PrestadorServico.Contato.Email <> '') then
    begin
      Gerador.wGrupoNFSe('Contato');
      Gerador.wCampoNFSe(tcStr, '#27', 'Telefone'              , 01, 011, 0, OnlyNumber(NFSe.PrestadorServico.Contato.Telefone), '');
      Gerador.wCampoNFSe(tcStr, '#28', 'Email   '              , 01, 080, 0, NFSe.PrestadorServico.Contato.Email, '');
      Gerador.wGrupoNFSe('/Contato');
    end;

    Gerador.wGrupoNFSe('/DadosPrestador');

  end
  else begin
    Gerador.wGrupoNFSe('Prestador');

    if ((VersaoXML = '1') and not (FProvedor in [proISSNet, proActcon])) or
       (FProvedor = proNFSeBrasil) then
      Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '')
    else begin
      Gerador.wGrupoNFSe('CpfCnpj');
      if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
        Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), '')
      else
        Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
      Gerador.wGrupoNFSe('/CpfCnpj');
    end;

    if (FProvedor = proTecnos) then
    begin
      Gerador.wCampoNFSe(tcStr, '#35', 'RazaoSocial', 01, 15, 1, NFSe.PrestadorServico.RazaoSocial, '');
      Gerador.wCampoNFSe(tcStr, '#36', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, '');
    end
    else
      Gerador.wCampoNFSe(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, '');

    if (FProvedor in [proISSDigital, proAgili]) then
    begin
      Gerador.wCampoNFSe(tcStr, '#36', 'Senha       ', 01, 255, 1, NFSe.Prestador.Senha, '');
      Gerador.wCampoNFSe(tcStr, '#37', 'FraseSecreta', 01, 255, 1, NFSe.Prestador.FraseSecreta, '');
    end;

    Gerador.wGrupoNFSe('/Prestador');
  end;
end;

procedure TNFSeW.GerarTomador;
var
  xMun: String;
begin
 if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
    (NFSe.Tomador.RazaoSocial <> '') or
    (NFSe.Tomador.Endereco.Endereco <> '') or
    (NFSe.Tomador.Contato.Telefone <> '') or
    (NFSe.Tomador.Contato.Email <>'')
   then begin
    if (FProvedor = proEL) then
    begin
      Gerador.wGrupoNFSe('DadosTomador');

       Gerador.wGrupoNFSe('IdentificacaoTomador');
        Gerador.wCampoNFSe(tcStr, '#34', 'CpfCnpj'              , 11, 014, 1, SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
        if Length(SomenteNumeros(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11
        then Gerador.wCampoNFSe(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '1', '')
        else Gerador.wCampoNFSe(tcStr, '#35', 'IndicacaoCpfCnpj', 01, 001, 1, '2', '');
        Gerador.wCampoNFSe(tcStr, '#36', 'InscricaoMunicipal'   , 01, 015, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
       Gerador.wGrupoNFSe('/IdentificacaoTomador');

       Gerador.wCampoNFSe(tcStr, '#37', 'RazaoSocial'           , 01, 115, 0, NFSe.Tomador.RazaoSocial, '');
       Gerador.wGrupoNFSe('Endereco');
        Gerador.wCampoNFSe(tcStr, '#38', 'LogradouroTipo'       , 01, 125, 0, NFSe.Tomador.Endereco.TipoLogradouro, '');
        Gerador.wCampoNFSe(tcStr, '#39', 'Logradouro'           , 01, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
        Gerador.wCampoNFSe(tcStr, '#40', 'LogradouroNumero'     , 01, 010, 0, NFSe.Tomador.Endereco.Numero, '');
        Gerador.wCampoNFSe(tcStr, '#41', 'LogradouroComplemento', 01, 060, 0, NFSe.Tomador.Endereco.Complemento, '');
        Gerador.wCampoNFSe(tcStr, '#42', 'Bairro'               , 01, 060, 0, NFSe.Tomador.Endereco.Bairro, '');
        Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipio'      , 07, 007, 0, SomenteNumeros(NFSe.Tomador.Endereco.CodigoMunicipio), '');
        if (Trim(NFSe.Tomador.Endereco.xMunicipio) = '') then begin
          xMun := CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio, 0));
          xMun := Copy(xMun,1,Length(xMun)-3);
          Gerador.wCampoNFSe(tcStr, '#44', 'Municipio'          , 01, 100, 0, UpperCase(xMun), '');
        end else
          Gerador.wCampoNFSe(tcStr, '#44', 'Municipio'          , 01, 100, 0, NFSe.Tomador.Endereco.xMunicipio, '');
        Gerador.wCampoNFSe(tcStr, '#45', 'Uf'                   , 02, 002, 0, NFSe.Tomador.Endereco.UF, '');
        Gerador.wCampoNFSe(tcStr, '#46', 'Cep'                  , 08, 008, 0, SomenteNumeros(NFSe.Tomador.Endereco.CEP), '');
       Gerador.wGrupoNFSe('/Endereco');

       if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '')
       then begin
        Gerador.wGrupoNFSe('Contato');
         Gerador.wCampoNFSe(tcStr, '#47', 'Telefone'            , 01, 011, 0, SomenteNumeros(NFSe.Tomador.Contato.Telefone), '');
         Gerador.wCampoNFSe(tcStr, '#48', 'Email   '            , 01, 080, 0, NFSe.Tomador.Contato.Email, '');
        Gerador.wGrupoNFSe('/Contato');
       end;

      Gerador.wGrupoNFSe('/DadosTomador');

    end
    else begin
    if (VersaoXML = '1') or
       (FProvedor in [pro4R, proAgili, proCoplan, proDigifred, proFiorilli,
                      proGoiania, proGovDigital, proISSDigital, proISSe, proSystemPro,
                      proProdata, proPVH, proSaatri, proVirtual, proFreire,
                      proLink3, proVitoria, proMitra, proTecnos, proSisPMJP,
                      proNFSeBrasil])
      then Gerador.wGrupoNFSe('Tomador')
      else Gerador.wGrupoNFSe('TomadorServico');
    // Alterado por Augusto Fontana em 12/06/2014
    if (NFSe.Tomador.Endereco.UF <> 'EX') or
       (FProvedor = proSimplISS) or (FProvedor = proISSNet) then
    begin
      Gerador.wGrupoNFSe('IdentificacaoTomador');
      Gerador.wGrupoNFSe('CpfCnpj');
      if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
        Gerador.wCampoNFSe(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '')
      else
        Gerador.wCampoNFSe(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
      Gerador.wGrupoNFSe('/CpfCnpj');
      Gerador.wCampoNFSe(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

      if (FProvedor = proSimplISS) or (FProvedor = proBetha) then
        Gerador.wCampoNFSe(tcStr, '#38', 'InscricaoEstadual', 01, 20, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');

      Gerador.wGrupoNFSe('/IdentificacaoTomador');
    end;

    Gerador.wCampoNFSe(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, '');

    Gerador.wGrupoNFSe('Endereco');

    if FProvedor = proVirtual then
    begin
      Gerador.wCampoNFSe(tcStr, '#39', 'Endereco   ', 001, 125, 1, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wCampoNFSe(tcStr, '#40', 'Numero     ', 001, 010, 1, NFSe.Tomador.Endereco.Numero, '');
      Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, '');
      Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 060, 1, NFSe.Tomador.Endereco.Bairro, '');
    end
    else begin
      Gerador.wCampoNFSe(tcStr, '#39', 'Endereco   ', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wCampoNFSe(tcStr, '#40', 'Numero     ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, '');
      if FProvedor <> proNFSeBrasil then
        Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 0, NFSe.Tomador.Endereco.Complemento, '')
      else
        Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, '');
      Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 060, 0, NFSe.Tomador.Endereco.Bairro, '');
    end;

    if FProvedor in [proEquiplano, proISSNet] then
    begin
      Gerador.wCampoNFSe(tcStr, '#43', 'Cidade', 007, 007, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
      Gerador.wCampoNFSe(tcStr, '#44', 'Estado', 002, 002, 0, NFSe.Tomador.Endereco.UF, '');
    end
    else begin
      Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
      Gerador.wCampoNFSe(tcStr, '#44', 'Uf             ', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');
    end;

    if (VersaoXML = '2') and (FProvedor <> proNFSeBrasil) then
      Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais ', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');

    Gerador.wCampoNFSe(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
    Gerador.wGrupoNFSe('/Endereco');

    case FProvedor of
     proVirtual: begin
                   Gerador.wGrupoNFSe('Contato');
                   Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
                   Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, '');
                   Gerador.wGrupoNFSe('/Contato');
                 end;
     proNFSeBrasil: begin
                      Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, '');
                      Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
                    end;
     else begin
            if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
            begin
              Gerador.wGrupoNFSe('Contato');
              Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
              Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 80, 0, NFSe.Tomador.Contato.Email, '');
              Gerador.wGrupoNFSe('/Contato');
            end;
          end;
    end;

    if (VersaoXML = '1') or
       (FProvedor in [pro4R, proAgili, proCoplan, proDigifred, proFiorilli,
                      proGoiania, proGovDigital, proISSDigital, proISSe, proSystemPro,
                      proProdata, proPVH, proSaatri, proVirtual, proFreire,
                      proLink3, proVitoria, proMitra, proTecnos, proSisPMJP,
                      proNFSeBrasil]) then
      Gerador.wGrupoNFSe('/Tomador')
    else
      Gerador.wGrupoNFSe('/TomadorServico');
    end;
   end
   else begin
     // Gera a TAG vazia quando nenhum dado do tomador for informado.
     if (VersaoXML = '1') or
        (FProvedor in [pro4R, proAgili, proCoplan, proDigifred, proFiorilli,
                      proGoiania, proGovDigital, proISSDigital, proISSe,
                      proProdata, proPVH, proSaatri, proVirtual, proFreire,
                      proLink3, proVitoria, proMitra, proTecnos, proSisPMJP]) then
       Gerador.wCampoNFSe(tcStr, '#', 'Tomador', 0, 1, 1, '', '')
     else
       Gerador.wCampoNFSe(tcStr, '#', 'TomadorServico', 0, 1, 1, '', '');
   end;
end;

procedure TNFSeW.GerarIntermediarioServico;
begin
 if (NFSe.IntermediarioServico.RazaoSocial<>'') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
 begin
   if VersaoXML = '1' then
   begin
     Gerador.wGrupoNFSe('IntermediarioServico');
     if FProvedor = proEL then
     begin
       Gerador.wCampoNFSe(tcStr, '#55', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
       Gerador.wCampoNFSe(tcStr, '#56', 'CpfCnpj', 14, 14, 1, SomenteNumeros(NFSe.IntermediarioServico.CpfCnpj), '');
       if Length(SomenteNumeros(NFSe.IntermediarioServico.CpfCnpj)) <= 11
         then Gerador.wCampoNFSe(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '1', '')
         else Gerador.wCampoNFSe(tcStr, '#57', 'IndicacaoCpfCnpj', 01, 01, 1, '2', '');
       Gerador.wCampoNFSe(tcStr, '#58', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
     end
     else begin
       Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
       Gerador.wGrupoNFSe('CpfCnpj');

       if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
         Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '')
       else
         Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

       Gerador.wGrupoNFSe('/CpfCnpj');
       Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
     end;
     Gerador.wGrupoNFSe('/IntermediarioServico');
   end
   else begin
     Gerador.wGrupoNFSe('Intermediario');
     Gerador.wGrupoNFSe('IdentificacaoIntermediario');
     Gerador.wGrupoNFSe('CpfCnpj');

     if FProvedor = proVirtual then
     begin
       if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj))<=11 then
       begin
         Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
         Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, '', '');
       end
       else begin
         Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, '', '');
         Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
       end;
     end
     else begin
       if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj))<=11 then
         Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '')
       else
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
     end;

     Gerador.wGrupoNFSe('/CpfCnpj');

     if FProvedor = proVirtual then
       Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 1, NFSe.IntermediarioServico.InscricaoMunicipal, '')
     else
       Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');

     Gerador.wGrupoNFSe('/IdentificacaoIntermediario');

     if FProvedor = proVirtual then
       Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 1, NFSe.IntermediarioServico.RazaoSocial, '')
     else
       Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');

     Gerador.wGrupoNFSe('/Intermediario');
   end;
 end;
end;

procedure TNFSeW.GerarConstrucaoCivil;
begin
 if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
 begin
   Gerador.wGrupoNFSe('ConstrucaoCivil');
   Gerador.wCampoNFSe(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, '');
   Gerador.wCampoNFSe(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, '');
   Gerador.wGrupoNFSe('/ConstrucaoCivil');
 end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW.GerarXML_ABRASF_V1;
begin
//  if FProvedor in [proLexsom, proPublica] then
//    FIdentificador := 'id';

  if (FIdentificador = '') {or (FProvedor = proPublica)}
    then Gerador.wGrupoNFSe('InfRps')
    else Gerador.wGrupoNFSe('InfRps ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
//    else Gerador.wGrupoNFSe('InfRps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');

   GerarIdentificacaoRPS;

   Gerador.wCampoNFSe(tcDatHor, '#4', 'DataEmissao     ', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
   Gerador.wCampoNFSe(tcStr,    '#5', 'NaturezaOperacao', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');

   if not (FProvedor in [proPublica, proDBSeller])
     then begin
       if (NFSe.RegimeEspecialTributacao <> retNenhum)
         then Gerador.wCampoNFSe(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');
     end;

   Gerador.wCampoNFSe(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
   Gerador.wCampoNFSe(tcStr, '#8', 'IncentivadorCultural  ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
   Gerador.wCampoNFSe(tcStr, '#9', 'Status                ', 01, 01, 1, StatusRPSToStr(NFSe.Status), '');

   if FProvedor in [proBetha, proFISSLex, proSimplISS]
    then Gerador.wCampoNFSe(tcStr, '#11', 'OutrasInformacoes', 001, 255, 0, NFSe.OutrasInformacoes, '');

   GerarRPSSubstituido;

   GerarServicoValores_V1;
   GerarPrestador;
   GerarTomador;
   GerarIntermediarioServico;
   GerarConstrucaoCivil;
   if (FProvedor = proBetha) then
     GerarCondicaoPagamento;

  Gerador.wGrupoNFSe('/InfRps');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW.GerarXML_ABRASF_V2;
begin
  case FProvedor of
   proDigifred,
   proFiorilli,
   proISSe,
   proISSDigital,
   proSisPMJP,
   proPVH,
   proMitra:     begin
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');
                   Gerador.wGrupoNFSe('Rps');
                 end;
   proGovDigital: begin
                    Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="Rps' + NFSe.InfID.ID + '"');
                    Gerador.wGrupoNFSe('Rps');
                  end;
   proSystemPro: begin
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
                 end;
   proTecnos:    begin
                   //Rodrigo Crovador - Adicionado o xmlns na tag InfDeclaracaoPrestacaoServico. Se removido, o provedor não reconhece a ass. digital
                   Gerador.WGrupoNFSe('tcDeclaracaoPrestacaoServico');
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');
                   Gerador.wGrupoNFSe('Rps');
                 end;
   proVirtual:   begin
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '=""');
                   Gerador.wGrupoNFSe('Rps ' + FIdentificador + '=""');
                 end;
   else          begin
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');
                   if FIdentificador = ''
                    then Gerador.wGrupoNFSe('Rps')
                    else Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');
                 end;
  end;

  GerarIdentificacaoRPS;

  if not (FProvedor in [proSystemPro]) then
  begin
    case FProvedor of
      proAgili,
      proCoplan,
      proFiorilli,
      proISSe,
      proISSDigital,
      proProdata,
      proPVH,
      proSaatri,
      proFreire,
      proVitoria,
      proVirtual,
      proMitra,
      proGovDigital,
      proSisPMJP,
      proActcon: Gerador.wCampoNFSe(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

//      proSisPMJP: Gerador.wCampoNFSe(tcStr, '#4', 'DataEmissao', 16, 16, 1, FormatDateTime('yyyy-mm-dd-hh:nn', NFSe.DataEmissao), DSC_DEMI);

      else Gerador.wCampoNFSe(tcDatHor, '#4', 'DataEmissao', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
    end;

    Gerador.wCampoNFSe(tcStr,    '#9', 'Status     ', 01, 01, 1, StatusRPSToStr(NFSe.Status), '');
  end;

  GerarRPSSubstituido;

  if FProvedor <> proSystemPro then
    Gerador.wGrupoNFSe('/Rps');


  if FProvedor in [profintelISS, proSystemPro] then
  begin
    GerarListaServicos;

    if NFSe.Competencia <> '' then
      Gerador.wCampoNFSe(tcStr,    '#4', 'Competencia', 10, 19, 1, NFSe.Competencia, DSC_DEMI)
    else
      Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
  end
  else begin
    if NFSe.Competencia <> '' then
    begin
      case FProvedor of
       proActcon,
       proISSDigital,
       proMitra,
       proPVH,
       proSisPMJP,
       proVirtual,
       proSystemPro: Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

       proGovDigital : Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, StrToDate(NFSe.Competencia), DSC_DEMI);  // Alterado por Nilton Olher - 20/02/2015

       proGoiania,
       proTecnos:  Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI);

       else        Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 19, 19, 1, NFSe.Competencia, DSC_DEMI);
      end;
    end
    else begin
      if FProvedor in [proPVH, proFreire, proISSe, proSystemPro, proFiorilli,
                       proSaatri, proCoplan, proISSDigital, proMitra,
                       proVitoria, proVirtual, proGovDigital, proProdata, proSisPMJP,
                       proActcon] then
        Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI)
      else begin
        if not (FProvedor in [proGoiania]) then
          Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
      end;
    end;

    if FProvedor in [proTecnos] then
      Gerador.wCampoNFSe(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio);

    GerarServicoValores_V2;
  end;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;

  if NFSe.RegimeEspecialTributacao <> retNenhum then
    Gerador.wCampoNFSe(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');

  if FProvedor = proTecnos then
  begin
    Gerador.wCampoNFSe(tcStr, '#7', 'NaturezaOperacao      ', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
    Gerador.wCampoNFSe(tcStr, '#8', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
    Gerador.wCampoNFSe(tcStr, '#9', 'IncentivoFiscal       ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
   end;

  if not (FProvedor in [proGoiania, proTecnos]) then
  begin
    Gerador.wCampoNFSe(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');
    Gerador.wCampoNFSe(tcStr, '#8', 'IncentivoFiscal       ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), '');
  end;

  if FProvedor = proTecnos then
    Gerador.wCampoNFSe(tcStr, '#9', 'OutrasInformacoes', 00, 255, 0, NFSe.OutrasInformacoes);

  if FProvedor = profintelISS then
    GerarValoresServico;

  if FProvedor in [proAgili, proISSDigital] then
    Gerador.wCampoNFSe(tcStr, '#9', 'Producao', 01, 01, 1, SimNaoToStr(NFSe.Producao), '');

  Gerador.wGrupoNFSe('/InfDeclaracaoPrestacaoServico');

  if FProvedor in [proTecnos] then
    Gerador.WGrupoNFSe('/tcDeclaracaoPrestacaoServico');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW.GerarServico_Provedor_IssDsf;
var
   i: integer;
   sDeducaoPor, sTipoDeducao, sTributavel: string;
begin

   Gerador.wGrupoNFSe('Itens');
   for i:=0 to NFSe.Servico.ItemServico.Count -1 do begin
     Gerador.wGrupoNFSe('Item');
	   sTributavel := EnumeradoToStr( NFSe.Servico.ItemServico.Items[i].Tributavel,
                                           ['S', 'N'], [snSim, snNao]);
	 
       Gerador.wCampoNFSe(tcStr, '', 'DiscriminacaoServico', 01, 80, 1, NFSe.Servico.ItemServico.Items[i].Descricao , '');
       Gerador.wCampoNFSe(tcDe4, '', 'Quantidade'          , 01, 15, 1, NFSe.Servico.ItemServico.Items[i].Quantidade , '');
       Gerador.wCampoNFSe(tcDe2, '', 'ValorUnitario'       , 01, 20, 1, NFSe.Servico.ItemServico.Items[i].ValorUnitario , '');
       Gerador.wCampoNFSe(tcDe2, '', 'ValorTotal'          , 01, 18, 1, NFSe.Servico.ItemServico.Items[i].ValorTotal , '');
       Gerador.wCampoNFSe(tcStr, '', 'Tributavel'          , 01, 01, 0, sTributavel , '');
     Gerador.wGrupoNFSe('/Item');
   end;
   Gerador.wGrupoNFSe('/Itens');

   for i:=0 to NFSe.Servico.Deducao.Count -1 do begin
      Gerador.wGrupoNFSe('Deducoes');
         Gerador.wGrupoNFSe('Deducao');

            sDeducaoPor := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].DeducaoPor,
                                           ['Percentual', 'Valor'], [dpPercentual, dpValor]);
            Gerador.wCampoNFSe(tcStr, '', 'DeducaoPor', 01, 20, 1, sDeducaoPor , '');

            sTipoDeducao := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].DeducaoPor,
                                            ['', 'Despesas com Materiais', 'Despesas com Sub-empreitada'],
                                            [tdNenhum, tdMateriais, tdSubEmpreitada]);
            Gerador.wCampoNFSe(tcStr, '', 'TipoDeducao', 00, 255, 1, sTipoDeducao , '');

            Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJReferencia'   , 00, 14, 1, OnlyNumber(NFSe.Servico.Deducao.Items[i].CpfCnpjReferencia) , '');
            Gerador.wCampoNFSe(tcStr, '', 'NumeroNFReferencia'  , 00, 10, 1, NFSe.Servico.Deducao.Items[i].NumeroNFReferencia, '');
            Gerador.wCampoNFSe(tcDe2, '', 'ValorTotalReferencia', 00, 18, 1, NFSe.Servico.Deducao.Items[i].ValorTotalReferencia, '');
            Gerador.wCampoNFSe(tcDe2, '', 'PercentualDeduzir'   , 00, 08, 1, NFSe.Servico.Deducao.Items[i].PercentualDeduzir, '');

         Gerador.wGrupoNFSe('/Deducao');
      Gerador.wGrupoNFSe('/Deducoes');
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TNFSeW.GerarXML_Provedor_IssDsf;
var
   sAssinatura, sSituacao, sTipoRecolhimento, sValorServico_Assinatura,
   sTipoRecolhimentoAssinaturaRPS: string;
begin
   Gerador.Prefixo := '';
  // Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="rps:' + NFSe.InfID.ID + '"');
   Gerador.wGrupoNFSe('RPS ' + FIdentificador + '="rps:' + NFSe.InfID.ID + '"');      //Alterado por Ailton 28/07/2014
                                                                                      //Alterado o item do grupo "RPS" passando de
                                                                                      // "Rps" para "RPS" no formato anterior dava erro na
                                                                                      // validação do XML>

      sSituacao := EnumeradoToStr( NFSe.Status,
                                   ['N','C'],
                                   [srNormal, srCancelado]);

      sTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                           ['A','R'],
                                           [stNormal, stRetencao]);

      // Wilker: ao assinar o RPS, a documentação indica trocar A por N:
      // "07 - Tipo Recolhimento, se for “A” preenche com “N” senão “S”"
      sTipoRecolhimentoAssinaturaRPS := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                           ['N','S'],
                                           [stNormal, stRetencao]);

      sValorServico_Assinatura := Poem_Zeros( OnlyNumber( FormatFloat('#0.00', (NFSe.Servico.Valores.ValorServicos - NFSe.Servico.Valores.ValorDeducoes) ) ), 15);

      sAssinatura := Poem_Zeros(NFSe.Prestador.InscricaoMunicipal, 11) +
                     PadRight( NFSe.IdentificacaoRps.Serie, 5 , ' ') +
                     Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12) +
                     FormatDateTime('yyyymmdd',NFse.DataEmissaoRps) +
                     EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','T'], [snSim, snNao])+ //ANTES ERA 'G', THIAGO FILIANO
                     ' ' +
                     sSituacao +
                     sTipoRecolhimentoAssinaturaRPS +
                     sValorServico_Assinatura +
                     Poem_Zeros( OnlyNumber( FormatFloat('#0.00',NFSe.Servico.Valores.ValorDeducoes)), 15 ) +
                     Poem_Zeros( OnlyNumber( NFSe.Servico.CodigoCnae ), 10 ) +
                     Poem_Zeros( OnlyNumber( NFSe.Tomador.IdentificacaoTomador.CpfCnpj), 14);

      sAssinatura := AsciiToHex(SHA1(sAssinatura));
      sAssinatura := LowerCase(sAssinatura);

      Gerador.wCampoNFSe(tcStr, '', 'Assinatura', 01, 2000, 1, sAssinatura, '');

      //Formatar segundo Cidade
      Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, NFSe.Prestador.InscricaoMunicipal, '');
      Gerador.wCampoNFSe(tcStr, '', 'RazaoSocialPrestador',        01, 120, 1, NFSe.PrestadorServico.RazaoSocial, '');
      Gerador.wCampoNFSe(tcStr, '', 'TipoRPS',                     01, 20,  1, 'RPS', '');

      if NFSe.IdentificacaoRps.Serie = '' then
         Gerador.wCampoNFSe(tcStr, '', 'SerieRPS', 01, 02,  1, 'NF', '')
      else
         Gerador.wCampoNFSe(tcStr, '', 'SerieRPS', 01, 02,  1, NFSe.IdentificacaoRps.Serie, '');

      Gerador.wCampoNFSe(tcStr,    '', 'NumeroRPS',      01, 12,  1, NFSe.IdentificacaoRps.Numero, '');
      Gerador.wCampoNFSe(tcDatHor, '', 'DataEmissaoRPS', 01, 21,  1, NFse.DataEmissaoRps, '');

      Gerador.wCampoNFSe(tcStr, '', 'SituacaoRPS', 01, 01, 1, sSituacao, '');

      if NFSe.RpsSubstituido.Numero <> '' then begin
         if NFSe.RpsSubstituido.Serie = '' then
            Gerador.wCampoNFSe(tcStr, '', 'SerieRPSSubstituido', 00, 02, 1, 'NF', '')
         else
            Gerador.wCampoNFSe(tcStr, '', 'SerieRPSSubstituido', 00, 02, 1, NFSe.RpsSubstituido.Serie, '');

         Gerador.wCampoNFSe(tcStr, '', 'NumeroNFSeSubstituida', 00, 02, 1, NFSe.RpsSubstituido.Numero, '');
         Gerador.wCampoNFSe(tcStr, '', 'NumeroRPSSubstituido',  00, 02, 1, NFSe.RpsSubstituido.Numero, '');
      end;

      if (NFSe.SeriePrestacao = '') then
         Gerador.wCampoNFSe(tcInt, '', 'SeriePrestacao', 01, 02, 1, '99', '')
      else
         Gerador.wCampoNFSe(tcInt, '', 'SeriePrestacao', 01, 02, 1, NFSe.SeriePrestacao, '');

      //TO DO - formatar segundo padrao da cidade
      Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalTomador', 01, 11,  1, '', '');

      Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJTomador',             01, 14,  1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
      Gerador.wCampoNFSe(tcStr, '', 'RazaoSocialTomador',         01, 120, 1, NFSe.Tomador.RazaoSocial, '');
      Gerador.wCampoNFSe(tcStr, '', 'DocTomadorEstrangeiro',      00, 20,  1, NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro, '');

      Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouroTomador',      00, 10,  1, NFSe.Tomador.Endereco.TipoLogradouro, '');

      Gerador.wCampoNFSe(tcStr, '', 'LogradouroTomador',          01, 50,  1, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wCampoNFSe(tcStr, '', 'NumeroEnderecoTomador',      01, 09,  1, NFSe.Tomador.Endereco.Numero, '');
      Gerador.wCampoNFSe(tcStr, '', 'ComplementoEnderecoTomador', 01, 30,  0, NFSe.Tomador.Endereco.Complemento, '');

      Gerador.wCampoNFSe(tcStr, '', 'TipoBairroTomador',      00, 10,  1, NFSe.Tomador.Endereco.TipoBairro, '');

      Gerador.wCampoNFSe(tcStr, '', 'BairroTomador',          01, 50,  1, NFSe.Tomador.Endereco.Bairro, '');

      Gerador.wCampoNFSe(tcStr, '', 'CidadeTomador',            01, 10,  1, CodCidadeToCodSiafi(strtoint64(NFSe.Tomador.Endereco.CodigoMunicipio)), '');
      if (Trim(NFSe.Tomador.Endereco.xMunicipio) <> '') then
        Gerador.wCampoNFSe(tcStr, '', 'CidadeTomadorDescricao', 01, 50,  1, NFSe.Tomador.Endereco.xMunicipio, '')
      else
        Gerador.wCampoNFSe(tcStr, '', 'CidadeTomadorDescricao', 01, 50,  1, CodCidadeToCidade(strtoint64(NFSe.Tomador.Endereco.CodigoMunicipio)), '');
      Gerador.wCampoNFSe(tcStr, '', 'CEPTomador',             01, 08,  1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
      Gerador.wCampoNFSe(tcStr, '', 'EmailTomador',           01, 60,  1, NFSe.Tomador.Contato.Email, '');

      Gerador.wCampoNFSe(tcStr, '', 'CodigoAtividade',        01, 09,  1, NFSe.Servico.CodigoCnae, '');
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaAtividade',      01, 11,  1, NFSe.Servico.Valores.Aliquota, '');

      // "A" a receber; "R" retido na Fonte
      Gerador.wCampoNFSe(tcStr, '', 'TipoRecolhimento',01, 01,  1, sTipoRecolhimento, '');

      Gerador.wCampoNFSe(tcStr, '', 'MunicipioPrestacao',          01, 10,  1, CodCidadeToCodSiafi(strtoint64(NFSe.Servico.CodigoMunicipio)), '');
      Gerador.wCampoNFSe(tcStr, '', 'MunicipioPrestacaoDescricao', 01, 30,  1, CodCidadeToCidade(strtoint64(NFSe.Servico.CodigoMunicipio)), '');

      if (NFSe.NaturezaOperacao in [noTributacaoNoMunicipio, noTributacaoForaMunicipio]) then begin

         Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, EnumeradoToStr( NFSe.DeducaoMateriais, ['B','A'], [snSim, snNao]), '');

         Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','T'], [snSim, snNao]), '');

      end
      else if(NFSe.NaturezaOperacao = noTributacaoForaMunicipio) then begin

         Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, EnumeradoToStr( NFSe.DeducaoMateriais, ['B','A'], [snSim, snNao]), '');

         Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','G'], [snSim, snNao]), '');

      end
      else if (NFSe.NaturezaOperacao = noIsencao) then begin
         Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, 'C', '');
         Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'C', '');
      end
      else if (NFSe.NaturezaOperacao = noImune) then begin
         Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, 'C', '');
         Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'F', '');
      end
      else if ( NFSe.NaturezaOperacao in [noSuspensaDecisaoJudicial, noSuspensaProcedimentoAdministrativo] ) then begin
         if NFSe.DeducaoMateriais = snSim then
            Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, 'B', '')
         else
            Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, 'A', '');

         Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'K', '');
      end
      else if ( NFSe.NaturezaOperacao = noNaoIncidencia) then begin
         Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01,  1, 'A', '');
         Gerador.wCampoNFSe(tcStr, '', 'Tributacao',01, 01,  1, 'N', '');
      end;

      if ( NFse.RegimeEspecialTributacao = retMicroempresarioIndividual ) then begin
         Gerador.wCampoNFSe(tcStr, '', 'Tributacao',01, 01,  1, 'M', '');
      end;

      //Valores
      Gerador.wCampoNFSe(tcDe2, '', 'ValorPIS',    01, 02, 1, NFSe.Servico.Valores.ValorPis, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS', 01, 02, 1, NFSe.Servico.Valores.ValorCofins, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorINSS',   01, 02, 1, NFSe.Servico.Valores.ValorInss, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorIR',     01, 02, 1, NFSe.Servico.Valores.ValorIr, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorCSLL',   01, 02, 1, NFSe.Servico.Valores.ValorCsll, '');

      //Aliquotas criar propriedades
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaPIS',    01, 02, 1, NFSe.Servico.Valores.AliquotaPIS, '');
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaCOFINS', 01, 02, 1, NFSe.Servico.Valores.AliquotaCOFINS, '');
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaINSS',   01, 02, 1, NFSe.Servico.Valores.AliquotaINSS, '');
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaIR',     01, 02, 1, NFSe.Servico.Valores.AliquotaIR, '');
      Gerador.wCampoNFSe(tcDe4, '', 'AliquotaCSLL',   01, 02, 1, NFSe.Servico.Valores.AliquotaCSLL, '');

      Gerador.wCampoNFSe(tcStr, '', 'DescricaoRPS',      01, 1500, 1, NFSe.OutrasInformacoes, '');

      if Length(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone)) = 11 then begin
         Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),3), '');
      end else
      if Length(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone)) = 10 then begin
         Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),2), '');
      end else
         Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, '', '');
      Gerador.wCampoNFSe(tcStr, '', 'TelefonePrestador', 00, 08, 1, RightStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),8), '');

      if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then begin
         Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),3), '');
      end else
      if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then begin
         Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),2), '');
      end else
         Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, '', '');

      Gerador.wCampoNFSe(tcStr, '', 'TelefoneTomador', 00, 08, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),8), '');

      if (NFSe.Status = srCancelado) then
         Gerador.wCampoNFSe(tcStr, '', 'MotCancelamento',01, 80, 1, NFSE.MotivoCancelamento, '')
      else
         Gerador.wCampoNFSe(tcStr, '', 'MotCancelamento',00, 80, 0, NFSE.MotivoCancelamento, '');

      Gerador.wCampoNFSe(tcStr, '', 'CpfCnpjIntermediario', 00, 14, 0, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

      GerarServico_Provedor_IssDsf;

   //Gerador.wGrupoNFSe('/Rps');
   Gerador.wGrupoNFSe('/RPS'); //Alterado por Ailton 28/07/2014
                               //Alterado o item do grupo "RPS" passando de
                               // "/Rps" para "/RPS" no formato anterior dava erro na
                               // validação do XML>
end;

procedure TNFSeW.GerarXML_Provedor_Infisc;
var
   i:integer;
   dTotBCISS,dTotISS:double;
   sChave,cNFSe,serie,nNFSe:string;
   cServ,xServ:string;
begin
   sChave := NFSe.ChaveNFSe;
   serie  := NFSE.SeriePrestacao;
   nNFSe  := NFSe.Numero;
   cNFSe  := copy(sChave,31,9);
   dTotBCISS := 0;
   dTotISS   := 0;

   // Alterado Por Moro em 18/02/2015
   if versaoXML = '1.1' then begin // para Caxias do Sul Versão XML = 1.1

      // Cab
      Gerador.Prefixo := '';
      Gerador.wGrupoNFSe('?xml version=''1.0'' encoding=''utf-8''?');
      Gerador.wGrupoNFSe('envioLote versao="1.0"');
        Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Prestador.Cnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'dhTrans', 01, 19,  1, FormatDateTime('yyyy-mm-dd hh:mm:ss',Now), '');

      // NFS-e
      Gerador.wGrupoNFSe('NFS-e');
        Gerador.wGrupoNFSe('infNFSe versao='''+ VersaoXML +'''');

      // ID
      Gerador.wGrupoNFSe('Id');
        Gerador.wCampoNFSe(tcStr, '', 'cNFS-e', 01, 09,  1, cNFSe, '');
        Gerador.wCampoNFSe(tcStr, '', 'mod', 01, 02,  1, NFSe.ModeloNFSe, '');  // segundo manual obrigatório ser 98
        Gerador.wCampoNFSe(tcStr, '', 'serie', 01, 03,  1, serie, '');          // tem que ser S - ver como está sendo passado a variável "serie"
        Gerador.wCampoNFSe(tcStr, '', 'nNFS-e', 01, 09,  1, nNFSe, '');
        Gerador.wCampoNFSe(tcStr, '', 'dEmi', 01, 10,  1, FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), '');
        Gerador.wCampoNFSe(tcStr, '', 'hEmi', 01, 10,  1, FormatDateTime('hh:mm',NFSe.DataEmissao), '');
        Gerador.wCampoNFSe(tcStr, '', 'tpNF', 01, 01,  1, '1', '');     // 0- Entrada 1- Saída
        Gerador.wCampoNFSe(tcStr, '', 'refNF', 01, 39,  1, sChave, ''); // chave de acesso 39 caracteres
        Gerador.wCampoNFSe(tcStr, '', 'tpEmis', 01, 01,  1, TipoEmissaoToStr(NFSe.TipoEmissao), ''); // N- Normal C- Contigencia

        if NFSe.Cancelada = snNao then
          Gerador.wCampoNFSe(tcStr, '', 'cancelada', 01, 01,  1, 'N', '')    // N- Não
        else
          Gerador.wCampoNFSe(tcStr, '', 'cancelada', 01, 01,  1, 'S', '');   // S- Sim

        Gerador.wCampoNFSe(tcStr, '', 'ambienteEmi', 01, 01,  1, SimNaoToStr(NFSe.Producao), ''); // ambiente 1- producao 2- homologacao
        // forma de emissao 1- portal contribuinte 2- servidor web 3- submetidos via Upload no portal 4- emissao via RPS
        Gerador.wCampoNFSe(tcStr, '', 'formaEmi', 01, 01,  1, '2', '');
        // 1- construcao civil 2- outros casos
        Gerador.wCampoNFSe(tcStr, '', 'empreitadaGlobal', 01, 01,  1, EmpreitadaGlobalToStr(NFSe.EmpreitadaGlobal), '');
      Gerador.wGrupoNFSe('/Id');

      // prest
      Gerador.wGrupoNFSe('prest');
        Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Prestador.Cnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100,  1, NFSe.PrestadorServico.RazaoSocial, '');
        Gerador.wCampoNFSe(tcStr, '', 'IM', 01, 15,  1, NFSe.Prestador.InscricaoMunicipal, '');

        Gerador.wGrupoNFSe('end');
          Gerador.wCampoNFSe(tcStr, '', 'xLgr', 01, 100,  1, NFSe.PrestadorServico.Endereco.Endereco, '');
          Gerador.wCampoNFSe(tcStr, '', 'nro', 01, 15,  1, NFSe.PrestadorServico.Endereco.Numero, '');
          Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100,  1, NFSe.PrestadorServico.Endereco.Bairro, '');
          Gerador.wCampoNFSe(tcStr, '', 'cMun', 01, 07,  1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');
          Gerador.wCampoNFSe(tcStr, '', 'xMun', 01, 60,  1, copy(CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)),
                                                      0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)))-1), '');
          Gerador.wCampoNFSe(tcStr, '', 'UF', 01, 02,  1, NFSe.PrestadorServico.Endereco.UF, '');
          Gerador.wCampoNFSe(tcStr, '', 'CEP', 01, 08,  1, NFSe.PrestadorServico.Endereco.CEP, '');
          Gerador.wCampoNFSe(tcInt, '', 'cPais', 01, 100,  1, NFSe.PrestadorServico.Endereco.CodigoPais, '');
          Gerador.wCampoNFSe(tcStr, '', 'xPais', 01, 100,  1, NFSe.PrestadorServico.Endereco.xPais, '');
        Gerador.wGrupoNFSe('/end');

        Gerador.wCampoNFSe(tcStr, '', 'fone', 01, 100,  1, NFSe.PrestadorServico.Contato.Telefone, '');
        //Alterado por Moro em 27/02/2015
        Gerador.wCampoNFSe(tcStr, '', 'IE', 01, 15,  1, NFSe.Prestador.InscricaoEstadual, '');

        case Nfse.RegimeEspecialTributacao of
          retNenhum,
          retMicroempresaMunicipal,
          retEstimativa,
          retSociedadeProfissionais,
          retCooperativa,
          retMicroempresarioIndividual    : ;// Não tem informação no manual

          retSimplesNacional              : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01,  1, '1', ''); // 1 - Simples

          retMicroempresarioEmpresaPP     : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01,  1, '2', ''); // 2 - SIMEI

          retLucroReal, retLucroPresumido : Gerador.wCampoNFSe(tcStr, '', 'regimeTrib', 01, 01,  1, '3', ''); // 3 - Normal
        end;

      Gerador.wGrupoNFSe('/prest');

      // TomS
      Gerador.wGrupoNFSe('TomS');
        if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) = 11 then
           Gerador.wCampoNFSe(tcStr, '', 'CPF', 01, 11,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '')
        else
           Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100,  1, NFSe.Tomador.RazaoSocial, '');

        Gerador.wGrupoNFSe('ender');
          Gerador.wCampoNFSe(tcStr, '', 'xLgr', 01, 100,  1, NFSe.Tomador.Endereco.Endereco, '');
          Gerador.wCampoNFSe(tcStr, '', 'nro', 01, 15,  1, NFSe.Tomador.Endereco.Numero, '');
          Gerador.wCampoNFSe(tcStr, '', 'xCpl', 01, 100,  1, NFSe.Tomador.Endereco.Complemento, '');
          Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100,  1, NFSe.Tomador.Endereco.Bairro, '');
          Gerador.wCampoNFSe(tcStr, '', 'cMun', 01, 07,  1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
          Gerador.wCampoNFSe(tcStr, '', 'xMun', 01, 60,  1, copy(CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)),
                                                                 0, pos('/',CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)))-1), '');
          Gerador.wCampoNFSe(tcStr, '', 'UF', 01, 02,  1, NFSe.Tomador.Endereco.UF, '');
          Gerador.wCampoNFSe(tcStr, '', 'CEP', 01, 08,  1, NFSe.Tomador.Endereco.CEP, '');
          Gerador.wCampoNFSe(tcInt, '', 'cPais', 01, 100,  1, NFSe.Tomador.Endereco.CodigoPais, '');
          Gerador.wCampoNFSe(tcStr, '', 'xPais', 01, 100,  1, NFSe.Tomador.Endereco.xPais, '');
        Gerador.wGrupoNFSe('/ender');
        //Alterado por Moro em 27/02/2015
        Gerador.wCampoNFSe(tcStr, '', 'IE', 01, 15,  1, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
      Gerador.wGrupoNFSe('/TomS');

      // Serviços
      for i:=0 to NFSe.Servico.ItemServico.Count-1 do
      begin
        cServ   := NFSe.Servico.ItemServico.Items[i].codServ; // cod. municipal
        xServ   := NFSe.Servico.ItemServico.Items[i].Descricao;

        // Item
        Gerador.wGrupoNFSe('det');
          Gerador.wCampoNFSe(tcStr, '', 'nItem', 01, 02,  1, IntToStr(i+1), '');

          Gerador.wGrupoNFSe('serv');
            Gerador.wCampoNFSe(tcStr, '', 'cServ', 01, 02,  1, cServ, '');  // Código municipal do serviço, apenas números
            Gerador.wCampoNFSe(tcStr, '', 'cLCServ', 01, 04,  1, NFSe.Servico.ItemServico.Items[i].codLCServ, ''); // Código do serviço conforme lei compl. 116
            Gerador.wCampoNFSe(tcStr, '', 'xServ', 01, 120,  1, xServ, ''); // Discriminação do serviço
            Gerador.wCampoNFSe(tcStr, '', 'localTributacao', 01, 04,  1, IntToStr(NFSe.Servico.MunicipioIncidencia), ''); // Local tributacao conforme codificacao do IBGE
            Gerador.wCampoNFSe(tcStr, '', 'localVerifResServ', 01, 04,  1, '1', ''); // Local da verificacao: 1 - Brasil 2 - Exterior
            Gerador.wCampoNFSe(tcStr, '', 'uTrib', 01, 06,  1, NFSe.Servico.ItemServico.Items[i].Unidade, '');       // unidade
            Gerador.wCampoNFSe(tcDe2, '', 'qTrib', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');    // quantidade
            Gerador.wCampoNFSe(tcDe2, '', 'vUnit', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''); // formatacao com 2 casas
            // (valor unitario * quantidade) - desconto
            Gerador.wCampoNFSe(tcDe2, '', 'vServ', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
            Gerador.wCampoNFSe(tcDe2, '', 'vDesc', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, '');

            Gerador.wCampoNFSe(tcDe2, '', 'vBCISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
            Gerador.wCampoNFSe(tcDe2, '', 'pISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].Aliquota, '');
            Gerador.wCampoNFSe(tcDe2, '', 'vISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorIss, '');
            dTotBCISS := dTotBCISS + NFSe.Servico.ItemServico.Items[i].BaseCalculo;
            dTotISS   := dTotISS   + NFSe.Servico.ItemServico.Items[i].ValorIss;

            //Alterado por Moro em 27/02/2015
            // Retenção INSS
            if NFSe.Servico.ItemServico.Items[i].ValorInss > 0 then
              Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorInss, '');
            // Retenção IRRF
            if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetIR', 01, 02,  1, 'Retenção IRRF', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetIR', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
            end;
            // Retenção PIS
            if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetPISPASEP', 01, 02,  1, 'Retenção PIS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetPISPASEP', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
            end;
            // Retenção COFINS
            if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetCOFINS', 01, 02,  1, 'Retenção COFINS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetCOFINS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
            end;
            // Retenção CSLL
            if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetCSLL', 01, 02,  1, 'Retenção CSLL', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetCSLL', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
            end;
          Gerador.wGrupoNFSe('/serv');

        Gerador.wGrupoNFSe('/det');
      end;

      // Total
      Gerador.wGrupoNFSe('total');
        Gerador.wCampoNFSe(tcDe2, '', 'vServ', 01, 15,  1, NFSe.Servico.Valores.ValorServicos, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vDesc', 01, 15,  1, NFSe.Servico.Valores.DescontoIncondicionado, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vtNF', 01, 15,  1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vtLiq', 01, 15,  1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
        Gerador.wCampoNFSe(tcDe2, '', 'totalAproxTrib', 01, 15,  1, 0, '');

        // Total Retenção IRRF
        if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
            NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
            NFSe.Servico.Valores.ValorInss) > 0 then
        begin
          //Alterado por Moro em 27/02/2015
          Gerador.wGrupoNFSe('Ret');
            if NFSe.Servico.Valores.ValorIr > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'xRetIR', 01, 02,  1, 'Retenção IRRF', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetIR', 01, 15,  1, NFSe.Servico.Valores.ValorIr, '');
            end;
            if NFSe.Servico.Valores.ValorPis > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetPISPASEP', 01, 02,  1, 'Retenção PIS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetPISPASEP', 01, 15,  1, NFSe.Servico.Valores.ValorPis, '');
            end;
            if NFSe.Servico.Valores.ValorCofins > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetCOFINS', 01, 02,  1, 'Retenção COFINS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetCOFINS', 01, 15,  1, NFSe.Servico.Valores.ValorCofins, '');
            end;
            if NFSe.Servico.Valores.ValorCsll > 0 then
            begin
              //Gerador.wCampoNFSe(tcStr, '', 'pRetCSLL', 01, 02,  1, 'Retenção CSLL', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetCSLL', 01, 15,  1, NFSe.Servico.Valores.ValorCsll, '');
            end;
            if NFSe.Servico.Valores.ValorInss > 0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetINSS', 01, 02,  1, 'Retenção INSS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15,  1, NFSe.Servico.Valores.ValorInss, '');
            end;
          Gerador.wGrupoNFSe('/Ret');
        end;

        Gerador.wCampoNFSe(tcDe2, '', 'vtLiqFaturas', 01, 15,  1, NFSe.Servico.Valores.ValorLiquidoNfse, ''); // tem tag <vtLiqFaturas>

        // Total Retenção ISSQN
        Gerador.wGrupoNFSe('ISS');
          Gerador.wCampoNFSe(tcDe2, '', 'vBCISS', 01, 15,  1, dTotBCISS, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vISS', 01, 15,  1, dTotISS, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vBCSTISS', 01, 15,  1, 0, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vSTISS', 01, 15,  1, NFSe.Servico.Valores.ValorIssRetido, '');
        Gerador.wGrupoNFSe('/ISS');
      Gerador.wGrupoNFSe('/total');

      Gerador.wGrupoNFSe('faturas');
        for i:=0 to NFSe.CondicaoPagamento.Parcelas.Count-1 do
        begin
          Gerador.wGrupoNFSe('fat');
            Gerador.wCampoNFSe(tcStr, '', 'nItem', 01, 15,  1, IntToStr(i+1), '');
            Gerador.wCampoNFSe(tcStr, '', 'nFat', 01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].Parcela, '');
            Gerador.wCampoNFSe(tcDat, '', 'dVenc', 01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].DataVencimento, DSC_DEMI);
            Gerador.wCampoNFSe(tcDe2, '', 'vFat', 01, 15,  1, NFSe.CondicaoPagamento.Parcelas[i].Valor, '');
          Gerador.wGrupoNFSe('/fat');
        end;
      Gerador.wGrupoNFSe('/faturas');

      Gerador.wCampoNFSe(tcStr, '', 'infAdicLT', 01, 100,  1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');

      // Finaliza
      Gerador.wGrupoNFSe('/infNFSe');
      Gerador.wGrupoNFSe('/NFS-e');
      Gerador.wGrupoNFSe('/envioLote');

   end; // fim do if Caxias do Sul

   if versaoXML = '1' then begin //demais cidades
      // Cab
      Gerador.Prefixo := '';
      Gerador.wGrupoNFSe('?xml version=''1.0'' encoding=''utf-8''?');
      Gerador.wGrupoNFSe('envioLote versao="1.0"');
        Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Prestador.Cnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'dhTrans', 01, 19,  1, FormatDateTime('yyyy-mm-dd hh:mm:ss',Now), '');

      // NFS-e
      Gerador.wGrupoNFSe('NFS-e');
      Gerador.wGrupoNFSe('infNFSe versao=''1.00''');

      // ID
      Gerador.wGrupoNFSe('Id');
        Gerador.wCampoNFSe(tcStr, '', 'cNFS-e', 01, 09,  1, cNFSe, '');
        Gerador.wCampoNFSe(tcStr, '', 'natOp', 01, 50,  1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
        Gerador.wCampoNFSe(tcStr, '', 'mod', 01, 02,  1, '55', '');
        Gerador.wCampoNFSe(tcStr, '', 'serie', 01, 03,  1, serie, '');
        Gerador.wCampoNFSe(tcStr, '', 'nNFS-e', 01, 09,  1, nNFSe, '');
        Gerador.wCampoNFSe(tcStr, '', 'dEmi', 01, 10,  1, FormatDateTime('yyyy-mm-dd',NFSe.DataEmissao), '');
        Gerador.wCampoNFSe(tcStr, '', 'hEmi', 01, 10,  1, FormatDateTime('hh:mm',NFSe.DataEmissao), '');
        Gerador.wCampoNFSe(tcStr, '', 'tpNF', 01, 01,  1, '1', ''); // 0.Entrada 1.Saída
        Gerador.wCampoNFSe(tcStr, '', 'cMunFG', 01, 07,  1, NFSe.Servico.CodigoMunicipio, '');
        Gerador.wCampoNFSe(tcStr, '', 'refNF', 01, 39,  1, sChave, '');
        Gerador.wCampoNFSe(tcStr, '', 'tpEmis', 01, 01,  1, 'N', '');
        Gerador.wCampoNFSe(tcStr, '', 'anulada', 01, 01,  1, 'N', '');
      Gerador.wGrupoNFSe('/Id');

      // Emit
      Gerador.wGrupoNFSe('emit');
        Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Prestador.Cnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100,  1, NFSe.PrestadorServico.RazaoSocial, '');
        Gerador.wCampoNFSe(tcStr, '', 'xFant', 01, 60,  1, NFSe.PrestadorServico.NomeFantasia, '');
        Gerador.wCampoNFSe(tcStr, '', 'IM', 01, 15,  1, NFSe.Prestador.InscricaoMunicipal, '');
        Gerador.wGrupoNFSe('end');
          Gerador.wCampoNFSe(tcStr, '', 'xLgr', 01, 100,  1, NFSe.PrestadorServico.Endereco.Endereco, '');
          Gerador.wCampoNFSe(tcStr, '', 'nro', 01, 15,  1, NFSe.PrestadorServico.Endereco.Numero, '');
          Gerador.wCampoNFSe(tcStr, '', 'xCpl', 01, 100,  1, NFSe.PrestadorServico.Endereco.Complemento, '');
          Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100,  1, NFSe.PrestadorServico.Endereco.Bairro, '');
          Gerador.wCampoNFSe(tcStr, '', 'cMun', 01, 07,  1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, '');
          Gerador.wCampoNFSe(tcStr, '', 'xMun', 01, 60,  1, CodCidadeToCidade(StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio,0)), '');
          Gerador.wCampoNFSe(tcStr, '', 'UF', 01, 02,  1, NFSe.PrestadorServico.Endereco.UF, '');
          Gerador.wCampoNFSe(tcStr, '', 'CEP', 01, 08,  1, NFSe.PrestadorServico.Endereco.CEP, '');
          Gerador.wCampoNFSe(tcStr, '', 'cPais', 01, 100,  1, '1058', '');
          Gerador.wCampoNFSe(tcStr, '', 'xPais', 01, 100,  1, 'Brasil', '');
          Gerador.wCampoNFSe(tcStr, '', 'fone', 01, 100,  1, NFSe.PrestadorServico.Contato.Telefone, '');
        Gerador.wGrupoNFSe('/end');
        Gerador.wCampoNFSe(tcStr, '', 'xEmail', 01, 100,  1, NFSe.PrestadorServico.Contato.Email, '');
      Gerador.wGrupoNFSe('/emit');

      // TomS
      Gerador.wGrupoNFSe('TomS');
        if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)=11 then
          Gerador.wCampoNFSe(tcStr, '', 'CPF', 01, 11,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '')
        else
          Gerador.wCampoNFSe(tcStr, '', 'CNPJ', 01, 14,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
        Gerador.wCampoNFSe(tcStr, '', 'xNome', 01, 100,  1, NFSe.Tomador.RazaoSocial, '');
        Gerador.wGrupoNFSe('ender');
          Gerador.wCampoNFSe(tcStr, '', 'xLgr', 01, 100,  1, NFSe.Tomador.Endereco.Endereco, '');
          Gerador.wCampoNFSe(tcStr, '', 'nro', 01, 15,  1, NFSe.Tomador.Endereco.Numero, '');
          Gerador.wCampoNFSe(tcStr, '', 'xCpl', 01, 100,  1, NFSe.Tomador.Endereco.Complemento, '');
          Gerador.wCampoNFSe(tcStr, '', 'xBairro', 01, 100,  1, NFSe.Tomador.Endereco.Bairro, '');
          Gerador.wCampoNFSe(tcStr, '', 'cMun', 01, 07,  1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
          Gerador.wCampoNFSe(tcStr, '', 'xMun', 01, 60,  1, CodCidadeToCidade(StrToIntDef(NFSe.Tomador.Endereco.CodigoMunicipio,0)), '');
          Gerador.wCampoNFSe(tcStr, '', 'UF', 01, 02,  1, NFSe.Tomador.Endereco.UF, '');
          Gerador.wCampoNFSe(tcStr, '', 'CEP', 01, 08,  1, NFSe.Tomador.Endereco.CEP, '');
          Gerador.wCampoNFSe(tcStr, '', 'cPais', 01, 100,  1, '1058', '');
          Gerador.wCampoNFSe(tcStr, '', 'xPais', 01, 100,  1, 'Brasil', '');
          Gerador.wCampoNFSe(tcStr, '', 'fone', 01, 100,  1, NFSe.Tomador.Contato.Telefone, '');
        Gerador.wGrupoNFSe('/ender');
        Gerador.wCampoNFSe(tcStr, '', 'xEmail', 01, 100,  1, NFSe.Tomador.Contato.Email, '');
        Gerador.wCampoNFSe(tcStr, '', 'IM', 01, 15,  1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

        // Campo Bom Deve ir Diferente...
        // 15/01/2015 - Leandro do Couto
        if NFSe.Servico.MunicipioIncidencia <> 0 then
        if( NFSe.Servico.MunicipioIncidencia = 4303905 ) then
        begin
         Gerador.wCampoNFSe(tcStr, '', 'Praca', 01, 60,  1, 'Campo Bom-RS', '');
        end
        else
        begin
          Gerador.wCampoNFSe(tcStr, '', 'Praca', 01, 60,  1, CodCidadeToCidade(NFSe.Servico.MunicipioIncidencia), '');
        end;
      Gerador.wGrupoNFSe('/TomS');

      // Serviços
      for i:=0 to NFSe.Servico.ItemServico.Count-1 do
      begin

        // Código/Descrição
        cServ := NFSe.Servico.ItemServico.Items[i].Codigo;
        xServ := NFSe.Servico.ItemServico.Items[i].Descricao;
        if NFSe.Servico.ItemListaServico <> '' then
          xServ := xServ + ' (Class.: ' + NFSe.Servico.ItemListaServico+')';

        // Item
        Gerador.wGrupoNFSe('det');
        Gerador.wCampoNFSe(tcStr, '', 'nItem', 01, 02,  1, IntToStr(i+1), '');
        Gerador.wGrupoNFSe('serv');
          Gerador.wCampoNFSe(tcStr, '', 'cServ', 01, 02,  1, cServ, '');
          Gerador.wCampoNFSe(tcStr, '', 'xServ', 01, 120,  1, xServ, '');
          Gerador.wCampoNFSe(tcStr, '', 'uTrib', 01, 06,  1, 'UN', '');
          Gerador.wCampoNFSe(tcDe2, '', 'qTrib', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].Quantidade, '');
          Gerador.wCampoNFSe(tcDe3, '', 'vUnit', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorUnitario, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vServ', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorServicos, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vDesc', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, '');

          Gerador.wCampoNFSe(tcDe2, '', 'vBCISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].BaseCalculo, '');
          Gerador.wCampoNFSe(tcDe2, '', 'pISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].Aliquota, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vISS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorIss, '');
          dTotBCISS := dTotBCISS + NFSe.Servico.ItemServico.Items[i].BaseCalculo;
          dTotISS   := dTotISS   + NFSe.Servico.ItemServico.Items[i].ValorIss;

          Gerador.wCampoNFSe(tcDe2, '', 'pRed', 01, 15,  1, 0, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vRed', 01, 15,  1, 0, '');

          // Retenção INSS
          if NFSe.Servico.ItemServico.Items[i].ValorInss > 0 then
            Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorInss, '');
          // Retenção IRRF
          if NFSe.Servico.ItemServico.Items[i].ValorIr > 0 then
          begin
            Gerador.wCampoNFSe(tcStr, '', 'xRetIRF', 01, 02,  1, 'Retenção IRRF', '');
            Gerador.wCampoNFSe(tcDe2, '', 'vRetIRF', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorIr, '');
          end;
          // Retenção PIS
          if NFSe.Servico.ItemServico.Items[i].ValorPis > 0 then
          begin
            Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02,  1, 'Retenção PIS', '');
            Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorPis, '');
          end;
          // Retenção COFINS
          if NFSe.Servico.ItemServico.Items[i].ValorCofins > 0 then
          begin
            Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-COFINS', 01, 02,  1, 'Retenção COFINS', '');
            Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-COFINS', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCofins, '');
          end;
          // Retenção CSLL
          if NFSe.Servico.ItemServico.Items[i].ValorCsll > 0 then
          begin
            Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-CSLL', 01, 02,  1, 'Retenção CSLL', '');
            Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-CSLL', 01, 15,  1, NFSe.Servico.ItemServico.Items[i].ValorCsll, '');
          end;

        Gerador.wGrupoNFSe('/serv');

        // Retenção ISSQN
        Gerador.wGrupoNFSe('ISSST');
        Gerador.wCampoNFSe(tcDe2, '', 'vISSST', 01, 15,  1, 0, '');
        Gerador.wGrupoNFSe('/ISSST');

        Gerador.wGrupoNFSe('/det');
      end;

      // Total
      Gerador.wGrupoNFSe('total');
        Gerador.wCampoNFSe(tcDe2, '', 'vServ', 01, 15,  1, NFSe.Servico.Valores.ValorServicos, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vDesc', 01, 15,  1, NFSe.Servico.Valores.DescontoIncondicionado, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vOutro', 01, 15,  1, 0, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vtNF', 01, 15,  1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vtLiq', 01, 15,  1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
        Gerador.wCampoNFSe(tcDe2, '', 'totalAproxTrib', 01, 15,  1, 0, '');

        // Total Retenção IRRF
        if (NFSe.Servico.Valores.ValorIr + NFSe.Servico.Valores.ValorPis +
            NFSe.Servico.Valores.ValorCofins + NFSe.Servico.Valores.ValorCsll +
            NFSe.Servico.Valores.ValorInss) > 0 then
        begin
          Gerador.wGrupoNFSe('Ret');
            if NFSe.Servico.Valores.ValorIr>0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetIRF', 01, 02,  1, 'Retenção IRRF', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetIRF', 01, 15,  1, NFSe.Servico.Valores.ValorIr, '');
            end;
            if NFSe.Servico.Valores.ValorPis>0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-PIS-PASEP', 01, 02,  1, 'Retenção PIS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-PIS-PASEP', 01, 15,  1, NFSe.Servico.Valores.ValorPis, '');
            end;
            if NFSe.Servico.Valores.ValorCofins>0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-COFINS', 01, 02,  1, 'Retenção COFINS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-COFINS', 01, 15,  1, NFSe.Servico.Valores.ValorCofins, '');
            end;
            if NFSe.Servico.Valores.ValorCsll>0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetLei10833-CSLL', 01, 02,  1, 'Retenção CSLL', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetLei10833-CSLL', 01, 15,  1, NFSe.Servico.Valores.ValorCsll, '');
            end;
            if NFSe.Servico.Valores.ValorInss>0 then
            begin
              Gerador.wCampoNFSe(tcStr, '', 'xRetINSS', 01, 02,  1, 'Retenção INSS', '');
              Gerador.wCampoNFSe(tcDe2, '', 'vRetINSS', 01, 15,  1, NFSe.Servico.Valores.ValorInss, '');
            end;
          Gerador.wGrupoNFSe('/Ret');
        end;
        // Total Retenção ISSQN
        Gerador.wGrupoNFSe('ISS');
          Gerador.wCampoNFSe(tcDe2, '', 'vBCISS', 01, 15,  1, dTotBCISS, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vISS', 01, 15,  1, dTotISS, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vBCSTISS', 01, 15,  1, 0, '');
          Gerador.wCampoNFSe(tcDe2, '', 'vSTISS', 01, 15,  1, NFSe.Servico.Valores.ValorIssRetido, '');
        Gerador.wGrupoNFSe('/ISS');
      Gerador.wGrupoNFSe('/total');

      // Lay-Out Infisc não possui campo específicos
      Gerador.wCampoNFSe(tcStr, '', 'infAdic', 01, 100,  1, NFSE.Servico.ItemListaServico, '');

      // OBS
      if Trim(NFSe.OutrasInformacoes) <> '' then
      begin
        Gerador.wGrupoNFSe('Observacoes');
          Gerador.wCampoNFSe(tcStr, '', 'xinf', 01, 100,  1, copy(NFSe.OutrasInformacoes,1,100), '');
        Gerador.wGrupoNFSe('/Observacoes');
      end;

      // Finaliza
      Gerador.wGrupoNFSe('/infNFSe');
      Gerador.wGrupoNFSe('/NFS-e');
      Gerador.wGrupoNFSe('/envioLote');
   end; //fim do if demais cidades
end;

procedure TNFSeW.GerarXML_Provedor_EgoverneISS;
begin
   Gerador.Prefixo := 'rgm:';
   Gerador.wGrupoNFSe('NotaFiscal');
   Gerador.Prefixo := 'rgm1:';
   Gerador.wCampoNFSe(tcDe4, '', 'Aliquota',                       01,   15, 1, NFSe.Servico.Valores.Aliquota, '');
   Gerador.wCampoNFSe(tcStr, '', 'Atividade',                      01,   09, 1, NFSe.Servico.CodigoTributacaoMunicipio, '');
   Gerador.wCampoNFSe(tcStr, '', 'ChaveAutenticacao',              01,   36, 1, NFSe.Prestador.Senha, '');
   Gerador.wCampoNFSe(tcStr, '', 'Homologacao',                    05,   05, 1, ifThen(SimNaoToStr(NFSe.Producao) = '1', 'false', 'true'), '');
   Gerador.wCampoNFSe(tcStr, '', 'NotificarTomadorPorEmail',       05,   05, 1, 'false', '');
   Gerador.wCampoNFSe(tcStr, '', 'SubstituicaoTributaria',         05,    5, 1, 'false', '');
   Gerador.wCampoNFSe(tcStr, '', 'InformacoesAdicionais',          00, 2300, 0, NFSe.OutrasInformacoes, '');
   Gerador.wGrupoNFSe('Tomador');
   Gerador.Prefixo := 'rgm2:';
   if Length(NFSE.Tomador.IdentificacaoTomador.CpfCnpj) > 11 then
   begin
     Gerador.wCampoNFSe(tcStr, '', 'CNPJ',                        01, 14,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
     Gerador.wCampoNFSe(tcStr, '', 'CPF',                         01, 14,  1, '', '');
   end
   else
   begin
     Gerador.wCampoNFSe(tcStr, '', 'CNPJ',                        01, 14,  1, '', '');
     Gerador.wCampoNFSe(tcStr, '', 'CPF',                         01, 14,  1, NFSe.Tomador.IdentificacaoTomador.CpfCnpj, '');
   end;
   Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipal',            01, 11,  0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');
   Gerador.wCampoNFSe(tcStr, '', 'Nome',                          01, 120, 1, NFSe.Tomador.RazaoSocial, '');
   Gerador.wGrupoNFSe('Endereco');
   Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouro',                00, 10,  1, NFSe.Tomador.Endereco.TipoLogradouro, '');
   Gerador.wCampoNFSe(tcStr, '', 'Logradouro',                    01, 50,  1, NFSe.Tomador.Endereco.Endereco, '');
   Gerador.wCampoNFSe(tcStr, '', 'Numero',                        01, 09,  1, NFSe.Tomador.Endereco.Numero, '');
   Gerador.wCampoNFSe(tcStr, '', 'Complemento',                   01, 30,  0, NFSe.Tomador.Endereco.Complemento, '');
   Gerador.wCampoNFSe(tcStr, '', 'Bairro',                        01, 50,  1, NFSe.Tomador.Endereco.Bairro, '');
   Gerador.wCampoNFSe(tcStr, '', 'Cidade',                        01, 50,  1, NFSe.Tomador.Endereco.xMunicipio, '');
   Gerador.wCampoNFSe(tcStr, '', 'CEP',                           01, 08,  1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
   Gerador.wCampoNFSe(tcStr, '', 'Estado',                        01, 08,  1, NFSe.Tomador.Endereco.UF, '');
   Gerador.wCampoNFSe(tcStr, '', 'Pais',                          01, 08,  1, NFSe.Tomador.Endereco.xPais, '');
   Gerador.wGrupoNFSe('/Endereco');
   if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then begin
     Gerador.wCampoNFSe(tcStr, '', 'DDD', 00, 03, 0, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),3), '');
   end
   else
   if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then begin
     Gerador.wCampoNFSe(tcStr, '', 'DDD',                         00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),2), '');
   end
   else
     Gerador.wCampoNFSe(tcStr, '', 'DDD',                         00, 03, 1, '', '');
   Gerador.wCampoNFSe(tcStr, '', 'Telefone',                      00, 08, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),8), '');
   Gerador.Prefixo := 'rgm1:';
   Gerador.wGrupoNFSe('/Tomador');
   if (Trim(NFSe.Tomador.Endereco.xPais) <> '') and (NFSe.Tomador.Endereco.xPais <> 'BRASIL') then
     Gerador.wCampoNFSe(tcStr, '', 'TomadorEstrangeiro',          05, 05, 1, 'true', '')
   else
     Gerador.wCampoNFSe(tcStr, '', 'TomadorEstrangeiro',          05, 05, 1, 'false', '');
   Gerador.wCampoNFSe(tcDe2, '', 'Valor',                         01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorDeducao',                  01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorIR',                       01, 15, 0, NFSe.Servico.Valores.ValorIr, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorINSS',                     01, 15, 0, NFSe.Servico.Valores.ValorInss, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS',                   01, 15, 0, NFSe.Servico.Valores.ValorCofins, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorPisPasep',                 01, 15, 0, NFSe.Servico.Valores.ValorPis, '');
   Gerador.wCampoNFSe(tcDe2, '', 'ValorCSLL',                     01, 15, 0, NFSe.Servico.Valores.ValorCsll, '');

  //Não encontrei um campo para esta tag. Como não é obrigatória, fica aqui apenas citada caso necessite no futuro
  //   Gerador.wCampoNFSe(tcDe2, '', 'ValorOutrosImpostos',           01, 15, 0, , '');

  Gerador.Prefixo := 'rgm:';
  Gerador.wGrupoNFSe('/NotaFiscal');
end;

procedure TNFSeW.GerarXML_Provedor_Equiplano;
var
  sTpDoc: string;
  iAux, iSerItem, iSerSubItem: Integer;
begin
  if (Trim(NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro) <> '') then
    sTpDoc:= '3'  //Est
  else if (Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj))=14) then
    sTpDoc:= '2'  //CNPJ
  else
    sTpDoc:= '1'; //CPF

  iAux:= StrToInt(OnlyNumber(NFSe.Servico.ItemListaServico)); //Ex.: 1402, 901
  if ( iAux > 999) then //Ex.: 1402
    begin
      iSerItem   := StrToInt( Copy( IntToStr(iAux), 1, 2) ); //14
      iSerSubItem:= StrToInt( Copy( IntToStr(iAux), 3, 2) ); //2
    end
  else //Ex.: 901
    begin
      iSerItem   := StrToInt( Copy( IntToStr(iAux), 1, 1) ); //9
      iSerSubItem:= StrToInt( Copy( IntToStr(iAux), 2, 2) ); //1
    end;
    
  Gerador.wGrupoNFSe('rps');
    Gerador.wCampoNFSe(tcInt,   '', 'nrRps       ', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
    Gerador.wCampoNFSe(tcStr,   '', 'nrEmissorRps', 01, 01, 1, NFSe.IdentificacaoRps.Serie, '');
    Gerador.wCampoNFSe(tcDatHor,'', 'dtEmissaoRps', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
    Gerador.wCampoNFSe(tcStr,   '', 'stRps       ', 01, 01, 1, '1', '');
    Gerador.wCampoNFSe(tcStr,   '', 'tpTributacao', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
    Gerador.wCampoNFSe(tcStr,   '', 'isIssRetido ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
    Gerador.wGrupoNFSe('tomador');
      Gerador.wGrupoNFSe('documento');
        Gerador.wCampoNFSe(tcStr, '', 'nrDocumento           ', 01, 14,  1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
        Gerador.wCampoNFSe(tcStr, '', 'tpDocumento           ', 01, 01,  1, sTpDoc, '');
        Gerador.wCampoNFSe(tcStr, '', 'dsDocumentoEstrangeiro', 00, 20,  1, NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro, '');
      Gerador.wGrupoNFSe('/documento');
      Gerador.wCampoNFSe(tcStr, '', 'nmTomador          ', 01, 080, 1, NFSe.Tomador.RazaoSocial, '');
      Gerador.wCampoNFSe(tcStr, '', 'dsEmail            ', 00, 080, 1, NFSe.Tomador.Contato.Email, '');
      Gerador.wCampoNFSe(tcStr, '', 'nrInscricaoEstadual', 00, 020, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');
      Gerador.wCampoNFSe(tcStr, '', 'dsEndereco         ', 00, 040, 1, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wCampoNFSe(tcStr, '', 'nrEndereco         ', 00, 010, 1, NFSe.Tomador.Endereco.Numero, '');
      Gerador.wCampoNFSe(tcStr, '', 'dsComplemento      ', 00, 060, 1, NFSe.Tomador.Endereco.Complemento, '');
      Gerador.wCampoNFSe(tcStr, '', 'nmBairro           ', 00, 025, 1, NFSe.Tomador.Endereco.Bairro, '');
      Gerador.wCampoNFSe(tcStr, '', 'nrCidadeIbge       ', 00, 007, 1, NFSe.Tomador.Endereco.CodigoMunicipio, '');
      Gerador.wCampoNFSe(tcStr, '', 'nmUf               ', 00, 002, 1, NFSe.Tomador.Endereco.UF, '');
      Gerador.wCampoNFSe(tcStr, '', 'nmPais             ', 01, 040, 1, NFSe.Tomador.Endereco.xPais, '');
      Gerador.wCampoNFSe(tcStr, '', 'nrCep              ', 00, 015, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
      Gerador.wCampoNFSe(tcStr, '', 'nrTelefone         ', 00, 020, 1, NFSe.Tomador.Contato.Telefone, '');
    Gerador.wGrupoNFSe('/tomador');
    Gerador.wGrupoNFSe('listaServicos');
      Gerador.wGrupoNFSe('servico');
        Gerador.wCampoNFSe(tcStr, '', 'nrServicoItem   ', 01, 02, 1, iSerItem, '');
        Gerador.wCampoNFSe(tcStr, '', 'nrServicoSubItem', 01, 02, 1, iSerSubItem, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vlServico       ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vlAliquota      ', 01, 02, 1, NFSe.Servico.Valores.Aliquota, '');
        if (NFSe.Servico.Valores.ValorDeducoes > 0) then
          begin
            Gerador.wGrupoNFSe('deducao');
              Gerador.wCampoNFSe(tcDe2, '', 'vlDeducao             ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, '');
              Gerador.wCampoNFSe(tcStr, '', 'dsJustificativaDeducao', 01,255, 1, NFSe.Servico.Valores.JustificativaDeducao, '');
            Gerador.wGrupoNFSe('/deducao');
          end;
        Gerador.wCampoNFSe(tcDe2, '', 'vlBaseCalculo         ', 01,  15, 1, NFSe.Servico.Valores.BaseCalculo, '');
        Gerador.wCampoNFSe(tcDe2, '', 'vlIssServico          ', 01,  15, 1, NFSe.Servico.Valores.ValorIss, '');
        Gerador.wCampoNFSe(tcStr, '', 'dsDiscriminacaoServico', 01,1024, 1, NFSe.Servico.Discriminacao, '');
      Gerador.wGrupoNFSe('/servico');
    Gerador.wGrupoNFSe('/listaServicos');
    Gerador.wCampoNFSe(tcDe2, '', 'vlTotalRps  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
    Gerador.wCampoNFSe(tcDe2, '', 'vlLiquidoRps', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');
    Gerador.wGrupoNFSe('retencoes');
      Gerador.wCampoNFSe(tcDe2, '', 'vlCofins        ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlCsll          ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlInss          ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlIrrf          ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlPis           ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlIss           ', 01, 15, 1, NFSe.Servico.Valores.ValorIssRetido, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlAliquotaCofins', 01, 02, 1, NFSe.Servico.Valores.AliquotaCofins, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlAliquotaCsll  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaCsll, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlAliquotaInss  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaInss, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlAliquotaIrrf  ', 01, 02, 1, NFSe.Servico.Valores.AliquotaIr, '');
      Gerador.wCampoNFSe(tcDe2, '', 'vlAliquotaPis   ', 01, 02, 1, NFSe.Servico.Valores.AliquotaPis, '');
    Gerador.wGrupoNFSe('/retencoes');  
  Gerador.wGrupoNFSe('/rps');
end;

procedure TNFSeW.GerarCondicaoPagamento;
var
  i: Integer;
begin
  if (NFSe.CondicaoPagamento.QtdParcela > 0) then 
    begin
      Gerador.wGrupoNFSe('CondicaoPagamento');
        Gerador.wCampoNFSe(tcStr, '#53', 'Condicao  ', 01, 15, 1, CondicaoToStr(NFSe.CondicaoPagamento.Condicao), '');
        Gerador.wCampoNFSe(tcInt, '#54', 'QtdParcela', 01, 3, 1, NFSe.CondicaoPagamento.QtdParcela, '');
        for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
          begin
            Gerador.wGrupoNFSe('Parcelas');
              Gerador.wCampoNFSe(tcInt, '#55', 'Parcela', 01, 03, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, '');
              Gerador.wCampoNFSe(tcDatVcto, '#55', 'DataVencimento', 19, 19, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, DSC_DVENC);
              Gerador.wCampoNFSe(tcDe2, '#55', 'Valor', 01, 18, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, '');
            Gerador.wGrupoNFSe('/Parcelas');
          end;
      Gerador.wGrupoNFSe('/CondicaoPagamento');
    end;
end;


procedure TNFSeW.GerarXML_Provedor_NFSEBrasil;
begin
  Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="' + NFSe.InfID.ID + '"');

  GerarIdentificacaoRPS;

  // Gerador.wCampoNFSe(tcDat, '#3','DataEmissao', 19, 19, 1, NFSe.DataEmissao + 'T10:00:00', DSC_DEMI);
  DateSeparator := '-';
  ShortDateFormat := 'yyyy-mm-dd';

  Gerador.wCampoNFSe(tcStr, '#3','DataEmissao', 19, 19, 1, DateTimeToStr(NFSe.DataEmissao)+ 'T10:00:00', DSC_DEMI);

  DateSeparator := '/';
  ShortDateFormat := 'dd/mm/yyyy';

  Gerador.wCampoNFSe(tcStr, '#4','NaturezaOperacao     ', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), '');
  Gerador.wCampoNFSe(tcStr, '#5','Status     ', 01, 01, 1, StatusRPSToStr(NFSe.Status), '');

  // GerarServico;
  GerarServicoValores_V2;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;

  Gerador.wGrupoNFSe('/Rps');
end;

procedure TNFSeW.GerarXML_Provedor_EL;
var
  LocPrest: String;
begin
  FIdentificador := 'Id';
  Gerador.wCampoNFSe(tcStr   , '#01', FIdentificador  , 001, 015, 1, NFSe.InfID.ID, '');

  LocPrest := '2';
  if NFSe.NaturezaOperacao = noTributacaoForaMunicipio then
  LocPrest := '1';

  Gerador.wCampoNFSe(tcStr   , '#02', 'LocalPrestacao', 001, 001, 1, LocPrest, ''); //Código para identificação do local de prestação do serviço 1-Fora do município 2-No município
  Gerador.wCampoNFSe(tcStr   , '#03', 'IssRetido'     , 001, 001, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), '');
  Gerador.wCampoNFSe(tcDatHor, '#04', 'DataEmissao'   , 019, 019, 1, NFSe.DataEmissao, DSC_DEMI);

  GerarIdentificacaoRPS;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarListaServicos2;
  GerarRPSSubstituido;

  Gerador.wCampoNFSe(tcStr   , '#90', 'Observacao'    , 001, 255, 0, NFSe.OutrasInformacoes, '');
  Gerador.wCampoNFSe(tcStr   , '#91', 'Status'        , 001, 001, 1, StatusRPSToStr(NFSe.Status), '');
end;

end.

