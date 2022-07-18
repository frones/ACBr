{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit pnfsNFSeW_ABRASFv1;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pnfsNFSeW, pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao, pnfsConsts, pcnConsts;

type
  { TNFSeW_ABRASFv1 }

  TNFSeW_ABRASFv1 = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;

    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;
    procedure GerarCondicaoPagamentoPublica;

    procedure GerarXML_ABRASF_V1;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout da versão 1.xx da ABRASF.                                             }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_ABRASFv1 }

procedure TNFSeW_ABRASFv1.GerarIdentificacaoRPS;
begin
  Gerador.wGrupo('IdentificacaoRps');
  Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampo(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  Gerador.wCampo(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS);
  Gerador.wGrupo('/IdentificacaoRps');
end;

procedure TNFSeW_ABRASFv1.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupo('RpsSubstituido');
    Gerador.wCampo(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampo(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampo(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupo('/RpsSubstituido');
  end;
end;

procedure TNFSeW_ABRASFv1.GerarPrestador;
begin
  Gerador.wGrupo('Prestador');

  case FProvedor of
    proActcon,
    proISSNet: begin
                 Gerador.wGrupo('CpfCnpj');
                 if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
                   Gerador.wCampo(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CPF)
                 else
                   Gerador.wCampo(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
                 Gerador.wGrupo('/CpfCnpj');
               end;
  else
    Gerador.wCampo(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
  end;

  Gerador.wCampo(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, DSC_IM);

  Gerador.wGrupo('/Prestador');
end;

procedure TNFSeW_ABRASFv1.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    Gerador.wGrupo('Tomador');

    if ((NFSe.Tomador.Endereco.UF <> 'EX') and
        ((NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
         (NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal <> '') or
         (NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual <> ''))) or
       ((FProvedor in [proSimplISS, proISSNet,proFISSLEX, proPronim, proLexsom])) then
    begin
      Gerador.wGrupo('IdentificacaoTomador');
      
      if NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
      begin
        Gerador.wGrupo('CpfCnpj');

        if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
          Gerador.wCampo(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CPF)
        else
          Gerador.wCampo(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CNPJ);

        Gerador.wGrupo('/CpfCnpj');
      end;

      Gerador.wCampo(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM);

      if FProvedor in [proBetha, proSimplISS, proFISSLEX] then
        Gerador.wCampo(tcStr, '#38', 'InscricaoEstadual', 01, 20, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, DSC_IE);

      Gerador.wGrupo('/IdentificacaoTomador');
    end;

    Gerador.wCampo(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, DSC_XNOME);

    if (NFSe.Tomador.Endereco.Endereco <> '') or (NFSe.Tomador.Endereco.Numero <> '') or
       (NFSe.Tomador.Endereco.Bairro <> '') or (NFSe.Tomador.Endereco.CodigoMunicipio <> '') or
       (NFSe.Tomador.Endereco.UF <> '') or (NFSe.Tomador.Endereco.CEP <> '') then
    begin
      Gerador.wGrupo('Endereco');
      Gerador.wCampo(tcStr, '#39', 'Endereco', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR);
      Gerador.wCampo(tcStr, '#40', 'Numero  ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, DSC_NRO);

      if FProvedor <> proNFSeBrasil then
        Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 060, 0, NFSe.Tomador.Endereco.Complemento, DSC_XCPL)
      else
        Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, DSC_XCPL);

      Gerador.wCampo(tcStr, '#42', 'Bairro', 001, 060, 0, NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO);

      if FProvedor in [proISSNet] then
      begin
        Gerador.wCampo(tcStr, '#43', 'Cidade', 007, 007, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);
        Gerador.wCampo(tcStr, '#44', 'Estado', 002, 002, 0, NFSe.Tomador.Endereco.UF, DSC_UF);
      end
      else begin
        Gerador.wCampo(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);
        Gerador.wCampo(tcStr, '#44', 'Uf             ', 2, 2, 0, NFSe.Tomador.Endereco.UF, DSC_UF);
      end;

      Gerador.wCampo(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP);
      Gerador.wGrupo('/Endereco');
    end;

    case FProvedor of
      proNFSeBrasil:
        begin
          Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
        end;
    else
      begin
        if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
        begin
          Gerador.wGrupo('Contato');
          Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
          Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 80, 0, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wGrupo('/Contato');
        end;
      end;
    end;

    Gerador.wGrupo('/Tomador');
  end
  else
    Gerador.wCampo(tcStr, '#', 'Tomador', 0, 1, 1, '', '');
end;

procedure TNFSeW_ABRASFv1.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial <> '') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupo('IntermediarioServico');
    Gerador.wCampo(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, DSC_XNOME);
    Gerador.wGrupo('CpfCnpj');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampo(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CPF)
    else
      Gerador.wCampo(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CNPJ);

    Gerador.wGrupo('/CpfCnpj');
    Gerador.wCampo(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, DSC_IM);
    Gerador.wGrupo('/IntermediarioServico');
  end;
end;

procedure TNFSeW_ABRASFv1.GerarServicoValores;
var
  i: Integer;
begin
  Gerador.wGrupo('Servico');
  Gerador.wGrupo('Valores');
  Gerador.wCampo(tcDe2, '#13', 'ValorServicos', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO);

  if FProvedor in [proRecife, proPronim, proISSNET, proNFSeBrasil, proGinfes, proThema, proDSFSJC] then
  begin
    Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);

    if FProvedor = proRecife then
      Gerador.wCampo(tcDe2, '#14', 'ValorTotalRecebido', 01, 15, 1, NFSe.Servico.Valores.ValorTotalRecebido, DSC_VTOTREC);

    Gerador.wCampo(tcDe2, '#15', 'ValorPis     ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
    Gerador.wCampo(tcDe2, '#16', 'ValorCofins  ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
    Gerador.wCampo(tcDe2, '#17', 'ValorInss    ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
    Gerador.wCampo(tcDe2, '#18', 'ValorIr      ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
    Gerador.wCampo(tcDe2, '#19', 'ValorCsll    ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
    Gerador.wCampo(tcStr, '#20', 'IssRetido    ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);
    Gerador.wCampo(tcDe2, '#21', 'ValorIss     ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VISS);
  end
  else begin
    Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes', 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);
    Gerador.wCampo(tcDe2, '#15', 'ValorPis     ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
    Gerador.wCampo(tcDe2, '#16', 'ValorCofins  ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
    Gerador.wCampo(tcDe2, '#17', 'ValorInss    ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
    Gerador.wCampo(tcDe2, '#18', 'ValorIr      ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, DSC_VIR);
    Gerador.wCampo(tcDe2, '#19', 'ValorCsll    ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
    Gerador.wCampo(tcStr, '#20', 'IssRetido    ', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);
    Gerador.wCampo(tcDe2, '#21', 'ValorIss     ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, DSC_VISS);
  end;

  if not (FProvedor in [proPronim, proBetha, proGovBr, proNFSeBrasil]) then
    Gerador.wCampo(tcDe2, '#22', 'ValorIssRetido', 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, DSC_VISSRET);

  if FProvedor in [proPronim, proNFSeBrasil] then
    Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES)
  else
    Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);

  if FProvedor <> proNFSeBrasil then
  begin
    if FProvedor in [proPronim, proNatal] then
      Gerador.wCampo(tcDe2, '#24', 'BaseCalculo', 01, 15, 1, NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS)
    else
      Gerador.wCampo(tcDe2, '#24', 'BaseCalculo', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS);
  end;

  case FProvedor of
    proThema:
      Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 1, (NFSe.Servico.Valores.Aliquota / 100), DSC_VALIQ);

    proRJ,
    proPublica,
    proBHISS,
    proAbaco:
      Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 0, (NFSe.Servico.Valores.Aliquota / 100), DSC_VALIQ);

    proGINFES:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 1, (NFSe.Servico.Valores.Aliquota / 100), DSC_VALIQ)
      else
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 0, (NFSe.Servico.Valores.Aliquota / 100), DSC_VALIQ);

    proGovBR,
    proPronim,
    proISSNet,
    proWebISS,
    proDSFSJC,
    proMetropolisWeb:
      Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proRecife:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ)
      else
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proNFSEBrasil:
      Gerador.wCampo(tcDe2, '#25', 'Aliquota', 1, 5, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);
  else
    Gerador.wCampo(tcDe4, '#25', 'Aliquota', 1, 5, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);
  end;

  if FProvedor <> proNFSEBrasil then
  begin
    Gerador.wCampo(tcDe2, '#26', 'ValorLiquidoNfse', 01, 15, 0, NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE);

    if (FProvedor in [proPronim, proBetha, proGovBr]) then
      Gerador.wCampo(tcDe2, '#22', 'ValorIssRetido', 01, 15, 0, NFSe.Servico.Valores.ValorIssRetido, DSC_VISSRET);
  end;

  if FProvedor in [proNFSeBrasil] then
  begin
    Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
    Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
  end
  else begin
    Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
    Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
  end;

  Gerador.wGrupo('/Valores');

  if FProvedor <> proNFSeBrasil then
  begin
    case FProvedor of
      proISSNet,
      proWebISS,
      proIssCuritiba,
      proAbaco,
      proRecife,
      proBetha: Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 1, OnlyNumber(NFSe.Servico.ItemListaServico), DSC_CLISTSERV);

      proSimplISS: Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 1, ifThen(copy(NFSe.Servico.ItemListaServico, 1, 1) = '0',
                                                                       copy(NFSe.Servico.ItemListaServico, 2, length(NFSe.Servico.ItemListaServico)),
                                                                       NFSe.Servico.ItemListaServico));
    else
      Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
    end;
  end;

  if FProvedor = proNFSeBrasil then
  begin
    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN);
    Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
  end;

  if not (FProvedor in [proPublica, proNFSeBrasil]) then
  begin
    Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN);
  end;

  Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                     StringReplace(FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ),
                      DSC_DISCR, (NFSe.PrestadorServico.Endereco.CodigoMunicipio <> '3304557'));

  if FProvedor in [proPublica] then
    Gerador.wCampo(tcStr, '', 'InformacoesComplementares', 001, 255, 0, NFSe.OutrasInformacoes, '');

  if FProvedor = proISSNet then
    Gerador.wCampo(tcStr, '#33', 'MunicipioPrestacaoServico', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN)
  else
    Gerador.wCampo(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);

  if FProvedor = proSimplISS  then
  begin
    for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
    begin
      Gerador.wGrupo('ItensServico');
      Gerador.wCampo(tcStr, '#33a', 'Descricao    ', 01, 100, 1, NFSe.Servico.ItemServico[i].Descricao, DSC_XSERV);
      Gerador.wCampo(tcDe2, '#33b', 'Quantidade   ', 01, 015, 1, NFSe.Servico.ItemServico[i].Quantidade, DSC_QTDE);
      Gerador.wCampo(tcDe4, '#33c', 'ValorUnitario', 01, 015, 1, NFSe.Servico.ItemServico[i].ValorUnitario, DSC_VUNIT);
      Gerador.wGrupo('/ItensServico');
    end;
    if NFSe.Servico.ItemServico.Count > 10 then
      Gerador.wAlerta('#33a', 'ItensServico', DSC_QTDEITENS, ERR_MSG_MAIOR_MAXIMO + '10');
  end;

  Gerador.wGrupo('/Servico');
end;

procedure TNFSeW_ABRASFv1.GerarListaServicos;
var
  i: Integer;
begin
  if FProvedor <> proSystemPro then
    Gerador.wGrupo('ListaServicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupo('Servico');
    Gerador.wGrupo('Valores');
    Gerador.wCampo(tcDe2, '#13', 'ValorServicos         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorServicos, DSC_VSERVICO);
    Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorDeducoes, DSC_VDEDUCISS);
    Gerador.wCampo(tcDe2, '#21', 'ValorIss              ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIss, DSC_VISS);
    Gerador.wCampo(tcDe2, '#25', 'Aliquota              ', 01, 05, 1, NFSe.Servico.ItemServico[i].Aliquota, DSC_VALIQ);
    Gerador.wCampo(tcDe2, '#24', 'BaseCalculo           ', 01, 15, 1, NFSe.Servico.ItemServico[i].BaseCalculo, DSC_VBCISS);
    Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoIncondicionado, DSC_VDESCINCOND);
    Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoCondicionado, DSC_VDESCCOND);

    if FProvedor=proSystemPro then
    begin
      Gerador.wCampo(tcDe2, '#15', 'ValorPis     ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorPis, DSC_VPIS);
      Gerador.wCampo(tcDe2, '#16', 'ValorCofins  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCofins, DSC_VCOFINS);
      Gerador.wCampo(tcDe2, '#17', 'ValorInss    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorInss, DSC_VINSS);
      Gerador.wCampo(tcDe2, '#18', 'ValorIr      ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIr, DSC_VIR);
      Gerador.wCampo(tcDe2, '#19', 'ValorCsll    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCsll, DSC_VCSLL);
    end;

    Gerador.wGrupo('/Valores');
    Gerador.wCampo(tcStr, '#20', 'IssRetido                ', 01, 01,   1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);
    Gerador.wCampo(tcStr, '#29', 'ItemListaServico         ', 01, 0005, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
    Gerador.wCampo(tcStr, '#30', 'CodigoCnae               ', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);
    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                    StringReplace( NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ),
                      DSC_DISCR, (NFSe.PrestadorServico.Endereco.CodigoMunicipio <> '3304557'));
    Gerador.wCampo(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);
    Gerador.wCampo(tcInt, '#34', 'CodigoPais               ', 04, 04,   0, NFSe.Servico.CodigoPais, DSC_CPAIS);
    Gerador.wCampo(tcStr, '#35', 'ExigibilidadeISS         ', 01, 01,   1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), DSC_INDISS);
    Gerador.wCampo(tcInt, '#36', 'MunicipioIncidencia      ', 07, 07,   0, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI);
    Gerador.wCampo(tcStr, '#37', 'NumeroProcesso           ', 01, 30,   0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);

    if (NFSe.Servico.Valores.IssRetido <> stNormal) and (FProvedor = proSystemPro) then
      Gerador.wCampo(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET);
    Gerador.wGrupo('/Servico');
  end;

  if FProvedor <> proSystemPro then
    Gerador.wGrupo('/ListaServicos');
end;

procedure TNFSeW_ABRASFv1.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Gerador.wGrupo('ConstrucaoCivil');
    Gerador.wCampo(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA);
    Gerador.wCampo(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, DSC_ART);
    Gerador.wGrupo('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_ABRASFv1.GerarCondicaoPagamentoPublica;
var
  i: Integer;
begin
  if (NFSe.CondicaoPagamento.QtdParcela > 0) then
  begin
    Gerador.wGrupo('CondicaoPagamento');
    for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
    begin
      Gerador.wGrupo('Parcelas');
      Gerador.wCampo(tcInt, '#53', 'Condicao', 01, 15, 1, CondicaoToStrPublica(NFSe.CondicaoPagamento.Parcelas.Items[i].Condicao), DSC_TPAG);
      Gerador.wCampo(tcInt, '#55', 'Parcela', 01, 03, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, DSC_NPARC);
      Gerador.wCampo(tcDe2, '#55', 'Valor', 01, 18, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, DSC_VPARC);
      Gerador.wCampo(tcStr, '#55', 'DataVencimento', 10, 10, 1, formatdatetime('yyyy-mm-dd', NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento), DSC_DVENC);
      Gerador.wGrupo('/Parcelas');
    end;
    Gerador.wGrupo('/CondicaoPagamento');
  end;
end;

procedure TNFSeW_ABRASFv1.GerarCondicaoPagamento;
var
  i: Integer;
begin
  Gerador.wGrupo('CondicaoPagamento');

  if (NFSe.CondicaoPagamento.QtdParcela > 0) then
  begin
    Gerador.wCampo(tcStr, '#53', 'Condicao  ', 01, 15, 1, CondicaoToStr(NFSe.CondicaoPagamento.Condicao), DSC_TPAG);
    Gerador.wCampo(tcInt, '#54', 'QtdParcela', 01, 3, 1, NFSe.CondicaoPagamento.QtdParcela, DSC_QPARC);
    for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
    begin
      Gerador.wGrupo('Parcelas');
      Gerador.wCampo(tcInt, '#55', 'Parcela', 01, 03, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, DSC_NPARC);
      Gerador.wCampo(tcDatVcto, '#55', 'DataVencimento', 10, 10, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, DSC_DVENC);
      Gerador.wCampo(tcDe2, '#55', 'Valor', 01, 18, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, DSC_VPARC);
      Gerador.wGrupo('/Parcelas');
    end;
  end
  else
    Gerador.wCampo(tcStr, '#53', 'Condicao', 01, 15, 1, 'A_VISTA', DSC_TPAG);

  Gerador.wGrupo('/CondicaoPagamento');
end;

procedure TNFSeW_ABRASFv1.GerarXML_ABRASF_V1;
begin
  if (FIdentificador = '') then
    Gerador.wGrupo('InfRps')
  else
  begin
    if FProvedor <>  proNFSeBrasil then
      Gerador.wGrupo('InfRps ' + FIdentificador + '="' + NFSe.InfID.ID + '"')
    else
      Gerador.wGrupo('InfRps ' + FIdentificador + '="' + NFSe.IdentificacaoRps.Numero + '"');
  end;

  GerarIdentificacaoRPS;

  Gerador.wCampo(tcDatHor, '#4', 'DataEmissao     ', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
  Gerador.wCampo(tcStr,    '#5', 'NaturezaOperacao', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP);

  if not (FProvedor in [proPublica, proDBSeller]) then
  begin
    if (NFSe.RegimeEspecialTributacao <> retNenhum) then
      Gerador.wCampo(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), DSC_REGISSQN);
  end;

  if FProvedor <>  proNFSeBrasil then
  begin
    Gerador.wCampo(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN);
    Gerador.wCampo(tcStr, '#8', 'IncentivadorCultural  ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCCULT);
  end;
  Gerador.wCampo(tcStr, '#9', 'Status', 01, 01, 1, StatusRPSToStr(NFSe.Status), DSC_INDSTATUS);

  if FProvedor in [proBetha, proFISSLex, proSimplISS] then
    Gerador.wCampo(tcStr, '#11', 'OutrasInformacoes', 001, 255, 0, NFSe.OutrasInformacoes, DSC_OUTRASINF);

  GerarRPSSubstituido;

  GerarServicoValores;
  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;

  if (FProvedor = proBetha) then
    GerarCondicaoPagamento;

  if (FProvedor = proPublica) then
    GerarCondicaoPagamentoPublica;

  Gerador.wGrupo('/InfRps');
end;

constructor TNFSeW_ABRASFv1.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_ABRASFv1.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_ABRASFv1.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML  := '';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  if (FProvedor in [proBHISS, proNatal, proProdemge, proPronim, proTinus,
                    proNFSEBrasil]) then
    FDefTipos := FServicoEnviar;

  if (FProvedor in [proWebISS]) then
    FDefTipos := '';

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  if (FProvedor = proISSDigital) and (NFSe.NumeroLote <> '')
    then Atributo := ' Id="' +  (NFSe.IdentificacaoRps.Numero) + '"';

  Gerador.Prefixo := '';

  if (FProvedor in [proBetha, proNFSeBrasil]) then
    Gerador.wGrupo('Rps')
  else
    Gerador.wGrupo('Rps' + Atributo);

  case FProvedor of
//    proAbaco,
    proRecife,
    proSalvador,
    proSmarAPDv1: FNFSe.InfID.ID := 'rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);
  else
    FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;
  end;

  Gerador.Prefixo := FPrefixo4;

  GerarXML_ABRASF_V1;

  Gerador.Prefixo := '';

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
