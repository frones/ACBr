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

unit pnfsNFSeW_ABRASFv2;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  synacode, ACBrConsts,
  pcnAuxiliar, pcnConsts, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsNFSeW, pnfsConversao, pnfsConsts;

type
  { TNFSeW_ABRASFv2 }

  TNFSeW_ABRASFv2 = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarCredenciais;
    procedure GerarRPSSubstituido;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarConstrucaoCivil;
    procedure GerarCondicaoPagamento;

    procedure GerarXML_ABRASF_v2;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: string; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout da versão 2.xx da ABRASF.                                             }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_ABRASFv2 }

procedure TNFSeW_ABRASFv2.GerarIdentificacaoRPS;
begin
  Gerador.wGrupoNFSe('IdentificacaoRps');
  Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  if FProvedor <> ProSigep then
    Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  if FProvedor <> ProSigep then
    Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS)
  else
    Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 02, 1, 'R1', DSC_TIPORPS);
  Gerador.wGrupoNFSe('/IdentificacaoRps');
end;

procedure TNFSeW_ABRASFv2.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupoNFSe('RpsSubstituido');
    Gerador.wCampoNFSe(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampoNFSe(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampoNFSe(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupoNFSe('/RpsSubstituido');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarPrestador;
begin
  Gerador.wGrupoNFSe('Prestador');

  Gerador.wGrupoNFSe('CpfCnpj');
  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CPF)
  else
    Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
  Gerador.wGrupoNFSe('/CpfCnpj');

  if (FProvedor = proTecnos) then
    Gerador.wCampoNFSe(tcStr, '#35', 'RazaoSocial', 01, 15, 1, NFSe.PrestadorServico.RazaoSocial, DSC_XNOME);

  Gerador.wCampoNFSe(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, DSC_IM);

  if (FProvedor in [proISSDigital, proAgili]) then
  begin
    Gerador.wCampoNFSe(tcStr, '#36', 'Senha       ', 01, 255, 1, NFSe.Prestador.Senha, DSC_SENHA);
    Gerador.wCampoNFSe(tcStr, '#37', 'FraseSecreta', 01, 255, 1, NFSe.Prestador.FraseSecreta, DSC_FRASESECRETA);
  end;

  Gerador.wGrupoNFSe('/Prestador');
end;

procedure TNFSeW_ABRASFv2.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
  begin
    if (FProvedor in [proActcon, proELv2, proVersaTecnologia, proISSJoinville,
        proSmarAPDABRASF]) or
       ((FProvedor in [proActconv201, proActconv2]) and (FVersaoDados = '2.01')) then
      Gerador.wGrupoNFSe('TomadorServico')
    else
      Gerador.wGrupoNFSe('Tomador');

    if NFSe.Tomador.Endereco.UF <> 'EX' then
    begin
      Gerador.wGrupoNFSe('IdentificacaoTomador');
      if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') then
      begin
        Gerador.wGrupoNFSe('CpfCnpj');
        if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
          Gerador.wCampoNFSe(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CPF)
        else
          Gerador.wCampoNFSe(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CNPJ);
        Gerador.wGrupoNFSe('/CpfCnpj');
      end;

      Gerador.wCampoNFSe(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM);

      Gerador.wGrupoNFSe('/IdentificacaoTomador');
    end;

    Gerador.wCampoNFSe(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, DSC_XNOME);

    Gerador.wGrupoNFSe('Endereco');
    if FProvedor = proSigep then
      Gerador.wCampoNFSe(tcStr, '#39', 'Logradouro', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR)
    else
      Gerador.wCampoNFSe(tcStr, '#39', 'Endereco', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR);
    Gerador.wCampoNFSe(tcStr, '#40', 'Numero  ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, DSC_NRO);

    if FProvedor <> proNFSeBrasil then
      Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 0, NFSe.Tomador.Endereco.Complemento, DSC_XCPL)
    else
      Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, DSC_XCPL);

    Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 060, 0, NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO);

    Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);
    Gerador.wCampoNFSe(tcStr, '#44', 'Uf             ', 2, 2, 0, NFSe.Tomador.Endereco.UF, DSC_UF);

    if not (FProvedor in [proELv2, proNFSeBrasil, proPronimv2, proISSJoinville, proSmarAPDABRASF]) or
       ((FProvedor = proPronimv2) and (OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio) = '9999999')) then
      Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais ', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, DSC_CPAIS);

    if FProvedor = proELv2 then
      Gerador.wCampoNFSe(tcStr, '#45', 'Cep', 008, 008, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP)
    else
      Gerador.wCampoNFSe(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP);

    Gerador.wGrupoNFSe('/Endereco');

    case FProvedor of
      proNFSeBrasil:
        begin
          Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
        end;
    else
      begin
        if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
        begin
          Gerador.wGrupoNFSe('Contato');
          Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
          if FProvedor = proSigep then
          begin
            Gerador.wCampoNFSe(tcStr, '#048', 'Ddd', 01, 03, 0, NFSe.Tomador.Contato.DDD, DSC_DDD);
            Gerador.wCampoNFSe(tcStr, '#049', 'TipoTelefone', 01, 02, 0, NFSe.Tomador.Contato.TipoTelefone, DSC_TPTELEFONE);
          end;
          Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 80, 0, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wGrupoNFSe('/Contato');
        end;
      end;
    end;

    if (FProvedor in [proActcon, proELv2, proVersaTecnologia, proISSJoinville,
        proSmarAPDABRASF]) or
        ((FProvedor in [proActconv201, proActconv2]) and (FVersaoDados = '2.01')) then
      Gerador.wGrupoNFSe('/TomadorServico')
    else
      Gerador.wGrupoNFSe('/Tomador');
  end
  else
  begin
    // Gera a TAG vazia quando nenhum dado do tomador for informado.
    if FProvedor in [proActcon, proVersaTecnologia, proSmarAPDABRASF] then
      Gerador.wCampoNFSe(tcStr, '#', 'TomadorServico', 0, 1, 1, '', '')
    else
      Gerador.wCampoNFSe(tcStr, '#', 'Tomador', 0, 1, 1, '', '');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial <> '') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupoNFSe('Intermediario');
    Gerador.wGrupoNFSe('IdentificacaoIntermediario');
    Gerador.wGrupoNFSe('CpfCnpj');

    if FProvedor = proVirtual then
    begin
      if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      begin
        Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CPF);
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, '', '');
      end
      else
      begin
        Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, '', '');
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CNPJ);
      end;
    end
    else
    begin
      if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
        Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CPF)
      else
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CNPJ);
    end;

    Gerador.wGrupoNFSe('/CpfCnpj');

    if FProvedor = proVirtual then
      Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 1, NFSe.IntermediarioServico.InscricaoMunicipal, DSC_IM)
    else
      Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, DSC_IM);

    Gerador.wGrupoNFSe('/IdentificacaoIntermediario');

    if FProvedor in [proVirtual, proVersaTecnologia] then
      Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 1, NFSe.IntermediarioServico.RazaoSocial, DSC_XNOME)
    else
      Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, DSC_XNOME);

    Gerador.wGrupoNFSe('/Intermediario');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarServicoValores;
begin
  Gerador.wGrupoNFSe('Servico');

  if FProvedor in [proTecnos] then
    Gerador.wGrupoNFSe('tcDadosServico');

  Gerador.wGrupoNFSe('Valores');
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO);

  case FProvedor of
    profintelISS:
      begin
        Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);
        Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss       ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VISS);
      end;

    proABase,
    proActcon,
    proPronimv2,
    proVirtual,
    proVersaTecnologia,
    proCoplan:
      begin
        Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);
        Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
        Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
        Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
        Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
        Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
        Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
      end;
  else
    begin
      Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes', 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);

      case FProvedor of
        proISSe,
        proNEAInformatica,
        proProdata,
        proSystemPro,
        proVitoria,
        proTecnos:
          begin
            Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
            Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
            Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
            Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
            Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
            Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
          end;
      else
        begin
          Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis       ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
          Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins    ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
          Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss      ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
          Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr        ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, DSC_VIR);
          Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll      ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
          Gerador.wCampoNFSe(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
        end;
      end;

      if not (FProvedor in [proProdata, proGoiania]) then
      begin
        if FProvedor in [pro4R, proISSDigital, proISSe, proSystemPro,
          proFiorilli, proSaatri, proCoplan, proLink3,
          proTecnos, proNEAInformatica, proSH3] then
          Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VINSS)
        else
          Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, DSC_VINSS);
      end;
    end;
  end;

  if FProvedor in [proActcon, proAgili, proTecnos] then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS);

  if FProvedor in [proABase, proActcon, proPronimv2, proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VINSS)
  else if FProvedor in [proCoplan] then
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, DSC_VINSS);

  case FProvedor of
    proActconv2,
    proCoplan,
    proDigifred,
    proELv2,
    proFriburgo,
    proNEAInformatica,
    proNotaInteligente,
    proPronimv2,
    proSisPMJP,
    proVitoria,
    proSmarAPDABRASF,
    proISSJoinville: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proABase,
    proEReceita,
    proProdata,
    proSafeWeb,
    proTecnos: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    pro4R,
    profintelISS,
    proISSDigital,
    proISSe,
    proLink3,
    proSaatri,
    proSystemPro,
    proVirtual,
    proVersaTecnologia,
    proSH3: Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proGoiania:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

  else
    Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);
  end;

  if FProvedor in [profintelISS] then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 1, NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS);

  case FProvedor of
    proABase,
    proActcon,
    proPronimv2,
    proTecnos,
    proVirtual,
    proCoplan,
    proVersaTecnologia:
      begin
        Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
        Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      end;

    pro4R,
    proFiorilli,
    proGoiania,
    proISSDigital,
    proISSe,
    proSystemPro,
    proPVH,
    proSaatri,
    proLink3,
    proNEAInformatica,
    proNotaInteligente,
    proVitoria,
    proSH3,
    proSIAPNet:
      begin
        Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
        Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      end;

    proProdata: Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);

  else
    begin
      Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
    end;
  end;

  Gerador.wGrupoNFSe('/Valores');

  if not (FProvedor in [proGoiania, proSigep]) then
    Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);

  if ((NFSe.Servico.Valores.IssRetido <> stNormal) and not (FProvedor in [proGoiania, proSigep])) or
     (FProvedor in [proProdata, proVirtual, proVersaTecnologia]) then
    Gerador.wCampoNFSe(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET);

  if FProvedor <> proGoiania then
  begin
    case FProvedor of
      proSisPMJP,
      proVirtual: Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, OnlyNumber(NFSe.Servico.ItemListaServico), DSC_CLISTSERV);

      proNotaInteligente: Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);

      proVitoria:
        begin
          if copy(NFSe.Servico.ItemListaServico, 1, 1) = '0' then
            Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, copy(NFSe.Servico.ItemListaServico, 2, 4), DSC_CLISTSERV)
          else
            Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
        end;
    else
      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
    end;

    if FProvedor = proVersaTecnologia then
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE)
    else
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 07, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
  end;

  if FProvedor in [proGoiania, proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN)
  else if FProvedor = proSIAPNet then
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN)
  else
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);

  Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
    StringReplace(FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);

  Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);

  if FProvedor in [proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 1, NFSe.Servico.CodigoPais, DSC_CPAIS)
  else
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 0, NFSe.Servico.CodigoPais, DSC_CPAIS);

  if FProvedor <> proGoiania then
  begin
    if FProvedor = proSigep then
      Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ('0' + ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS)), DSC_INDISS)
    else
      Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), DSC_INDISS);

    if not (FProvedor in [proProdata, proVirtual, proSigep]) then
      Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 0, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI)
    else
      if FProvedor <> proSigep then
        Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 1, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI);
  end;

  Gerador.wCampoNFSe(tcStr, '#37', 'NumeroProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);

  if FProvedor in [proTecnos] then
    Gerador.wGrupoNFSe('/tcDadosServico');

  Gerador.wGrupoNFSe('/Servico');
end;

procedure TNFSeW_ABRASFv2.GerarListaServicos;
var
  i: Integer;
begin
  if FProvedor <> proSystemPro then
    Gerador.wGrupoNFSe('ListaServicos');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupoNFSe('Servico');
    Gerador.wGrupoNFSe('Valores');
    Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorServicos, DSC_VSERVICO);
    Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDeducoes         ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorDeducoes, DSC_VDEDUCISS);
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss              ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIss, DSC_VISS);
    Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota              ', 01, 05, 1, NFSe.Servico.ItemServico[i].Aliquota, DSC_VALIQ);
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo           ', 01, 15, 1, NFSe.Servico.ItemServico[i].BaseCalculo, DSC_VBCISS);
    Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoIncondicionado, DSC_VDESCINCOND);
    Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoCondicionado, DSC_VDESCCOND);

    if FProvedor = proSystemPro then
    begin
      Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis   ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorPis, DSC_VPIS);
      Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCofins, DSC_VCOFINS);
      Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorInss, DSC_VINSS);
      Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIr, DSC_VIR);
      Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCsll, DSC_VCSLL);
    end;

    Gerador.wGrupoNFSe('/Valores');

    if FProvedor <> proSigep then
      Gerador.wCampoNFSe(tcStr, '#20', 'IssRetido', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);

    Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 0005, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);

    if FProvedor = proVersaTecnologia then
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE)
    else
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);

    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);
    Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
      StringReplace(NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);
    Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais               ', 04, 04, 0, NFSe.Servico.CodigoPais, DSC_CPAIS);
    Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS         ', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), DSC_INDISS);
    Gerador.wCampoNFSe(tcInt, '#36', 'MunicipioIncidencia      ', 07, 07, 0, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI);
    Gerador.wCampoNFSe(tcStr, '#37', 'NumeroProcesso           ', 01, 30, 0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);

    if (NFSe.Servico.Valores.IssRetido <> stNormal) and (FProvedor = proSystemPro) then
      Gerador.wCampoNFSe(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET);
    Gerador.wGrupoNFSe('/Servico');
  end;

  if FProvedor <> proSystemPro then
    Gerador.wGrupoNFSe('/ListaServicos');
end;

procedure TNFSeW_ABRASFv2.GerarValoresServico;
begin
  Gerador.wGrupoNFSe('ValoresServico');
  Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis        ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
  Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins     ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
  Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss       ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
  Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr         ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, DSC_VIR);
  Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll       ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
  Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss        ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VISS);
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorLiquidoNfse', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE);
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos   ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO);
  Gerador.wGrupoNFSe('/ValoresServico');
end;

procedure TNFSeW_ABRASFv2.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Gerador.wGrupoNFSe('ConstrucaoCivil');
    Gerador.wCampoNFSe(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA);
    Gerador.wCampoNFSe(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, DSC_ART);
    Gerador.wGrupoNFSe('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarCredenciais;
begin
  Gerador.wGrupoNFSe('credenciais');
  Gerador.wCampoNFSe(tcStr, '#01', 'usuario     ', 01, 15, 1, NFSe.Autenticador, DSC_USUARIO);
  Gerador.wCampoNFSe(tcStr, '#02', 'senha       ', 01, 05, 1, NFSe.Prestador.Senha, DSC_SENHA);
  Gerador.wCampoNFSe(tcStr, '#03', 'chavePrivada', 01, 01, 1, NFSe.Assinatura, DSC_ASSINATURA);
  Gerador.wGrupoNFSe('/credenciais');
  Gerador.wGrupoNFSe('Rps');
end;

procedure TNFSeW_ABRASFv2.GerarCondicaoPagamento;
//var
//  i: Integer;
begin
(*
  if (NFSe.CondicaoPagamento.QtdParcela > 0) then
  begin
    Gerador.wGrupoNFSe('CondicaoPagamento');
    Gerador.wCampoNFSe(tcStr, '#53', 'Condicao  ', 01, 15, 1, CondicaoToStr(NFSe.CondicaoPagamento.Condicao), DSC_TPAG);
    Gerador.wCampoNFSe(tcInt, '#54', 'QtdParcela', 01, 3, 1, NFSe.CondicaoPagamento.QtdParcela, DSC_QPARC);
    for i := 0 to NFSe.CondicaoPagamento.Parcelas.Count - 1 do
    begin
      Gerador.wGrupoNFSe('Parcelas');
      Gerador.wCampoNFSe(tcInt, '#55', 'Parcela', 01, 03, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Parcela, DSC_NPARC);
      Gerador.wCampoNFSe(tcDatVcto, '#55', 'DataVencimento', 10, 10, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].DataVencimento, DSC_DVENC);
      Gerador.wCampoNFSe(tcDe2, '#55', 'Valor', 01, 18, 1, NFSe.CondicaoPagamento.Parcelas.Items[i].Valor, DSC_VPARC);
      Gerador.wGrupoNFSe('/Parcelas');
    end;
    Gerador.wGrupoNFSe('/CondicaoPagamento');
  end;
*)
end;

procedure TNFSeW_ABRASFv2.GerarXML_ABRASF_v2;
begin
  case FProvedor of
    proSigep: GerarCredenciais;
  end;

  case FProvedor of
    proABase, proDigifred,proBethav2,  proEReceita, proFiorilli, proGovDigital,
    proISSe, proMitra, proNEAInformatica, proNotaInteligente, proPVH, proSisPMJP,
    proCoplan, proSIAPNet, proSystemPro, proISSJoinville:
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');

    proISSDigital:
        // alterado em 23/03/2018 para ver se funciona com a cidade de Cabo Frio
        // alterado em 09/05/2018 por italo (incluido novamente o namespace)
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');

    proSiam:
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="Declaracao_' + OnlyNumber(NFSe.Prestador.Cnpj) + '"');

    proTecnos:
      begin
        Gerador.WGrupoNFSe('tcDeclaracaoPrestacaoServico');
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');
      end;

    proVirtual:
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '=""');

    proTiplanv2:
        Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + 'xmlns="http://www.abrasf.org.br/nfse.xsd" ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
  else
    Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');
  end;

  // Não escrever os dados do RPS se não houver um tipo definido
  if NFSe.IdentificacaoRps.Tipo <> trNone then
  begin
    case FProvedor of
      proABase, proDigifred,proBethav2,  proEReceita, proFiorilli, proGovDigital,
      proISSe, proMitra, proNEAInformatica, proNotaInteligente, proPVH, proSisPMJP,
      proCoplan, proSIAPNet, proSystemPro, proPronimv2, proTecnos, proTiplanv2:
          Gerador.wGrupoNFSe('Rps');

      proISSDigital:
          Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="' +
                                      OnlyNumber(NFSe.NumeroLote) +
                                      OnlyNumber(FNFSe.IdentificacaoRps.Numero) + '"');

      proSiam:
          Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="' + NFSe.InfID.ID + '"');

      proVirtual:
          Gerador.wGrupoNFSe('Rps ' + FIdentificador + '=""');

    else
      begin
        if FIdentificador = '' then
          Gerador.wGrupoNFSe('Rps')
        else
          Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');
      end;
    end;

    GerarIdentificacaoRPS;

    case FProvedor of
      proABase, proActcon, proActconv201, proActconv2, proAgili, proBethav2,
      proCoplan, proEReceita, proFiorilli, proFriburgo, proGovDigital,
      proISSDigital, proISSe, proMitra, proNEAInformatica, proNotaInteligente,
      proProdata, proPronimv2, proPVH, proSaatri, proSisPMJP, proSiam, proVirtual,
      proVersaTecnologia, proVitoria, proWebISSv2, proActconv202, proSIAPNet,
      proBelford, proSystemPro, proSH3, proISSJoinville, proSmarAPDABRASF,
      proElv2, proAsten,
      proTiplanv2: Gerador.wCampoNFSe(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

    else
      Gerador.wCampoNFSe(tcDatHor, '#4', 'DataEmissao', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
    end;

    if FProvedor = ProSigep then
      Gerador.wCampoNFSe(tcStr, '#9', 'Status', 01, 01, 1, 'CO', DSC_INDSTATUS)
    else
      Gerador.wCampoNFSe(tcStr, '#9', 'Status', 01, 01, 1, StatusRPSToStr(NFSe.Status), DSC_INDSTATUS);

    GerarRPSSubstituido;

    Gerador.wGrupoNFSe('/Rps');
  end;

  if FProvedor in [profintelISS, proSystemPro] then
  begin
    GerarListaServicos;

    if NFSe.Competencia <> '' then
      Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 10, 19, 1, NFSe.Competencia, DSC_DEMI)
    else
      Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
  end
  else
  begin
    if NFSe.Competencia <> '' then
    begin
      case FProvedor of
        proActcon, proISSDigital, proMitra, proPVH, proSisPMJP, proVirtual,
        proSystemPro, proNEAInformatica,
        proEReceita: Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proABase, proBethav2, proFriburgo, proGovDigital, proNotaInteligente,
        proPronimv2, proVersaTecnologia, proWebISSv2, proActconv202, proBelford,
        proSH3, proSIAPNet, proISSJoinville, proSmarAPDABRASF, proELv2, proAsten,
        proTiplanv2: Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proTecnos: Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI);

      else
        Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 19, 19, 1, NFSe.Competencia, DSC_DEMI);
      end;
    end
    else
    begin
      if FProvedor in [proABase, proActcon, proBethav2, proCoplan, proEReceita,
         proFiorilli, proFriburgo, proGovDigital, proISSDigital, proISSe, proMitra,
         proNEAInformatica, proNotaInteligente, proPronimv2, proProdata, proPVH,
         proSaatri, proSiam, proSisPMJP, proSystemPro, proVirtual, proVitoria,
         proVersaTecnologia, proWebISSv2, proActconv202, proSH3, proSIAPNet,
         proBelford, proISSJoinville, proSmarAPDABRASF, proAsten, proELv2, proTiplanv2] then
        Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI)
      else
      begin
        if not (FProvedor in [proGoiania, proSigep]) then
          Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
      end;
    end;

    if FProvedor in [proTecnos] then
      if NFSe.PrestadorServico.Endereco.CodigoMunicipio <> '' then
        Gerador.wCampoNFSe(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, DSC_CMUN)
      else
        Gerador.wCampoNFSe(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.Servico.CodigoMunicipio, DSC_CMUN);

    GerarServicoValores;
  end;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;

  if NFSe.RegimeEspecialTributacao <> retNenhum then
    Gerador.wCampoNFSe(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), DSC_REGISSQN);

  if FProvedor = proTecnos then
  begin
    Gerador.wCampoNFSe(tcStr, '#7', 'NaturezaOperacao      ', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP);
    Gerador.wCampoNFSe(tcStr, '#8', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN);
    Gerador.wCampoNFSe(tcStr, '#9', 'IncentivoFiscal       ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCENTIVO);
  end;

  if not (FProvedor in [proGoiania, proTecnos, proSigep]) then
  begin
    Gerador.wCampoNFSe(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN);
    Gerador.wCampoNFSe(tcStr, '#8', 'IncentivoFiscal       ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCENTIVO);
  end;

  if FProvedor = proTecnos then
    Gerador.wCampoNFSe(tcStr, '#9', 'OutrasInformacoes', 00, 255, 0, NFSe.OutrasInformacoes, DSC_OUTRASINF);

  if FProvedor in [proELv2, proISSJoinville, proPublica, proSmarAPDABRASF] then
    Gerador.wCampoNFSe(tcStr, '#9', 'InformacoesComplementares', 00, 2000, 0, NFSe.InformacoesComplementares, DSC_OUTRASINF);

  if FProvedor = profintelISS then
    GerarValoresServico;

  if FProvedor in [proAgili, proISSDigital] then
    Gerador.wCampoNFSe(tcStr, '#9', 'Producao', 01, 01, 1, SimNaoToStr(NFSe.Producao), DSC_TPAMB);

  Gerador.wGrupoNFSe('/InfDeclaracaoPrestacaoServico');

  if FProvedor in [proTecnos] then
    Gerador.WGrupoNFSe('/tcDeclaracaoPrestacaoServico');
end;

////////////////////////////////////////////////////////////////////////////////

constructor TNFSeW_ABRASFv2.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_ABRASFv2.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_ABRASFv2.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo := FPrefixo4;

  if (FProvedor in [pro4R, proABase, proActcon, proAgili, proCoplan, proDigifred,
     profintelISS, proFiorilli, proGoiania, proGovDigital,
     proISSDigital, proLink3, proProdata, proPVH, proSaatri,
     proSisPMJP, proSystemPro, proTecnos, proVirtual, proVitoria,
     proNFSEBrasil, proNEAInformatica, proNotaInteligente, proVersaTecnologia,
     proSH3, proSIAPNet]) then
    FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '') then
    FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> '' then
    Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
  else
    Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  if (FProvedor in [proISSDigital, proNotaInteligente]) and (NFSe.NumeroLote <> '') then
    Atributo := ' Id="' + (NFSe.IdentificacaoRps.Numero) + '"';

  if (FProvedor in [proNotaInteligente, proPronimv2, proTiplanv2]) then
    Gerador.wGrupo('Rps')
  else
    Gerador.wGrupo('Rps' + Atributo);

  case FProvedor of
    proDigifred,
    proFiorilli,
    proISSe,
    proSisPMJP,
    proPVH,
    proNEAInformatica,
    proMitra: NFSe.InfID.ID := 'rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                                       FNFSe.IdentificacaoRps.Serie;

    (*
    proISSDigital: FNFSe.InfID.ID := {'rps' + }ChaveAcesso(FNFSe.Prestador.cUF,
                                               FNFSe.DataEmissao,
                                               OnlyNumber(FNFSe.Prestador.Cnpj),
                                               0, // Serie
                                               StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)),
                                               StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)));
    *)

    proISSDigital: FNFSe.InfID.ID := OnlyNumber(FNFSe.Prestador.Cnpj) +
                                     OnlyNumber(NFSe.Prestador.InscricaoMunicipal) +
                                     OnlyNumber(NFSe.NumeroLote) +
                                     OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proTecnos: FNFSe.InfID.ID := '1' + //Fixo - Lote Sincrono
      //                                 FormatDateTime('yyyy', FNFSe.DataEmissao) +
                                       OnlyNumber(FNFSe.Prestador.Cnpj) +
                                       IntToStrZero(StrToIntDef(FNFSe.IdentificacaoRps.Numero, 1), 16);

    proABase,
    proSiam,
    proGovDigital: FNFSe.InfID.ID := 'Rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proBethav2: FNFSe.InfID.ID := 'rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proNotaInteligente: FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proPronimv2,
    proVirtual: FNFSe.InfID.ID := '';
  else
    FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;
  end;

  if (Provedor = proPronimv2) then
    Gerador.Opcoes.SuprimirDecimais := True;

  GerarXML_ABRASF_v2;

  if FOpcoes.GerarTagAssinatura <> taNunca then
  begin
    Gerar := true;
    if FOpcoes.GerarTagAssinatura = taSomenteSeAssinada then
      Gerar := ((NFSe.signature.DigestValue <> '') and
        (NFSe.signature.SignatureValue <> '') and
        (NFSe.signature.X509Certificate <> ''));
    if FOpcoes.GerarTagAssinatura = taSomenteParaNaoAssinada then
      Gerar := ((NFSe.signature.DigestValue = '') and
        (NFSe.signature.SignatureValue = '') and
        (NFSe.signature.X509Certificate = ''));
    if Gerar then
    begin
      FNFSe.signature.URI := FNFSe.InfID.ID;
      FNFSe.signature.Gerador.Opcoes.IdentarXML := Gerador.Opcoes.IdentarXML;
      FNFSe.signature.GerarXMLNFSe;
      Gerador.ArquivoFormatoXML := Gerador.ArquivoFormatoXML +
                                   FNFSe.signature.Gerador.ArquivoFormatoXML;
    end;
  end;

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
