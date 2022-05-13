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

unit pnfsNFSeW_ABRASFv2;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts, pcnAuxiliar, pcnConsts, pcnConversao, pcnGerador,
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
    procedure GerarEnderecoExterior;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;
    procedure GerarValoresServico;
    procedure GerarListaItensServico;

    procedure GerarConstrucaoCivil;

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
  Gerador.wGrupo('IdentificacaoRps');
  Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  if (FProvedor <> ProSigep) and (FProvedor <> proMegaSoft)  then
    Gerador.wCampo(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);
  if (FProvedor <> proMegaSoft) then
  begin
    if (FProvedor <> ProSigep) then
      Gerador.wCampo(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS)
    else
      Gerador.wCampo(tcStr, '#3', 'Tipo  ', 01, 02, 1, 'R1', DSC_TIPORPS);
  end;

  Gerador.wGrupo('/IdentificacaoRps');
end;

procedure TNFSeW_ABRASFv2.GerarRPSSubstituido;
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

procedure TNFSeW_ABRASFv2.GerarPrestador;
begin
  Gerador.wGrupo('Prestador');

  Gerador.wGrupo('CpfCnpj');
  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampo(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CPF)
  else
    Gerador.wCampo(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), DSC_CNPJ);
  Gerador.wGrupo('/CpfCnpj');

  if (FProvedor = proTecnos) then
    Gerador.wCampo(tcStr, '#35', 'RazaoSocial', 01, 15, 1, NFSe.PrestadorServico.RazaoSocial, DSC_XNOME);

  Gerador.wCampo(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, DSC_IM);

  if FProvedor = proAsten then
    Gerador.wCampo(tcStr, '#35', 'Token', 01, 50, 0, nfse.Prestador.Token, ''); //RNR

  if (FProvedor in [proISSDigital, proAgili]) then
  begin
    Gerador.wCampo(tcStr, '#36', 'Senha       ', 01, 255, 1, NFSe.Prestador.Senha, DSC_SENHA);
    Gerador.wCampo(tcStr, '#37', 'FraseSecreta', 01, 255, 1, NFSe.Prestador.FraseSecreta, DSC_FRASESECRETA);
  end
  else if FProvedor = proAdm then
  begin
    Gerador.wCampo(tcStr, '#36', 'Key       ', 01, 255, 1, NFSe.Prestador.Key, DSC_KEY);
    Gerador.wCampo(tcStr, '#37', 'Auth      ', 01, 255, 1, NFSe.Prestador.Auth, DSC_AUTH);
    Gerador.wCampo(tcStr, '#37', 'RequestId ', 01, 255, 1, NFSe.Prestador.RequestId, DSC_REQUESTID);
  end;

  Gerador.wGrupo('/Prestador');
end;

procedure TNFSeW_ABRASFv2.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
  begin
    if (FProvedor in [proActcon, proELv2, proActconv204, proISSJoinville,
        proSmarAPDABRASF, proNotaInteligente, proGiss, proTcheInfov2, proiiBrasilv2,
        proSiapSistemas, proAdm, proAbacov2]) or
       ((FProvedor in [proActconv201, proActconv2]) and (FVersaoDados = '2.01')) or
       ((FProvedor = profintelISS) and (NFSe.Servico.CodigoMunicipio <> '3136702')) or
       ((FProvedor = proVersaTecnologia) and (NFSe.Servico.CodigoMunicipio <> '3115300')) then
      Gerador.wGrupo('TomadorServico')
    else
      Gerador.wGrupo('Tomador');

    if (((NFSe.Tomador.Endereco.UF <> 'EX') and (NFSe.Tomador.Endereco.UF <> '')) or
        ((FProvedor in [proGoiania, proMegaSoft]) and (NFSe.Tomador.Endereco.UF = ''))) and
       ((NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
       (NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal <> '') or
       (NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual <> '')) then
    begin
      Gerador.wGrupo('IdentificacaoTomador');
      if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') then
      begin
        Gerador.wGrupo('CpfCnpj');
        if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
          Gerador.wCampo(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CPF)
        else
          Gerador.wCampo(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), DSC_CNPJ);
        Gerador.wGrupo('/CpfCnpj');
      end;

      Gerador.wCampo(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM);
 
      if FProvedor in [proCoplan] then
        Gerador.wCampo(tcStr, '#38', 'InscricaoEstadual',  01, 30, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, DSC_IE);

      Gerador.wGrupo('/IdentificacaoTomador');
    end;

    if (NFSe.Tomador.Endereco.UF = 'EX') and
       (FProvedor in [proISSJoinville, proSmarAPDABRASF]) then
      Gerador.wCampo(tcStr, '#38', 'NifTomador', 001, 40, 0, NFSe.Tomador.NifTomador);

    Gerador.wCampo(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, DSC_XNOME);

    if NFSe.Tomador.Endereco.Endereco <> '' then
    begin
      if (NFSe.Tomador.Endereco.UF = 'EX') and
         (FProvedor in [proISSJoinville, proSmarAPDABRASF]) then
        GerarEnderecoExterior
      else
      begin
        Gerador.wGrupo('Endereco');

        if FProvedor = proSigep then
        begin
          Gerador.wCampo(tcStr, '#39', 'TipoLogradouro', 001, 50, 0, NFSe.Tomador.Endereco.TipoLogradouro, DSC_XLGR);
          Gerador.wCampo(tcStr, '#39', 'Logradouro', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR);
        end
        else
          Gerador.wCampo(tcStr, '#39', 'Endereco', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR);

        Gerador.wCampo(tcStr, '#40', 'Numero  ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, DSC_NRO);

        if not (FProvedor in [proNFSeBrasil, ProTecnos]) then
          Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 060, 0, NFSe.Tomador.Endereco.Complemento, DSC_XCPL)
        else
          Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, DSC_XCPL);

        Gerador.wCampo(tcStr, '#42', 'Bairro     ', 001, 060, 0, NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO);

        if (FProvedor = proMegaSoft) then
            Gerador.wCampo(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP);

        Gerador.wCampo(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);
        if (FProvedor <> proMegaSoft) then
          Gerador.wCampo(tcStr, '#44', 'Uf             ', 2, 2, 0, NFSe.Tomador.Endereco.UF, DSC_UF);

        if not (FProvedor in [proELv2, proActconv204, proNFSeBrasil, proPronimv2,
                              proISSJoinville, proSmarAPDABRASF, proGiss,
                              proTcheInfov2, proSigep, proiiBrasilv2, proMegaSoft,
                              proModernizacaoPublica, proDigifred, proSiapSistemas,
                              proSmarAPDv23, proAbacov2, proElotech]) or
           ((FProvedor in [proPronimv2, proModernizacaoPublica, proElotech]) and (OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio) = '9999999')) then
          Gerador.wCampo(tcInt, '#34', 'CodigoPais ', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, DSC_CPAIS);

        if (FProvedor <> proMegaSoft) then
        begin
          if FProvedor in [proELv2, proActconv204] then
            Gerador.wCampo(tcStr, '#45', 'Cep', 008, 008, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP)
          else
            Gerador.wCampo(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP);
        end;

        Gerador.wGrupo('/Endereco');
      end;
    end;

    case FProvedor of
      proNFSeBrasil:
        begin
          Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
        end;
      proMegaSoft:
        begin

        end;
      proTecnos:  
        begin
          Gerador.wGrupo('Contato');
          Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 1, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);
          Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 80, 1, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wGrupo('/Contato');
        end; 
    else
      begin
        if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
        begin
          Gerador.wGrupo('Contato');
          Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE);

          if FProvedor = proSigep then
          begin
            Gerador.wCampo(tcStr, '#048', 'Ddd', 01, 03, 0, NFSe.Tomador.Contato.DDD, DSC_DDD);
            Gerador.wCampo(tcStr, '#049', 'TipoTelefone', 01, 02, 0, NFSe.Tomador.Contato.TipoTelefone, DSC_TPTELEFONE);
          end;

          Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 80, 0, NFSe.Tomador.Contato.Email, DSC_EMAIL);
          Gerador.wGrupo('/Contato');
        end;
      end;
    end;

    if FProvedor = proiiBrasilv2 then
    begin
      Gerador.wCampo(tcStr, '#', 'AtualizaTomador', 01, 01, 1, SimNaoToStr(NFSe.Tomador.AtualizaTomador), '****');
      Gerador.wCampo(tcStr, '#', 'TomadorExterior', 01, 01, 1, SimNaoToStr(NFSe.Tomador.TomadorExterior), '****');
    end;

    if (FProvedor in [proActcon, proELv2, proActconv204, proISSJoinville,
        proSmarAPDABRASF, proNotaInteligente, proGiss, proTcheInfov2, proiiBrasilv2,
        proSiapSistemas, proAdm, proAbacov2]) or
        ((FProvedor in [proActconv201, proActconv2]) and (FVersaoDados = '2.01')) or
       ((FProvedor = profintelISS) and (NFSe.Servico.CodigoMunicipio <> '3136702')) or
       ((FProvedor = proVersaTecnologia) and (NFSe.Servico.CodigoMunicipio <> '3115300')) then
      Gerador.wGrupo('/TomadorServico')
    else
      Gerador.wGrupo('/Tomador');
  end
  else
  begin
    if not (FProvedor in [proGiss]) then
    begin
      // Gera a TAG vazia quando nenhum dado do tomador for informado.
      if FProvedor in [proActcon, proVersaTecnologia, proSmarAPDABRASF, proAdm,
                       proSmarAPDv23] then
        Gerador.wCampo(tcStr, '#', 'TomadorServico', 0, 1, 1, '', '')
      else
        Gerador.wCampo(tcStr, '#', 'Tomador', 0, 1, 1, '', '');
    end;
  end;
end;

procedure TNFSeW_ABRASFv2.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial <> '') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupo('Intermediario');
    Gerador.wGrupo('IdentificacaoIntermediario');
    Gerador.wGrupo('CpfCnpj');

    if FProvedor = proVirtual then
    begin
      if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      begin
        Gerador.wCampo(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CPF);
        Gerador.wCampo(tcStr, '#49', 'Cnpj', 14, 14, 1, '', '');
      end
      else
      begin
        Gerador.wCampo(tcStr, '#49', 'Cpf ', 11, 11, 1, '', '');
        Gerador.wCampo(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CNPJ);
      end;
    end
    else
    begin
      if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
        Gerador.wCampo(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CPF)
      else
        Gerador.wCampo(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), DSC_CNPJ);
    end;

    Gerador.wGrupo('/CpfCnpj');

    if FProvedor = proVirtual then
      Gerador.wCampo(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 1, NFSe.IntermediarioServico.InscricaoMunicipal, DSC_IM)
    else
      Gerador.wCampo(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, DSC_IM);

    Gerador.wGrupo('/IdentificacaoIntermediario');

    if FProvedor in [proVirtual, proVersaTecnologia, proAdm] then
      Gerador.wCampo(tcStr, '#48', 'RazaoSocial', 001, 115, 1, NFSe.IntermediarioServico.RazaoSocial, DSC_XNOME)
    else
      Gerador.wCampo(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, DSC_XNOME);

    Gerador.wGrupo('/Intermediario');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarServicoValores;
begin
  Gerador.wGrupo('Servico');

  if FProvedor in [proTecnos] then
    Gerador.wGrupo('tcDadosServico');

  Gerador.wGrupo('Valores');

  if Provedor = ProTecnos then
  begin
    Gerador.wCampo(tcDe2, '#13', 'BaseCalculoCRS', 01, 15, 1, 0 , DSC_VSERVICO);
    Gerador.wCampo(tcDe2, '#13', 'IrrfIndenizacao', 01, 15, 1, 0 , DSC_VSERVICO);
  end;

  Gerador.wCampo(tcDe2, '#13', 'ValorServicos  ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO);

  case FProvedor of
    proTecnos:
    begin
      Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);
      Gerador.wCampo(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
      Gerador.wCampo(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
      Gerador.wCampo(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
      Gerador.wCampo(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
      Gerador.wCampo(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
      Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
      Gerador.wCampo(tcDe2, '#21', 'ValorIss       ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VINSS)
    end;

    proABase,
    proActcon,
    proAdm,
    proPronimv2,
    proVirtual,
    proVersaTecnologia,
    proCoplan,
    proSigCorp,
    proDeISS:
      begin
        Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes  ', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);
        Gerador.wCampo(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
        Gerador.wCampo(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
        Gerador.wCampo(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
        Gerador.wCampo(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
        Gerador.wCampo(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
        Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
      end;
  else
    begin
      if FProvedor = proSimplISSv2 then
        Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes', 01, 15, 1, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS)
      else
        Gerador.wCampo(tcDe2, '#14', 'ValorDeducoes', 01, 15, 0, NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS);

      case FProvedor of
        proISSe,
        proNEAInformatica,
        proProdata,
        proSystemPro,
        proVitoria,
        proTecnos:
          begin
            Gerador.wCampo(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
            Gerador.wCampo(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
            Gerador.wCampo(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
            Gerador.wCampo(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
            Gerador.wCampo(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
            Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
          end;

        proSimplISSv2, 
		proMegaSoft:
        begin
          Gerador.wCampo(tcDe2, '#15', 'ValorPis       ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
          Gerador.wCampo(tcDe2, '#16', 'ValorCofins    ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
          Gerador.wCampo(tcDe2, '#17', 'ValorInss      ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
          Gerador.wCampo(tcDe2, '#18', 'ValorIr        ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
          Gerador.wCampo(tcDe2, '#19', 'ValorCsll      ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
        end;

      else
        begin
          Gerador.wCampo(tcDe2, '#15', 'ValorPis       ', 01, 15, 0, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
          Gerador.wCampo(tcDe2, '#16', 'ValorCofins    ', 01, 15, 0, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
          Gerador.wCampo(tcDe2, '#17', 'ValorInss      ', 01, 15, 0, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
          Gerador.wCampo(tcDe2, '#18', 'ValorIr        ', 01, 15, 0, NFSe.Servico.Valores.ValorIr, DSC_VIR);
          Gerador.wCampo(tcDe2, '#19', 'ValorCsll      ', 01, 15, 0, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
          Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 0, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
        end;
      end;

      if not (FProvedor in [proCenti, proProdata, proGoiania, proSigep, proSimplISSv2, proMegaSoft, proDeISS, proiiBrasilv2]) then
      begin
        if FProvedor in [pro4R, proISSDigital, proISSe, proSystemPro, proFiorilli,
            proSaatri, proCoplan, proLink3, proTecnos, proNEAInformatica, proSH3] then
          Gerador.wCampo(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VINSS)
        else
          Gerador.wCampo(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, DSC_VINSS);
      end;
    end;
  end;

  if FProvedor = proSimplISSv2 then
  begin
    Gerador.wCampo(tcDe2, '#23', 'OutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES);
    Gerador.wCampo(tcDe2, '#22', 'ValTotTributos ', 01, 15, 1, NFSe.Servico.Valores.ValorTotalTributos);
  end
  else if FProvedor = proDeISS then
    Gerador.wCampo(tcDe2, '#22', 'ValTotTributos ', 01, 15, 1, NFSe.Servico.Valores.ValorTotalTributos);

  if FProvedor in [proActcon, proAgili] then
    Gerador.wCampo(tcDe2, '#24', 'BaseCalculo', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS);

  if FProvedor in [proABase, proActcon, proPronimv2, proVirtual, proVersaTecnologia,
                   proSimplISSv2, proAdm] then
    Gerador.wCampo(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VINSS)
  else if FProvedor in [proCoplan, proDeISS] then
    Gerador.wCampo(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, DSC_VINSS);

  case FProvedor of
    proCoplan, proDigifred, proELv2, proFriburgo, proNEAInformatica,
    proNotaInteligente, proSisPMJP, proVitoria, proSmarAPDABRASF, proGiss,
    proDeISS, proTcheInfov2, proCenti, proRLZ, proiiBrasilv2, proTecnos,
    proSigCorp, proSiapSistemas, proISSJoinville, proSmarAPDv23,
    proAbacov2:
      Gerador.wCampo(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proABase, proDesenvolve, proEReceita, proProdata, proSafeWeb,
    proSimplISSv2:
      Gerador.wCampo(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    pro4R, proISSDigital, proISSe, proLink3, proSaatri, proSystemPro, proVirtual,
    proVersaTecnologia, proAdm,
    proSH3:
      Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proGoiania:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ)
      else
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proModernizacaoPublica:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampo(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ)
      else
        Gerador.wCampo(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);

    proPronimv2:
      if NFSe.OptanteSimplesNacional = snSim then
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, DSC_VALIQ)
      else
        Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);
  else
    Gerador.wCampo(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, DSC_VALIQ);
  end;

  case FProvedor of
    proABase, proActcon, proDeISS, proPronimv2, proTecnos, proVirtual, proCoplan,
    proVersaTecnologia, proSigCorp, proSimplISSv2,
    proAdm:
      begin
        Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
        Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      end;

    pro4R, proCenti, proFiorilli, proGoiania, proISSDigital, proISSe,
    proSystemPro, proPVH, proSaatri, proLink3, proNEAInformatica,
    proNotaInteligente, proVitoria, proSH3,
    proSIAPNet:
      begin
        Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
        if not (FProvedor in [proCenti]) then
          Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      end;

    proProdata, 
    proMegaSoft:
      Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);

  else
    begin
      Gerador.wCampo(tcDe2, '#27', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND);
      Gerador.wCampo(tcDe2, '#28', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND);
    end;
  end;

  Gerador.wGrupo('/Valores');

  if not (FProvedor in [proGoiania, proSigep]) then
    Gerador.wCampo(tcStr, '#20', 'IssRetido', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido, FProvedor), DSC_INDISSRET);

  if ((NFSe.Servico.Valores.IssRetido <> stNormal) and not
      (FProvedor in [proGoiania, proSigep, proMegaSoft, proElotech])) or
     (FProvedor in [proProdata, proVirtual, proVersaTecnologia, proTecnos, proAdm]) then
    Gerador.wCampo(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET);

  if not (FProvedor in [proGoiania, proMegaSoft, proElotech]) then
  begin
    case FProvedor of
      proBethav2,
      proSisPMJP,
      proVirtual: Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 0, OnlyNumber(NFSe.Servico.ItemListaServico), DSC_CLISTSERV);

      proNotaInteligente: Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);

      proISSJoinville,
      proVitoria:
        begin
          if copy(NFSe.Servico.ItemListaServico, 1, 1) = '0' then
            Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 0, copy(NFSe.Servico.ItemListaServico, 2, 4), DSC_CLISTSERV)
          else
            Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
        end;
    else
      Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);
    end;

    if FProvedor in [proVersaTecnologia, proDesenvolve, proAdm]  then
      Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE)
    else
      Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 07, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
  end;


  if not (FProvedor in [proElotech, proMegaSoft]) then
  begin
    case FProvedor of
      proGoiania, proVirtual, proVersaTecnologia, proAdm:
        Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);

      proSIAPNet,
      proSimplISSv2:
        Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN);
    else
      Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);
    end;

    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
      StringReplace(FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);
  end;

  if (FProvedor = proElotech) then
  begin
    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
      StringReplace(FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);
    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN);
  end;

  Gerador.wCampo(tcStr, '#33', 'CodigoMunicipio', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);

  if (FProvedor = proMegaSoft) then
  begin
    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN);
    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
      StringReplace(FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);
  end;

  if not (FProvedor in [proSigep, proMegaSoft, proDigifred, proSiapSistemas]) then
  begin
    if FProvedor in [proVirtual, proVersaTecnologia, proSimplISSv2, proAdm] then
      Gerador.wCampo(tcInt, '#34', 'CodigoPais', 04, 04, 1, NFSe.Servico.CodigoPais, DSC_CPAIS)
    else
    begin
      if FProvedor = proAbacov2 then
      begin
        if (NFSe.Servico.ExigibilidadeISS = exiExportacao) then
          Gerador.wCampo(tcInt, '#34', 'CodigoPais', 04, 04, 0, NFSe.Servico.CodigoPais, DSC_CPAIS);
      end
      else
        Gerador.wCampo(tcInt, '#34', 'CodigoPais', 04, 04, 0, NFSe.Servico.CodigoPais, DSC_CPAIS);
    end;
  end;

  if not (FProvedor in [proGoiania, proMegaSoft]) then
  begin
    case FProvedor of
      proSigep:
        Gerador.wCampo(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ('0' + ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS)), DSC_INDISS);
      proiiBrasilv2:
        begin
          // Não gera a tag.
        end
    else
      Gerador.wCampo(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), DSC_INDISS);
    end;

    if not (FProvedor in [proiiBrasilv2, proSigep]) then
    begin
      if not (FProvedor in [proProdata, proVirtual]) then
        Gerador.wCampo(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 0, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI)
      else
        Gerador.wCampo(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 1, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI);
    end;
  end;

  if (FProvedor in [ ProTecnos])then
    Gerador.wCampo(tcStr, '#37', 'NumeroProcesso', 01, 30, 1, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);
  {else
    Gerador.wCampo(tcStr, '#37', 'NumeroProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);}


  if not (FProvedor in [ ProTecnos]) then
    Gerador.wCampo(tcStr, '#37', 'NumeroProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);

  if FProvedor in [proTecnos] then
    Gerador.wGrupo('/tcDadosServico');

  if FProvedor = proElotech then
    GerarListaItensServico;

  Gerador.wGrupo('/Servico');
end;

procedure TNFSeW_ABRASFv2.GerarListaItensServico;
var
  i: Integer;
begin
  Gerador.wGrupo('ListaItensServico');

  for I := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Gerador.wGrupo('ItemServico');
    Gerador.wCampo(tcStr, '', 'ItemListaServico', 1, 6, 1, NFSe.Servico.ItemServico[i].ItemListaServico);
    Gerador.wCampo(tcStr, '', 'CodigoCnae', 1, 7, 0, NFSe.Servico.CodigoCnae);
    Gerador.wCampo(tcStr, '', 'Descricao', 1, 20, 0, NFSe.Servico.ItemServico[i].Descricao);
    Gerador.wCampo(tcStr, '', 'Tributavel', 1, 1, 0, SimNaoToStr(NFSe.Servico.ItemServico[i].Tributavel));
    Gerador.wCampo(tcDe2, '', 'Quantidade', 0, 17, 0, NFSe.Servico.ItemServico[i].Quantidade);
    Gerador.wCampo(tcDe2, '', 'ValorUnitario', 0, 17, 0, NFSe.Servico.ItemServico[i].ValorUnitario);
    Gerador.wCampo(tcDe2, '', 'ValorDesconto', 0, 17, 1, NFSe.Servico.ItemServico[i].DescontoCondicionado);
    Gerador.wCampo(tcDe2, '', 'ValorLiquido', 0, 17, 1, NFSe.Servico.ItemServico[i].ValorTotal);
    Gerador.wGrupo('/ItemServico');
  end;
  Gerador.wGrupo('/ListaItensServico');
end;

procedure TNFSeW_ABRASFv2.GerarListaServicos;
var
  i: Integer;
  Temp: string;
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
    if (FProvedor = proSystemPro) then
    begin
      Gerador.wCampo(tcDe2, '', 'ValorTTS', 01, 15, 0, NFSe.Servico.ItemServico[i].ValorTaxaTurismo);
      Gerador.wCampo(tcDe2, '', 'QuantDiarias', 01, 15, 0, NFSe.Servico.ItemServico[i].QuantidadeDiaria);
    end;
    Gerador.wCampo(tcDe2, '#25', 'Aliquota              ', 01, 05, 1, NFSe.Servico.ItemServico[i].Aliquota, DSC_VALIQ);
    Gerador.wCampo(tcDe2, '#24', 'BaseCalculo           ', 01, 15, 1, NFSe.Servico.ItemServico[i].BaseCalculo, DSC_VBCISS);
    if FProvedor = proSystemPro then
    begin
      Gerador.wCampo(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoIncondicionado, DSC_VDESCINCOND);
      Gerador.wCampo(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 0, NFSe.Servico.ItemServico[i].DescontoCondicionado, DSC_VDESCCOND);
      Gerador.wCampo(tcDe2, '#15', 'ValorPis   ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorPis, DSC_VPIS);
      Gerador.wCampo(tcDe2, '#16', 'ValorCofins', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCofins, DSC_VCOFINS);
      Gerador.wCampo(tcDe2, '#17', 'ValorInss  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorInss, DSC_VINSS);
      Gerador.wCampo(tcDe2, '#18', 'ValorIr    ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorIr, DSC_VIR);
      Gerador.wCampo(tcDe2, '#19', 'ValorCsll  ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorCsll, DSC_VCSLL);
    end;

    Gerador.wGrupo('/Valores');

    if FProvedor = proAdm then
    begin
      case NFSe.CondicaoPagamento.Condicao of
        cpAVista:         Temp := 'Avista';
        cpNaApresentacao: Temp := 'Naapresentacao';
        cpCartaoCredito:  Temp := 'CartaoCredito';
        cpCartaoDebito:   Temp := 'CartaoDebito';
      else //cpAPrazo:
        Temp := 'Aprazo';
      end;

      Gerador.wCampo(tcStr, '#37', 'CondicaoDePagamento     ', 01, 30, 0, Temp, DSC_CONDPAGTO);
    end;

    if not (FProvedor in [proCenti, proSigep]) then
      Gerador.wCampo(tcStr, '#20', 'IssRetido', 01, 01, 1, SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET);

    Gerador.wCampo(tcStr, '#29', 'ItemListaServico', 01, 0005, 1, NFSe.Servico.ItemListaServico, DSC_CLISTSERV);

    if not (FProvedor in [proCenti]) then
    begin
      if FProvedor in [proVersaTecnologia, proAdm] then
        Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE)
      else
        Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE);
    end;

    Gerador.wCampo(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), DSC_CSERVTRIBMUN);

    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
      StringReplace(NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase]), DSC_DISCR);
    Gerador.wCampo(tcStr, '#33', 'CodigoMunicipio          ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);

    if not (FProvedor in [proDigifred]) then
    begin
      Gerador.wCampo(tcInt, '#34', 'CodigoPais               ', 04, 04, 0, NFSe.Servico.CodigoPais, DSC_CPAIS);
    end;

    if not (FProvedor in [proCenti]) then
      Gerador.wCampo(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), DSC_INDISS);

    Gerador.wCampo(tcInt, '#36', 'MunicipioIncidencia', 07, 07, 0, NFSe.Servico.MunicipioIncidencia, DSC_MUNINCI);
    Gerador.wCampo(tcStr, '#37', 'NumeroProcesso     ', 01, 30, 0, NFSe.Servico.NumeroProcesso, DSC_NPROCESSO);

    if (NFSe.Servico.Valores.IssRetido <> stNormal) and (FProvedor = proSystemPro) then
      Gerador.wCampo(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), DSC_INDRESPRET);

    Gerador.wGrupo('/Servico');
  end;

  if FProvedor <> proSystemPro then
    Gerador.wGrupo('/ListaServicos');
end;

procedure TNFSeW_ABRASFv2.GerarValoresServico;
begin
  Gerador.wGrupo('ValoresServico');
  Gerador.wCampo(tcDe2, '#15', 'ValorPis        ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, DSC_VPIS);
  Gerador.wCampo(tcDe2, '#16', 'ValorCofins     ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS);
  Gerador.wCampo(tcDe2, '#17', 'ValorInss       ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, DSC_VINSS);
  Gerador.wCampo(tcDe2, '#18', 'ValorIr         ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, DSC_VIR);
  Gerador.wCampo(tcDe2, '#19', 'ValorCsll       ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, DSC_VCSLL);
  Gerador.wCampo(tcDe2, '#21', 'ValorIss        ', 01, 15, 1, NFSe.Servico.Valores.ValorIss, DSC_VISS);
  Gerador.wCampo(tcDe2, '#13', 'ValorLiquidoNfse', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE);
  Gerador.wCampo(tcDe2, '#13', 'ValorServicos   ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO);
  Gerador.wGrupo('/ValoresServico');
end;

procedure TNFSeW_ABRASFv2.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') or (FProvedor in [ProTecnos ]) then
  begin
    Gerador.wGrupo('ConstrucaoCivil');
    Gerador.wCampo(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA);
    Gerador.wCampo(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, DSC_ART);
    Gerador.wGrupo('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarCredenciais;
begin
  Gerador.wGrupo('credenciais');
  Gerador.wCampo(tcStr, '#01', 'usuario     ', 01, 15, 1, NFSe.Autenticador, DSC_USUARIO);
  Gerador.wCampo(tcStr, '#02', 'senha       ', 01, 05, 1, NFSe.Prestador.Senha, DSC_SENHA);
  Gerador.wCampo(tcStr, '#03', 'chavePrivada', 01, 01, 1, NFSe.Assinatura, DSC_ASSINATURA);
  Gerador.wGrupo('/credenciais');
end;

procedure TNFSeW_ABRASFv2.GerarXML_ABRASF_v2;
var
 i : Integer;
begin
  case FProvedor of
    proABase, proDigifred,proBethav2,  proEReceita, proFiorilli, proGovDigital,
    proISSe, proMitra, proNEAInformatica, proNotaInteligente, proPVH, proSisPMJP,
    proCoplan, proSIAPNet, proSystemPro, proISSJoinville, proDesenvolve,
    proBelford, proWebISSv2, proMegaSoft, proModernizacaoPublica,
    proVitoria, proActconv204, proSiapSistemas, proAbacov2:
      Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');

    proiiBrasilv2:
      begin
        // São Sebastião - SP
        if NFSe.Servico.CodigoMunicipio = '3550704' then
          Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + OnlyNumber(NFSe.InfID.ID) + '"')
        else
          Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
      end;

    proDeISS,
    proRLZ,
    proSigCorp,
    proSimplISSv2:
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="dec' + NFSe.InfID.ID + '"');

    proISSDigital:
        // alterado em 23/03/2018 para ver se funciona com a cidade de Cabo Frio
        // alterado em 09/05/2018 por italo (incluido novamente o namespace)
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');

    proSaatri,
    proSiam:
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="Declaracao_' + OnlyNumber(NFSe.Prestador.Cnpj) + '"');

    proTecnos:
      begin
        Gerador.wGrupo('tcDeclaracaoPrestacaoServico');
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');
      end;

    proVirtual:
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '=""');

    proSmarAPDABRASF,
    proSmarAPDv23:
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="declaracao_' + NFSe.InfID.ID + '"');

    proTiplanv2:
        Gerador.wGrupo('InfDeclaracaoPrestacaoServico ' + 'xmlns="http://www.abrasf.org.br/nfse.xsd" ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
  else
    Gerador.wGrupo('InfDeclaracaoPrestacaoServico');
  end;

  if FProvedor = proSiapSistemas then
    Gerador.wCampo(tcStr, '#', 'Id', 01, 15, 1, NFSe.InfID.ID, '');

  // Não escrever os dados do RPS se não houver um tipo definido
  if NFSe.IdentificacaoRps.Tipo <> trNone then
  begin
    case FProvedor of
      proABase, proDigifred, proBethav2, proEReceita, proFiorilli, proGovDigital,
      proISSe, proMitra, proNEAInformatica, proNotaInteligente, proPVH, proSisPMJP,
      proCoplan, proSIAPNet, proSystemPro, proPronimv2, proTecnos, proTiplanv2,
      proSigep, proDesenvolve, proCenti, proMegaSoft, proVitoria, proSiapSistemas,
      proElotech, proAbacov2:
        Gerador.wGrupo('Rps');

      proISSDigital:
          Gerador.wGrupo('Rps ' + FIdentificador + '="' +
                                      OnlyNumber(NFSe.NumeroLote) +
                                      OnlyNumber(FNFSe.IdentificacaoRps.Numero) + '"');

      proSaatri,
      proSiam:
          Gerador.wGrupo('Rps ' + FIdentificador + '="' + NFSe.InfID.ID + '"');

      proVirtual:
          Gerador.wGrupo('Rps ' + FIdentificador + '=""');

    else
      begin
        if FIdentificador = '' then
          Gerador.wGrupo('Rps')
        else
          Gerador.wGrupo('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');
      end;
    end;

    GerarIdentificacaoRPS;

    case FProvedor of
      proABase, proActcon, proActconv201, proActconv2, proAgili, proBethav2,
      proCoplan, proEReceita, proFiorilli, proFriburgo, proGovDigital,
      proISSDigital, proISSe, proMitra, proNEAInformatica, proNotaInteligente,
      proProdata, proPronimv2, proPVH, proSaatri, proSisPMJP, proSiam, proVirtual,
      proVersaTecnologia, proVitoria, proWebISSv2, proSIAPNet,
      proBelford, proSystemPro, proSH3, proISSJoinville, proSmarAPDABRASF,
      proElv2, proActconv204, proAsten, proGiss, proDeISS, proTcheInfov2, proDataSmart,
      proDesenvolve, proRLZ, proTiplanv2, proSigCorp, proiiBrasilv2, proSimplISSv2,
      proModernizacaoPublica, proFuturize, proSiapSistemas, proElotech, proAdm,
      proAEG, proSmarAPDv23,
      proAbacov2: Gerador.wCampo(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

      proActconv202:
      begin
        if NFSe.Servico.CodigoMunicipio = '3101508' then
          Gerador.wCampo(tcDatHor, '#4', 'DataEmissao', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI)
        else
          Gerador.wCampo(tcDat, '#4', 'DataEmissao', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
      end;

      profintelISS:
        begin
          if NFSe.Servico.CodigoMunicipio <> '3136702' then
            Gerador.wCampo(tcDatHor, '#4', 'DataEmissao', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI)
          else
            Gerador.wCampo(tcDat, '#4', 'DataEmissao', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
        end;
    else
      Gerador.wCampo(tcDatHor, '#4', 'DataEmissao', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
    end;

    if (FProvedor <> proMegaSoft) then
    begin
      if FProvedor = ProSigep then
        Gerador.wCampo(tcStr, '#9', 'Status', 01, 01, 1, 'CO', DSC_INDSTATUS)
      else
        Gerador.wCampo(tcStr, '#9', 'Status', 01, 01, 1, StatusRPSToStr(NFSe.Status), DSC_INDSTATUS);
    end;

    GerarRPSSubstituido;

    Gerador.wGrupo('/Rps');
  end;

  if (FProvedor = profintelISS) and (NFSe.Servico.CodigoMunicipio <> '3136702') then
  begin
    GerarListaServicos;

    if NFSe.Competencia <> '' then
      Gerador.wCampo(tcStr, '#4', 'Competencia', 10, 19, 1, NFSe.Competencia, DSC_DEMI)
    else
      Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
  end
  else
  begin
    if NFSe.Competencia <> '' then
    begin
      case FProvedor of
        proActcon, proISSDigital, proMitra, proPVH, proSisPMJP, proVirtual,
        proSystemPro, proNEAInformatica,
        proEReceita: Gerador.wCampo(tcStr, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proABase, proBethav2, proFriburgo, proGovDigital, proNotaInteligente,
        proPronimv2, proVersaTecnologia, proWebISSv2, proActconv202, proBelford,
        proSH3, proSIAPNet, proISSJoinville, proSmarAPDABRASF, proELv2, proAsten,
        proTiplanv2, proDeISS, proTcheInfov2, proDataSmart, proDesenvolve,
        proRLZ, proGiss, proSigCorp, proiiBrasilv2, proSimplISSv2, proActconv204,
        proSiapSistemas, proModernizacaoPublica, proElotech, proAdm,
        proFuturize, proSmarAPDv23,
        proAbacov2: Gerador.wCampo(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proTecnos,
        proDigifred,
        proCenti: Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI);

        proSigep, proMegaSoft, proGoiania:
          Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, -1, NFSe.Competencia, DSC_DEMI);

        profintelISS:
          begin
           if NFSe.Servico.CodigoMunicipio <> '3136702' then
             Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI)
           else
             Gerador.wCampo(tcDat, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI);
          end;
      else
        Gerador.wCampo(tcStr, '#4', 'Competencia', 19, 19, 1, NFSe.Competencia, DSC_DEMI);
      end;
    end
    else
    begin
      case FProvedor of
        proSigep, proMegaSoft, proGoiania:
          Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, -1, NFSe.DataEmissao, DSC_DEMI);

        proCenti, proActconv2, ProTecnos, proSafeWeb, proDigifred, pro4R:
          Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);

        profintelISS:
          begin
            if NFSe.Servico.CodigoMunicipio <> '3136702' then
              Gerador.wCampo(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI)
            else
              Gerador.wCampo(tcDat, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
          end;
      else
        Gerador.wCampo(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);
      end;
    end;
{
    if FProvedor in [proTecnos] then
      if NFSe.PrestadorServico.Endereco.CodigoMunicipio <> '' then
        Gerador.wCampo(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio, DSC_CMUN)
      else
        Gerador.wCampo(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.Servico.CodigoMunicipio, DSC_CMUN);
}
    if FProvedor  = proSystemPro then
       GerarListaServicos
    else
       GerarServicoValores;
  end;

  GerarPrestador;
  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;

  if FProvedor = proAdm then
    Gerador.wCampo(tcStr, '#5', 'NaturezaOperacao', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP);

  if not (FProvedor in [proSigep, proiiBrasilv2, proMegaSoft,
                        proSiapSistemas]) then
    if NFSe.RegimeEspecialTributacao <> retNenhum then
      Gerador.wCampo(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), DSC_REGISSQN);

  if FProvedor = proTecnos then
  begin
    Gerador.wCampo(tcStr, '#7', 'NaturezaOperacao      ', 01, 01, 1, NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP);
    Gerador.wCampo(tcStr, '#8', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN);
    Gerador.wCampo(tcStr, '#9', 'IncentivoFiscal       ', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCENTIVO);

    Gerador.wCampo(tcDe2, '#9', 'PercentualCargaTributaria', 01, 05, 1, NFSe.PercentualCargaTributaria, '');
    Gerador.wCampo(tcDe2, '#9', 'ValorCargaTributaria     ', 01, 15, 1, NFSe.ValorCargaTributaria, '');

    Gerador.wCampo(tcDe2, '#9', 'PercentualCargaTributariaMunicipal', 01, 05, 1, NFSe.PercentualCargaTributariaMunicipal, '');
    Gerador.wCampo(tcDe2, '#9', 'ValorCargaTributariaMunicipal     ', 01, 15, 1, NFSe.ValorCargaTributariaMunicipal, '');

    Gerador.wCampo(tcDe2, '#9', 'PercentualCargaTributariaEstadual', 01, 05, 1, NFSe.PercentualCargaTributariaEstadual, '');
    Gerador.wCampo(tcDe2, '#9', 'ValorCargaTributariaEstadual     ', 01, 15, 1, NFSe.ValorCargaTributariaEstadual, '');
  end;


  if (FProvedor = proiiBrasilv2) and (NFSe.Servico.CodigoMunicipio = '3550704') then
  begin
    Gerador.wCampo(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 0, 1, DSC_REGISSQN);
    Gerador.wCampo(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, snNao, DSC_INDOPSN);
  end;


  if not (FProvedor in [proGoiania, proTecnos, proSigep, proiiBrasilv2, proMegaSoft]) then
  begin
    if FProvedor = proAdm then
    begin
      //Optante Simples: 0 Sim / 1 Não
      Gerador.wCampo(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, IfThen(NFSe.OptanteSimplesNacional = snSim, '0', '1'), DSC_INDOPSN);
      // Incentivador Fiscal: 0 Sim / 1 Não
      Gerador.wCampo(tcStr, '#8', 'IncentivadorCultural  ', 01, 01, 1, IfThen(NFSe.IncentivadorCultural = snSim, '0', '1'), DSC_INDINCENTIVO);
    end
    else
    begin
      if not (Provedor = proElotech) then
        Gerador.wCampo(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN);

      Gerador.wCampo(tcStr, '#8', 'IncentivoFiscal', 01, 01, 1, SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCENTIVO);
    end;
  end;

  if FProvedor = proTecnos then
    Gerador.wCampo(tcStr, '#9', 'OutrasInformacoes', 00, 255, 0, NFSe.OutrasInformacoes, DSC_OUTRASINF);

  if FProvedor in [proELv2, proActconv204, proISSJoinville, proPublica, proSmarAPDABRASF, proSmarAPDv23] then
    Gerador.wCampo(tcStr, '#9', 'InformacoesComplementares', 00, 2000, 0, NFSe.InformacoesComplementares, DSC_OUTRASINF);

  if (FProvedor = profintelISS) and (NFSe.Servico.CodigoMunicipio <> '3136702') then
    GerarValoresServico;

  if FProvedor in [proAgili, proISSDigital] then
    Gerador.wCampo(tcStr, '#9', 'Producao', 01, 01, 1, SimNaoToStr(NFSe.Producao), DSC_TPAMB);

  if FProvedor in [proTecnos] then
  begin
   Gerador.wCampo(tcStr, '#1' , 'TipoNota        ' ,01,01,1 , '1' , DSC_TPAMB );
   Gerador.wCampo(tcStr, '#44', 'SiglaUF         ' , 2, 2,0 , NFSe.PrestadorServico.Endereco.UF, DSC_UF);
   Gerador.wCampo(tcStr, '#4' , 'IdCidade        ' , 7, 7,1 , NFSe.PrestadorServico.Endereco.CodigoMunicipio, DSC_CMUN);
   Gerador.wCampo(tcStr, '#1' , 'EspecieDocumento' ,01,01,1 , '0' , DSC_TPAMB );
   Gerador.wCampo(tcStr, '#1' , 'SerieTalonario  ' ,01,01,1 , '0' , DSC_TPAMB );
   Gerador.wCampo(tcStr, '#1' , 'FormaPagamento  ' ,01,01,1 , '0' , DSC_TPAMB );
   Gerador.wCampo(tcStr, '#1' , 'NumeroParcelas  ' ,01,01,1 , '0' , DSC_TPAMB );
  end;

  if (FProvedor = proiiBrasilV2) and (NFSe.Quartos.Count > 0) then
  begin
    Gerador.wGrupo('Quartos');

    for i := 0 to NFSe.Quartos.Count -1 do
    begin
      Gerador.wGrupo('Quarto');
       Gerador.wCampo(tcInt, '#1', 'CodigoInternoQuarto' ,01, 09, 1, NFSe.Quartos.Items[i].CodigoInternoQuarto, DSC_CODQRT);
       Gerador.wCampo(tcInt, '#1', 'QtdHospedes        ' ,01, 09, 1, NFSe.Quartos.Items[i].QtdHospedes, DSC_TPAMB);
       Gerador.wCampo(tcDat, '#1', 'CheckIn            ' ,10, 10, 1, NFSe.Quartos.Items[i].CheckIn,     DSC_QTDHOSPDS);
       Gerador.wCampo(tcInt, '#1', 'QtdDiaria          ' ,01, 09, 1, NFSe.Quartos.Items[i].QtdDiarias,  DSC_QTDDIAR);
       Gerador.wCampo(tcDe2, '#1', 'ValorDiaria        ' ,01, 15, 1, NFSe.Quartos.Items[i].ValorDiaria, DSC_VDIAR);
      Gerador.wGrupo('/Quarto');
    end;

    Gerador.wGrupo('/Quartos');
  end;

  Gerador.wGrupo('/InfDeclaracaoPrestacaoServico');

  if FProvedor in [proTecnos] then
    Gerador.wGrupo('/tcDeclaracaoPrestacaoServico');
end;

constructor TNFSeW_ABRASFv2.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_ABRASFv2.ObterNomeArquivo: string;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

procedure TNFSeW_ABRASFv2.GerarEnderecoExterior;
begin
  Gerador.wGrupo('EnderecoExterior');
  Gerador.wCampo(tcInt, '#34', 'CodigoPais ', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, DSC_CPAIS);
  Gerador.wCampo(tcStr, '#39', 'EnderecoCompletoExterior', 001, 255, 0, NFSe.Tomador.Endereco.Endereco, DSC_XLGR);
  Gerador.wGrupo('/EnderecoExterior');
end;

function TNFSeW_ABRASFv2.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML  := '';
  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  if (FProvedor in [pro4R, proABase, proActcon, proAgili, proCoplan, proDigifred,
     proFiorilli, proGoiania, proGovDigital,
     proISSDigital, proLink3, proProdata, proPVH, proSaatri,
     proSisPMJP, proSystemPro, proTecnos, proVirtual, proVitoria,
     proNFSEBrasil, proNEAInformatica, proNotaInteligente, //proVersaTecnologia,
     proSH3, proSIAPNet, proAdm]) then
    FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '') then
    FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> '' then
    Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
  else
    Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  if (FProvedor in [proISSDigital, proNotaInteligente]) and (NFSe.NumeroLote <> '') then
    Atributo := ' Id="' + (NFSe.IdentificacaoRps.Numero) + '"';

  Gerador.Prefixo := '';

  if (FProvedor in [proNotaInteligente, proPronimv2, proTiplanv2, proiiBrasilv2,
                    proElotech]) then
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

    proSaatri: FNFSe.InfID.ID := 'RPS' + OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie + TipoRPSToStr(FNFSe.IdentificacaoRps.Tipo);

    proABase,
    proSiam,
    proGovDigital: FNFSe.InfID.ID := 'Rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proBethav2,
    proMegaSoft,
    proSIAPNet: FNFSe.InfID.ID := 'rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proNotaInteligente: FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proPronimv2,
    proSigep,
    proVirtual: FNFSe.InfID.ID := '';
  else
    FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                      FNFSe.IdentificacaoRps.Serie;
  end;

  if (Provedor = proPronimv2) then
    Gerador.Opcoes.SuprimirDecimais := True;

  Gerador.Prefixo := FPrefixo4;

  GerarXML_ABRASF_v2;

  Gerador.Prefixo := '';

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
