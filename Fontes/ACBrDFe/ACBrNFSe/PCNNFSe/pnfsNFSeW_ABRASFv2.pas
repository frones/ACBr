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
  pnfsNFSeW,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsConversao;

type
  { TNFSeW_ABRASFv2 }

  TNFSeW_ABRASFv2 = class(TNFSeWClass)
  private
  protected

    procedure GerarIdentificacaoRPS;
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

    function ObterNomeArquivo: String; override;
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
  Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
  Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, '');
  Gerador.wCampoNFSe(tcStr, '#3', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), '');
  Gerador.wGrupoNFSe('/IdentificacaoRps');
end;

procedure TNFSeW_ABRASFv2.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupoNFSe('RpsSubstituido');
    Gerador.wCampoNFSe(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), '');
    Gerador.wCampoNFSe(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, '');
    Gerador.wCampoNFSe(tcStr, '#12', 'Tipo  ', 01, 01, 1, TipoRPSToStr(NFSe.RpsSubstituido.Tipo), '');
    Gerador.wGrupoNFSe('/RpsSubstituido');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarPrestador;
begin
  Gerador.wGrupoNFSe('Prestador');

   Gerador.wGrupoNFSe('CpfCnpj');
   if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
     Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), '')
   else
     Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');
   Gerador.wGrupoNFSe('/CpfCnpj');
(*
  if ((VersaoNFSe = ve100) and not (FProvedor in [proISSNet, proActcon])) or
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
*)
  if (FProvedor = proTecnos) then
    Gerador.wCampoNFSe(tcStr, '#35', 'RazaoSocial', 01, 15, 1, NFSe.PrestadorServico.RazaoSocial, '');

  Gerador.wCampoNFSe(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, '');

  if (FProvedor in [proISSDigital, proAgili]) then
  begin
    Gerador.wCampoNFSe(tcStr, '#36', 'Senha       ', 01, 255, 1, NFSe.Prestador.Senha, '');
    Gerador.wCampoNFSe(tcStr, '#37', 'FraseSecreta', 01, 255, 1, NFSe.Prestador.FraseSecreta, '');
  end;

  Gerador.wGrupoNFSe('/Prestador');
end;

procedure TNFSeW_ABRASFv2.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    if FProvedor in [proActcon, proVersaTecnologia] then
      Gerador.wGrupoNFSe('TomadorServico')
    else
      Gerador.wGrupoNFSe('Tomador');

    if NFSe.Tomador.Endereco.UF <> 'EX' then
    begin
      Gerador.wGrupoNFSe('IdentificacaoTomador');

      Gerador.wGrupoNFSe('CpfCnpj');
      if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
        Gerador.wCampoNFSe(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '')
      else
        Gerador.wCampoNFSe(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
      Gerador.wGrupoNFSe('/CpfCnpj');

      Gerador.wCampoNFSe(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

      Gerador.wGrupoNFSe('/IdentificacaoTomador');
    end;

    Gerador.wCampoNFSe(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, '');

    Gerador.wGrupoNFSe('Endereco');
    Gerador.wCampoNFSe(tcStr, '#39', 'Endereco', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
    Gerador.wCampoNFSe(tcStr, '#40', 'Numero  ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, '');

    if FProvedor <> proNFSeBrasil then
      Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 0, NFSe.Tomador.Endereco.Complemento, '')
    else
      Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 060, 1, NFSe.Tomador.Endereco.Complemento, '');

    Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 060, 0, NFSe.Tomador.Endereco.Bairro, '');

    Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
    Gerador.wCampoNFSe(tcStr, '#44', 'Uf             ', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');

    if FProvedor <> proNFSeBrasil then
      Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais ', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');

    Gerador.wCampoNFSe(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
    Gerador.wGrupoNFSe('/Endereco');

    case FProvedor of
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

    if FProvedor in [proActcon, proVersaTecnologia] then
      Gerador.wGrupoNFSe('/TomadorServico')
    else
      Gerador.wGrupoNFSe('/Tomador');
  end
  else begin
    // Gera a TAG vazia quando nenhum dado do tomador for informado.
    if FProvedor in [proActcon, proVersaTecnologia] then
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
        Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, '', '');
      end
      else begin
        Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, '', '');
        Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
      end;
    end
    else begin
      if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
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

    if FProvedor in [proVirtual, proVersaTecnologia] then
      Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 1, NFSe.IntermediarioServico.RazaoSocial, '')
    else
      Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');

    Gerador.wGrupoNFSe('/Intermediario');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarServicoValores;
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

   proABase,
   proActcon,
   proPronimv2,
   proVirtual,
   proVersaTecnologia: begin
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
           proNEAInformatica,
           proProdata,
           proSystemPro,
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
                            proTecnos, proNEAInformatica] then
             Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '')
           else
             Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
         end;
       end;
  end;

  if FProvedor in [proActcon, proAgili, proTecnos] then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');

  if FProvedor in [proABase, proActcon, proPronimv2, proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss', 01, 15, 1, NFSe.Servico.Valores.ValorIss, '');

  case FProvedor of
    proCoplan,
    proDigifred,
    proEReceita,
    proFiorilli,
    proNEAInformatica,
    proNotaInteligente,
    proPronimv2,
    proSisPMJP    : Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');

    proTecnos,
    proABase,
    proProdata: Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

    pro4R,
    profintelISS,
    proGovDigital,
    proISSDigital,
    proISSe,
    proLink3,
    proSaatri,
    proSystemPro,
    proVirtual,
    proVersaTecnologia: Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 1, NFSe.Servico.Valores.Aliquota, '');

    proGoiania: if NFSe.OptanteSimplesNacional = snSim then
                  Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');

  else
    Gerador.wCampoNFSe(tcDe4, '#25', 'Aliquota', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
  end;

  if FProvedor in [profintelISS] then
    Gerador.wCampoNFSe(tcDe2, '#24', 'BaseCalculo', 01, 15, 1, NFSe.Servico.Valores.BaseCalculo, '');

  case FProvedor of
   proABase,
   proActcon,
   proPronimv2,
   proTecnos,
   proVirtual,
   proVersaTecnologia: begin
                Gerador.wCampoNFSe(tcDe2, '#27', 'DescontoIncondicionado', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
                Gerador.wCampoNFSe(tcDe2, '#28', 'DescontoCondicionado  ', 01, 15, 1, NFSe.Servico.Valores.DescontoCondicionado, '');
              end;

   pro4R,
   proFiorilli,
   proGoiania,
   proISSDigital,
   proISSe,
   proSystemPro,
   proPVH,
   proSaatri,
   proCoplan,
   proLink3,
   proNEAInformatica,
   proNotaInteligente,
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

  if ((NFSe.Servico.Valores.IssRetido <> stNormal) and (FProvedor <> proGoiania)) or
     (FProvedor in [proProdata, proVirtual, proVersaTecnologia]) then
    Gerador.wCampoNFSe(tcStr, '#21', 'ResponsavelRetencao', 01, 01, 1, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');

  if FProvedor <> proGoiania then
  begin
    case FProvedor of
      proVirtual: Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, OnlyNumber(NFSe.Servico.ItemListaServico), '');

      proNotaInteligente: Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 1, NFSe.Servico.ItemListaServico, '');

      proVitoria: begin
                    if copy(NFSe.Servico.ItemListaServico, 1, 1) = '0'  then
                      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, copy(NFSe.Servico.ItemListaServico, 2, 4), '')
                    else
                      Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, '');
                  end;
      else
        Gerador.wCampoNFSe(tcStr, '#29', 'ItemListaServico', 01, 05, 0, NFSe.Servico.ItemListaServico, '');
    end;

    if FProvedor = proVersaTecnologia then
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoCnae), '')
    else
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 07, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');
  end;

  if FProvedor in [proGoiania, proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 1, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '')
  else
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 20, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');

  Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                     StringReplace( FNFSe.Servico.Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');

  Gerador.wCampoNFSe(tcStr, '#33', 'CodigoMunicipio', 01, 07, 1, OnlyNumber(NFSe.Servico.CodigoMunicipio), '');

  if FProvedor in [proVirtual, proVersaTecnologia] then
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 1, NFSe.Servico.CodigoPais, '')
  else
    Gerador.wCampoNFSe(tcInt, '#34', 'CodigoPais', 04, 04, 0, NFSe.Servico.CodigoPais, '');

  if FProvedor <> proGoiania then
  begin
    Gerador.wCampoNFSe(tcStr, '#35', 'ExigibilidadeISS', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '');

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
    if FProvedor = proVersaTecnologia then
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae               ', 01, 0007, 1, OnlyNumber(NFSe.Servico.CodigoCnae), '')
    else
      Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae               ', 01, 0007, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');
    Gerador.wCampoNFSe(tcStr, '#31', 'CodigoTributacaoMunicipio', 01, 0020, 0, OnlyNumber(NFSe.Servico.CodigoTributacaoMunicipio), '');
    Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                    StringReplace( NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');
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

procedure TNFSeW_ABRASFv2.GerarValoresServico;
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

procedure TNFSeW_ABRASFv2.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Gerador.wGrupoNFSe('ConstrucaoCivil');
    Gerador.wCampoNFSe(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, '');
    Gerador.wCampoNFSe(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, '');
    Gerador.wGrupoNFSe('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_ABRASFv2.GerarCondicaoPagamento;
//var
//  i: Integer;
begin
(*
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
*)
end;

procedure TNFSeW_ABRASFv2.GerarXML_ABRASF_v2;
begin
  case FProvedor of
   proABase, proDigifred, proEReceita, proFiorilli, proGovDigital, proISSDigital,
   proISSe, proMitra, proNEAInformatica, proNotaInteligente, proPVH,
   proSisPMJP: begin
                 Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
                 Gerador.wGrupoNFSe('Rps');
               end;

   proPronimv2: begin
                  Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');
                  Gerador.wGrupoNFSe('Rps');
                end;

   proSiam: begin
              Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="Declaracao_' + OnlyNumber(NFSe.Prestador.Cnpj) + '"');
              Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
            end;

   proSystemPro: begin
                   Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"');
                 end;

   proTecnos: begin
                Gerador.WGrupoNFSe('tcDeclaracaoPrestacaoServico');
                Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '="' + NFSe.InfID.ID + '"' + ' xmlns="http://www.abrasf.org.br/nfse.xsd"');
                Gerador.wGrupoNFSe('Rps');
              end;

   proVirtual: begin
                 Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico ' + FIdentificador + '=""');
                 Gerador.wGrupoNFSe('Rps ' + FIdentificador + '=""');
               end;

  else begin
         Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');
         if FIdentificador = '' then
           Gerador.wGrupoNFSe('Rps')
         else
           Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');
       end;
  end;

  GerarIdentificacaoRPS;

  if not (FProvedor in [proSystemPro]) then
  begin
    case FProvedor of
      proABase, proActcon, proAgili, proCoplan, proEReceita, proFiorilli,
      proGovDigital, proISSDigital, proISSe, proMitra, proNEAInformatica,
      proNotaInteligente, proProdata, proPronimv2, proPVH, proSaatri, proSisPMJP,
      proVirtual, proVersaTecnologia,
      proVitoria: Gerador.wCampoNFSe(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

    else
      Gerador.wCampoNFSe(tcDatHor, '#4', 'DataEmissao', 19, 19, 1, NFSe.DataEmissao, DSC_DEMI);
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
        proActcon, proISSDigital, proMitra, proPVH, proSisPMJP, proVirtual,
        proSystemPro, proNEAInformatica,
        proEReceita: Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proABase,
        proGovDigital,
        proNotaInteligente,
        proPronimv2,
        proVersaTecnologia : Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.Competencia, DSC_DEMI);

        proTecnos:  Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.Competencia, DSC_DEMI);

      else
        Gerador.wCampoNFSe(tcStr, '#4', 'Competencia', 19, 19, 1, NFSe.Competencia, DSC_DEMI);
      end;
    end
    else begin
      if FProvedor in [proActcon, proCoplan, proEReceita, proFiorilli,
                       proGovDigital, proISSDigital, proISSe, proMitra,
                       proNEAInformatica, proNotaInteligente, proPronimv2,
                       proProdata, proPVH, proSaatri, proSisPMJP, proSystemPro,
                       proVirtual, proVitoria, proVersaTecnologia] then
        Gerador.wCampoNFSe(tcDat, '#4', 'Competencia', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI)
      else begin
        if not (FProvedor in [proGoiania]) then
          Gerador.wCampoNFSe(tcDatHor, '#4', 'Competencia', 19, 19, 0, NFSe.DataEmissao, DSC_DEMI);
      end;
    end;

    if FProvedor in [proTecnos] then
      Gerador.wCampoNFSe(tcStr, '#4', 'IdCidade', 7, 7, 1, NFSe.PrestadorServico.Endereco.CodigoMunicipio);

    GerarServicoValores;
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

constructor TNFSeW_ABRASFv2.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_ABRASFv2.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_ABRASFv2.GerarXml: Boolean;
var
  Gerar: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  if (FProvedor in [pro4R, proABase, proActcon, proAgili, proCoplan, proDigifred,
                    profintelISS, proFiorilli, proGoiania, proGovDigital,
                    proISSDigital, proLink3, proProdata, proPVH, proSaatri,
                    proSisPMJP, proSystemPro, proTecnos, proVirtual, proVitoria,
                    proNFSEBrasil, proNEAInformatica, proNotaInteligente,
                    proVersaTecnologia]) then
    FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  if (FProvedor in [proISSDigital, proNotaInteligente]) and (NFSe.NumeroLote <> '')
    then Atributo := ' Id="' +  (NFSe.IdentificacaoRps.Numero) + '"';

  if (FProvedor in [proBetha, proNotaInteligente, proPronimv2]) then
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
    proMitra:     NFSe.InfID.ID := 'rps' +
                                   OnlyNumber(FNFSe.IdentificacaoRps.Numero) +
                                   FNFSe.IdentificacaoRps.Serie;

    proISSDigital: FNFSe.InfID.ID := 'rps' + ChaveAcesso(FNFSe.Prestador.cUF,
                                                 FNFSe.DataEmissao,
                                                 OnlyNumber(FNFSe.Prestador.Cnpj),
                                                 0, // Serie
                                                 StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)),
                                                 StrToInt(OnlyNumber(FNFSe.IdentificacaoRps.Numero)));

    proTecnos: FNFSe.InfID.ID := '1' + //Fixo - Lote Sincrono
//                                 FormatDateTime('yyyy', FNFSe.DataEmissao) +
                                 OnlyNumber(FNFSe.Prestador.Cnpj) +
                                 IntToStrZero(StrToIntDef(FNFSe.IdentificacaoRps.Numero, 1), 16);

    proABase,
    proSiam,
    proGovDigital: FNFSe.InfID.ID := 'Rps' + OnlyNumber(FNFSe.IdentificacaoRps.Numero);

    proNotaInteligente : FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero);

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
