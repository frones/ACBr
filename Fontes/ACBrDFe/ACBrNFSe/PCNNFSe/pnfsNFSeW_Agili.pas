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

unit pnfsNFSeW_Agili;

interface

uses
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}

{$ENDIF}
  SysUtils, Classes, StrUtils,
  ACBrConsts,
  pcnAuxiliar, pcnConversao, pcnGerador,
  pnfsNFSe, pnfsNFSeW, pnfsConversao, pnfsConsts;

type
  { TNFSeW_Agili }

  TNFSeW_Agili = class(TNFSeWClass)
  protected

    function FormatarCnae(const Cnae: String): String;

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarRps;
    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarServicoValores;
    procedure GerarListaServicos;

    procedure GerarConstrucaoCivil;
    procedure GerarRegimeEspecialTributacao;
    procedure GerarResponsavelISSQN;
    procedure GerarExigibilidadeISSQN;

    procedure GerarXML_Agili;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil, MaskUtils;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do provedor Agili.                                                    }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

function TNFSeW_Agili.FormatarCnae(const Cnae: String): String;
begin
  Result := OnlyNumber(Cnae);

  if Length(Result) <> 7 then
    Exit;

  if VersaoNFSe = ve100 then
    Result := FormatMaskText('99.9.9-9.99;0', Result)
  else
    Result := FormatMaskText('9999-999;0', Result);
end;

function _RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): String;
begin
  result := EnumeradoToStr(t,
                           ['','-2','-4','-5','-6'],
                           [retNenhum, retEstimativa, retCooperativa, retMicroempresarioIndividual, retMicroempresarioEmpresaPP]);
end;

function SimNaoToStr(const t: TnfseSimNao): String;
begin
  result := EnumeradoToStr(t,
                           ['1','0'],
                           [snSim, snNao]);
end;

function _ExigibilidadeISSToStr(const t: TnfseExigibilidadeISS): String;
begin
  // -8 = Fixo
  result := EnumeradoToStr(t,
                           ['-1','-2','-3','-4','-5','-6','-7'],
                           [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                            exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo]);
end;

function _TipoRPSToStr(const t: TnfseTipoRPS): String;
begin
  result := EnumeradoToStr(t,
                           ['-2','-4','-5'],
                           [trRPS, trNFConjugada, trCupom]);
end;

function _ResponsavelRetencaoToStr(const T: TnfseResponsavelRetencao): String;
begin
  result := EnumeradoToStr(t,
                           ['-1', '-2', '-3'],
                           [ptTomador, rtIntermediario, rtPrestador]);
end;

{ TNFSeW_Agili }

procedure TNFSeW_Agili.GerarIdentificacaoRPS;
begin
  Gerador.wGrupoNFSe('IdentificacaoRps');
  Gerador.wCampoNFSe(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampoNFSe(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);

  if VersaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 01, 1, _TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS)
  else
    Gerador.wCampoNFSe(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS);

  Gerador.wGrupoNFSe('/IdentificacaoRps');
end;

procedure TNFSeW_Agili.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupoNFSe('RpsSubstituido');
    Gerador.wCampoNFSe(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampoNFSe(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampoNFSe(tcStr, '#12', 'Tipo  ', 01, 01, 1, _TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupoNFSe('/RpsSubstituido');
  end;
end;

procedure TNFSeW_Agili.GerarRps;
begin
  if FIdentificador = '' then
    Gerador.wGrupoNFSe('Rps')
  else
    Gerador.wGrupoNFSe('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');

  GerarIdentificacaoRPS;
  Gerador.wCampoNFSe(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

  Gerador.wGrupoNFSe('/Rps');
end;

procedure TNFSeW_Agili.GerarPrestador;
begin
  Gerador.wGrupoNFSe('IdentificacaoPrestador');
  Gerador.wCampoNFSe(tcStr, '', 'ChaveDigital', 32, 32, 1, NFSe.Prestador.ChaveAcesso, '');
  Gerador.wGrupoNFSe('CpfCnpj');

  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampoNFSe(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), '')
  else
    Gerador.wCampoNFSe(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');

  Gerador.wGrupoNFSe('/CpfCnpj');
  Gerador.wCampoNFSe(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wGrupoNFSe('/IdentificacaoPrestador');
end;

procedure TNFSeW_Agili.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    Gerador.wGrupoNFSe('DadosTomador');

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

    if NFSe.Tomador.Endereco.UF = 'EX' then
    begin
      Gerador.wCampoNFSe(tcStr, '', 'LocalEndereco', 1, 1, 1, '2', '');
      Gerador.wGrupoNFSe('EnderecoExterior');
      Gerador.wCampoNFSe(tcStr, '', 'Descricao', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wGrupoNFSe('Pais');
      Gerador.wCampoNFSe(tcStr, '', 'CodigoPaisBacen', 04, 04, 1, NFSe.Tomador.Endereco.CodigoPais, '');
      Gerador.wCampoNFSe(tcStr, '', 'Descricao', 0, 300, 0, NFSe.Tomador.Endereco.xPais, '');
      Gerador.wGrupoNFSe('/Pais');
      Gerador.wGrupoNFSe('/EnderecoExterior');
    end
    else
    begin
      Gerador.wCampoNFSe(tcStr, '', 'LocalEndereco', 1, 1, 1, '1', '');
      Gerador.wGrupoNFSe('Endereco');

      if VersaoNFSe = ve100 then
      begin
        Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouro', 001, 120, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
        Gerador.wCampoNFSe(tcStr, '#39', 'Logradouro ', 001, 120, 1, NFSe.Tomador.Endereco.Endereco, '');
        Gerador.wCampoNFSe(tcStr, '#40', 'Numero     ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, '');
        Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 300, 0, NFSe.Tomador.Endereco.Complemento, '');
        Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 120, 0, NFSe.Tomador.Endereco.Bairro, '');

        Gerador.wGrupoNFSe('Municipio');
        Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipioIBGE', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
        Gerador.wCampoNFSe(tcStr, '', 'Descricao', 1, 300, 0, NFSe.Tomador.Endereco.xMunicipio, '');
        Gerador.wCampoNFSe(tcStr, '#44', 'Uf', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');
        Gerador.wGrupoNFSe('/Municipio');

        Gerador.wGrupoNFSe('Pais');
        Gerador.wCampoNFSe(tcStr, '', 'CodigoPaisBacen', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');
        Gerador.wCampoNFSe(tcStr, '', 'Descricao', 0, 300, 0, NFSe.Tomador.Endereco.xPais, '');
        Gerador.wGrupoNFSe('/Pais');
      end
      else
      begin
        Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouro', 001, 030, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
        Gerador.wCampoNFSe(tcStr, '#39', 'Logradouro ', 001, 120, 1, NFSe.Tomador.Endereco.Endereco, '');
        Gerador.wCampoNFSe(tcStr, '#40', 'Numero     ', 001, 015, 1, NFSe.Tomador.Endereco.Numero, '');
        Gerador.wCampoNFSe(tcStr, '#41', 'Complemento', 001, 030, 0, NFSe.Tomador.Endereco.Complemento, '');
        Gerador.wCampoNFSe(tcStr, '#42', 'Bairro     ', 001, 030, 0, NFSe.Tomador.Endereco.Bairro, '');

        Gerador.wCampoNFSe(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
        Gerador.wCampoNFSe(tcStr, '#44', 'Uf', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');
        Gerador.wCampoNFSe(tcStr, '', 'CodigoPais', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');
      end;

      Gerador.wCampoNFSe(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
      Gerador.wGrupoNFSe('/Endereco');
    end;

    if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
    begin
      Gerador.wGrupoNFSe('Contato');

      if VersaoNFSe = ve100 then
      begin
        Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 14, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
        Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 300, 0, NFSe.Tomador.Contato.Email, '');
      end
      else
      begin
        Gerador.wCampoNFSe(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
        Gerador.wCampoNFSe(tcStr, '#47', 'Email   ', 01, 120, 0, NFSe.Tomador.Contato.Email, '');
      end;

      Gerador.wGrupoNFSe('/Contato');

      if VersaoNFSe = ve100 then
        Gerador.wCampoNFSe(tcStr, '', 'InscricaoEstadual', 01, 20, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '')
      else
        Gerador.wCampoNFSe(tcStr, '', 'InscricaoEstadual', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');

    end;

    Gerador.wGrupoNFSe('/DadosTomador');
  end
  else
  begin
    Gerador.wCampoNFSe(tcStr, '#', 'Tomador', 0, 1, 1, '', '');
  end;
end;

procedure TNFSeW_Agili.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial <> '') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupoNFSe('Intermediario');
    Gerador.wGrupoNFSe('IdentificacaoIntermediario');
    Gerador.wGrupoNFSe('CpfCnpj');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampoNFSe(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '')
    else
     Gerador.wCampoNFSe(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

    Gerador.wGrupoNFSe('/CpfCnpj');
    Gerador.wCampoNFSe(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupoNFSe('/IdentificacaoIntermediario');
    Gerador.wCampoNFSe(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wGrupoNFSe('/Intermediario');
  end;
end;

procedure TNFSeW_Agili.GerarServicoValores;
begin
  Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServicos ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDescontos', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampoNFSe(tcDe2, '#15', 'ValorPis      ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '#16', 'ValorCofins   ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '#17', 'ValorInss     ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');

  if VersaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIrrf', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '')
  else
    Gerador.wCampoNFSe(tcDe2, '#18', 'ValorIr  ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');

  Gerador.wCampoNFSe(tcDe2, '#19', 'ValorCsll           ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampoNFSe(tcDe2, '#23', 'ValorOutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampoNFSe(tcDe2, '#24', 'ValorBaseCalculoISSQN', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');
    Gerador.wCampoNFSe(tcDe2, '#25', 'AliquotaISSQN        ', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorISSQNCalculado  ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');

    if NFSe.OptanteSimplesNacional = snNao then
      Gerador.wCampoNFSe(tcDe2, '#21', 'ValorISSQNRecolher', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  end
  else
  begin
    Gerador.wCampoNFSe(tcDe2, '#24', 'ValorBaseCalculoIss  ', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');
    Gerador.wCampoNFSe(tcDe2, '#25', 'Aliquota             ', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
    Gerador.wCampoNFSe(tcDe2, '#21', 'ValorIss             ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  end;

  Gerador.wCampoNFSe(tcDe2, '', 'ValorDeducaoConstCivil', 01, 15, 1, 0, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorLiquido          ', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampoNFSe(tcStr, '', 'Observacao            ', 01, 4000, 0, NFSe.OutrasInformacoes, '');
    Gerador.wCampoNFSe(tcStr, '', 'Complemento           ', 01, 3000, 0, '', '');  // Não enviar TAG
  end;

end;

procedure TNFSeW_Agili.GerarListaServicos;
var
  i: Integer;
  codLCServ: string;
begin
  Gerador.wGrupoNFSe('ListaServico');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    codLCServ := IntToStr(StrToIntDef(OnlyNumber(NFSe.Servico.ItemServico[i].CodLCServ), 0));

    if Length(codLCServ) > 2 then
      Insert('.', codLCServ, Length(codLCServ) - 2 + 1);

    Gerador.wGrupoNFSe('DadosServico');
    Gerador.wCampoNFSe(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                    StringReplace( NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');

    if VersaoNFSe = ve100 then
    begin
      case StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0) of
        // Ipiranga do Norte - MT
        5104526:
          begin
            Gerador.wCampoNFSe(tcStr, '#29', 'ItemLei116', 01, 140, 1, codLCServ, '');
            Gerador.wCampoNFSe(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
          end;
        // Juina - MT
        5105150:
          Gerador.wCampoNFSe(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
      else
        begin
          Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, FormatarCnae(NFSe.Servico.CodigoCnae), '');
          Gerador.wCampoNFSe(tcStr, '#29', 'ItemLei116', 01, 140, 1, codLCServ, '');
          Gerador.wCampoNFSe(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
        end;
      end;
    end
    else
    begin
      Gerador.wCampoNFSe(tcStr, '#29', 'ItemLei116', 01, 015, 0, codLCServ, '');
      Gerador.wCampoNFSe(tcDe2, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
    end;

    Gerador.wCampoNFSe(tcDe2, '#13', 'ValorServico ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorUnitario, '');
    Gerador.wCampoNFSe(tcDe2, '#14', 'ValorDesconto', 01, 15, 1, NFSe.Servico.ItemServico[i].DescontoIncondicionado, '');

    Gerador.wGrupoNFSe('/DadosServico');
  end;

  Gerador.wGrupoNFSe('/ListaServico');
end;

procedure TNFSeW_Agili.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Gerador.wGrupoNFSe('ConstrucaoCivil');
    Gerador.wCampoNFSe(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, '');
    Gerador.wCampoNFSe(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, '');
    Gerador.wGrupoNFSe('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_Agili.GerarRegimeEspecialTributacao;
begin
  if VersaoNFSe = ve100 then
  begin
    if _RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao) <> '' then
    begin
      Gerador.wGrupoNFSe('RegimeEspecialTributacao');
      Gerador.wCampoNFSe(tcStr, '', 'Codigo   ', 01, 01, 1, _RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');
      Gerador.wCampoNFSe(tcStr, '', 'Descricao', 01, 300, 0, '', '');
      Gerador.wGrupoNFSe('/RegimeEspecialTributacao');
    end;
  end
  else
  begin
    Gerador.wCampoNFSe(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 1, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '')
  end;
end;

procedure TNFSeW_Agili.GerarResponsavelISSQN;
begin
  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupoNFSe('ResponsavelISSQN');
    Gerador.wCampoNFSe(tcStr, '', 'Codigo   ', 01, 01, 0, _ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), ''); 
    Gerador.wCampoNFSe(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wGrupoNFSe('/ResponsavelISSQN');
  end
  else
  begin
    if (NFSe.Servico.Valores.IssRetido <> stNormal) then
      Gerador.wCampoNFSe(tcStr, '', 'ResponsavelRetencao', 01, 01, 0, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');
  end;
end;

procedure TNFSeW_Agili.GerarExigibilidadeISSQN;
begin
  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupoNFSe('ExigibilidadeISSQN');
    Gerador.wCampoNFSe(tcStr, '', 'Codigo   ', 01, 01, 1, _ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '');
    Gerador.wCampoNFSe(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wGrupoNFSe('/ExigibilidadeISSQN');
  end
  else
  begin
    Gerador.wCampoNFSe(tcStr, '', 'ExigibilidadeIss', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '')
  end;
end;

procedure TNFSeW_Agili.GerarXML_Agili;
var
 LTagAtividadeEconomica: String;
begin
  Gerador.wGrupoNFSe('InfDeclaracaoPrestacaoServico');

  if VersaoNFSe = ve100 then
  begin
    GerarPrestador;

    if OnlyNumber(NFSe.NfseSubstituida) <> '' then
      Gerador.wCampoNFSe(tcStr, '', 'NfseSubstituida', 01, 15, 1, OnlyNumber(NFSe.NfseSubstituida), '');

    GerarRps;
  end
  else begin
    GerarRps;
    GerarPrestador;
  end;

  GerarTomador;
  GerarIntermediarioServico;
  GerarConstrucaoCivil;
  GerarRegimeEspecialTributacao;

  Gerador.wCampoNFSe(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampoNFSe(tcStr, '', 'OptanteMEISimei', 01, 01, 1, SimNaoToStr(snNao), '');

    if NFSe.Servico.Valores.IssRetido = stRetencao then
      Gerador.wCampoNFSe(tcStr, '', 'ISSQNRetido', 01, 01, 1, SimNaoToStr(snSim), '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'ISSQNRetido', 01, 01, 1, SimNaoToStr(snNao), '');
  end
  else
  begin
    if NFSe.Servico.Valores.IssRetido = stRetencao then
      Gerador.wCampoNFSe(tcStr, '', 'IssRetido', 01, 01, 1, SimNaoToStr(snSim), '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'IssRetido', 01, 01, 1, SimNaoToStr(snNao), '');
  end;

  GerarResponsavelISSQN;

  case StrToInt(NFSe.PrestadorServico.Endereco.CodigoMunicipio) of
    5104526: LTagAtividadeEconomica := 'CodigoCnaeAtividadeEconomica';
    5105150,
    5107305: LTagAtividadeEconomica := 'ItemLei116AtividadeEconomica';
  else
    LTagAtividadeEconomica := 'CodigoAtividadeEconomica';
  end;

  if NaoEstaVazio(NFSe.Servico.CodigoTributacaoMunicipio) then
    Gerador.wCampoNFSe(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, NFSe.Servico.CodigoTributacaoMunicipio, '')
  else
  begin
    if VersaoNFSe = ve100 then
      Gerador.wCampoNFSe(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, FormatarCnae(NFSe.Servico.CodigoCnae), '')
    else
      Gerador.wCampoNFSe(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, NFSe.Servico.CodigoCnae, '');
  end;

  if VersaoNFSe = ve200 then
    Gerador.wCampoNFSe(tcStr, '#30', 'CodigoCnae', 01, 15, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');

  GerarExigibilidadeISSQN;
  Gerador.wCampoNFSe(tcStr, '', 'BeneficioProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupo('MunicipioIncidencia');
    Gerador.wCampoNFSe(tcInt, '#36', 'CodigoMunicipioIBGE', 07, 07, 1, NFSe.Servico.MunicipioIncidencia, '');
    Gerador.wCampoNFSe(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wCampoNFSe(tcStr, '', 'Uf', 02, 02, 0, '', '');
    Gerador.wGrupo('/MunicipioIncidencia');
  end
  else begin
    Gerador.wCampoNFSe(tcStr, '', 'MunicipioIncidencia', 7, 7, 0, NFSe.Servico.MunicipioIncidencia, '')
  end;

  GerarServicoValores;
  GerarListaServicos;

  if VersaoNFSe = ve100 then
    Gerador.wCampoNFSe(tcStr, '', 'Versao', 04, 04, 1, '1.00', '');

  Gerador.wGrupoNFSe('/InfDeclaracaoPrestacaoServico');
end;

constructor TNFSeW_Agili.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);
end;

function TNFSeW_Agili.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_Agili.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '') then
    FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> '' then
    Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
  else
    Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  Gerador.wGrupo('Rps' + Atributo);

  FNFSe.InfID.ID := OnlyNumber(FNFSe.IdentificacaoRps.Numero) + FNFSe.IdentificacaoRps.Serie;

  GerarXML_Agili;

  Gerador.wGrupo('/Rps');

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
