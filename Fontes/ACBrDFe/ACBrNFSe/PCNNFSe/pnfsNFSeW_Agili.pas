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
  LResources, 
  Controls, 
{$ELSE}

{$ENDIF}
  SysUtils, 
  Classes, 
  StrUtils,
  ACBrConsts,
  pcnAuxiliar, 
  pcnConversao, 
  pcnGerador, 
  pcnConsts,
  pnfsNFSe, 
  pnfsNFSeW, 
  pnfsConversao, 
  pnfsConsts;

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
  ACBrUtil.Strings,
  ACBrUtil.Base,
  MaskUtils;

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
                           ['-1','-2','-3','-4','-5','-6','-7', '-8'],
                           [exiExigivel, exiNaoIncidencia, exiIsencao, exiExportacao, exiImunidade,
                            exiSuspensaDecisaoJudicial, exiSuspensaProcessoAdministrativo, exiISSFixo]);
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
  Gerador.wGrupo('IdentificacaoRps');
  Gerador.wCampo(tcStr, '#1', 'Numero', 01, 15, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS);
  Gerador.wCampo(tcStr, '#2', 'Serie ', 01, 05, 1, NFSe.IdentificacaoRps.Serie, DSC_SERIERPS);

  if VersaoNFSe = ve100 then
    Gerador.wCampo(tcStr, '#3', 'Tipo', 01, 01, 1, _TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS)
  else
    Gerador.wCampo(tcStr, '#3', 'Tipo', 01, 01, 1, TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS);

  Gerador.wGrupo('/IdentificacaoRps');
end;

procedure TNFSeW_Agili.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Gerador.wGrupo('RpsSubstituido');
    Gerador.wCampo(tcStr, '#10', 'Numero', 01, 15, 1, OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB);
    Gerador.wCampo(tcStr, '#11', 'Serie ', 01, 05, 1, NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB);
    Gerador.wCampo(tcStr, '#12', 'Tipo  ', 01, 01, 1, _TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB);
    Gerador.wGrupo('/RpsSubstituido');
  end;
end;

procedure TNFSeW_Agili.GerarRps;
begin
  if FIdentificador = '' then
    Gerador.wGrupo('Rps')
  else
    Gerador.wGrupo('Rps ' + FIdentificador + '="rps' + NFSe.InfID.ID + '"');

  GerarIdentificacaoRPS;
  Gerador.wCampo(tcDat, '#4', 'DataEmissao', 10, 10, 1, NFSe.DataEmissao, DSC_DEMI);

  Gerador.wGrupo('/Rps');
end;

procedure TNFSeW_Agili.GerarPrestador;
begin
  Gerador.wGrupo('IdentificacaoPrestador');
  Gerador.wCampo(tcStr, '', 'ChaveDigital', 32, 32, 1, NFSe.Prestador.ChaveAcesso, '');
  Gerador.wGrupo('CpfCnpj');

  if length(OnlyNumber(NFSe.Prestador.Cnpj)) <= 11 then
    Gerador.wCampo(tcStr, '#34', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Prestador.Cnpj), '')
  else
    Gerador.wCampo(tcStr, '#34', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Prestador.Cnpj), '');

  Gerador.wGrupo('/CpfCnpj');
  Gerador.wCampo(tcStr, '#35', 'InscricaoMunicipal', 01, 15, 0, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wGrupo('/IdentificacaoPrestador');
end;

procedure TNFSeW_Agili.GerarTomador;
begin
  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    Gerador.wGrupo('DadosTomador');

    if NFSe.Tomador.Endereco.UF <> 'EX' then
    begin
      Gerador.wGrupo('IdentificacaoTomador');

      Gerador.wGrupo('CpfCnpj');

      if Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) <= 11 then
        Gerador.wCampo(tcStr, '#36', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '')
      else
        Gerador.wCampo(tcStr, '#36', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');

      Gerador.wGrupo('/CpfCnpj');

      Gerador.wCampo(tcStr, '#37', 'InscricaoMunicipal', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');

      Gerador.wGrupo('/IdentificacaoTomador');
    end;

    Gerador.wCampo(tcStr, '#38', 'RazaoSocial', 001, 115, 0, NFSe.Tomador.RazaoSocial, '');

    if NFSe.Tomador.Endereco.UF = 'EX' then
    begin
      Gerador.wCampo(tcStr, '', 'LocalEndereco', 1, 1, 1, '2', '');
      Gerador.wGrupo('EnderecoExterior');
      Gerador.wCampo(tcStr, '', 'Descricao', 001, 125, 0, NFSe.Tomador.Endereco.Endereco, '');
      Gerador.wGrupo('Pais');
      Gerador.wCampo(tcStr, '', 'CodigoPaisBacen', 04, 04, 1, NFSe.Tomador.Endereco.CodigoPais, '');
      Gerador.wCampo(tcStr, '', 'Descricao', 0, 300, 0, NFSe.Tomador.Endereco.xPais, '');
      Gerador.wGrupo('/Pais');
      Gerador.wGrupo('/EnderecoExterior');
    end
    else
    begin
      Gerador.wCampo(tcStr, '', 'LocalEndereco', 1, 1, 1, '1', '');
      Gerador.wGrupo('Endereco');

      if VersaoNFSe = ve100 then
      begin
        Gerador.wCampo(tcStr, '', 'TipoLogradouro', 001, 120, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
        Gerador.wCampo(tcStr, '#39', 'Logradouro ', 001, 120, 1, NFSe.Tomador.Endereco.Endereco, '');
        Gerador.wCampo(tcStr, '#40', 'Numero     ', 001, 010, 0, NFSe.Tomador.Endereco.Numero, '');
        Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 300, 0, NFSe.Tomador.Endereco.Complemento, '');
        Gerador.wCampo(tcStr, '#42', 'Bairro     ', 001, 120, 0, NFSe.Tomador.Endereco.Bairro, '');

        Gerador.wGrupo('Municipio');
        Gerador.wCampo(tcStr, '#43', 'CodigoMunicipioIBGE', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
        Gerador.wCampo(tcStr, '', 'Descricao', 1, 300, 0, NFSe.Tomador.Endereco.xMunicipio, '');
        Gerador.wCampo(tcStr, '#44', 'Uf', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');
        Gerador.wGrupo('/Municipio');

        Gerador.wGrupo('Pais');
        Gerador.wCampo(tcStr, '', 'CodigoPaisBacen', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');
        Gerador.wCampo(tcStr, '', 'Descricao', 0, 300, 0, NFSe.Tomador.Endereco.xPais, '');
        Gerador.wGrupo('/Pais');
      end
      else
      begin
        Gerador.wCampo(tcStr, '', 'TipoLogradouro', 001, 030, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
        Gerador.wCampo(tcStr, '#39', 'Logradouro ', 001, 120, 1, NFSe.Tomador.Endereco.Endereco, '');
        Gerador.wCampo(tcStr, '#40', 'Numero     ', 001, 015, 1, NFSe.Tomador.Endereco.Numero, '');
        Gerador.wCampo(tcStr, '#41', 'Complemento', 001, 030, 0, NFSe.Tomador.Endereco.Complemento, '');
        Gerador.wCampo(tcStr, '#42', 'Bairro     ', 001, 030, 0, NFSe.Tomador.Endereco.Bairro, '');

        Gerador.wCampo(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0, OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), '');
        Gerador.wCampo(tcStr, '#44', 'Uf', 2, 2, 0, NFSe.Tomador.Endereco.UF, '');
        Gerador.wCampo(tcStr, '', 'CodigoPais', 04, 04, 0, NFSe.Tomador.Endereco.CodigoPais, '');
      end;

      Gerador.wCampo(tcStr, '#45', 'Cep', 008, 008, 0, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
      Gerador.wGrupo('/Endereco');
    end;

    if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
    begin
      Gerador.wGrupo('Contato');

      if VersaoNFSe = ve100 then
      begin
        Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 14, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
        Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 300, 0, NFSe.Tomador.Contato.Email, '');
      end
      else
      begin
        Gerador.wCampo(tcStr, '#46', 'Telefone', 01, 11, 0, OnlyNumber(NFSe.Tomador.Contato.Telefone), '');
        Gerador.wCampo(tcStr, '#47', 'Email   ', 01, 120, 0, NFSe.Tomador.Contato.Email, '');
      end;

      Gerador.wGrupo('/Contato');

      if VersaoNFSe = ve100 then
        Gerador.wCampo(tcStr, '', 'InscricaoEstadual', 01, 20, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '')
      else
        Gerador.wCampo(tcStr, '', 'InscricaoEstadual', 01, 15, 0, NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, '');

    end;

    Gerador.wGrupo('/DadosTomador');
  end
  else
  begin
    Gerador.wCampo(tcStr, '#', 'Tomador', 0, 1, 1, '', '');
  end;
end;

procedure TNFSeW_Agili.GerarIntermediarioServico;
begin
  if (NFSe.IntermediarioServico.RazaoSocial <> '') or
     (NFSe.IntermediarioServico.CpfCnpj <> '') then
  begin
    Gerador.wGrupo('Intermediario');
    Gerador.wGrupo('IdentificacaoIntermediario');
    Gerador.wGrupo('CpfCnpj');

    if Length(OnlyNumber(NFSe.IntermediarioServico.CpfCnpj)) <= 11 then
      Gerador.wCampo(tcStr, '#49', 'Cpf ', 11, 11, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '')
    else
     Gerador.wCampo(tcStr, '#49', 'Cnpj', 14, 14, 1, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');

    Gerador.wGrupo('/CpfCnpj');
    Gerador.wCampo(tcStr, '#50', 'InscricaoMunicipal', 01, 15, 0, NFSe.IntermediarioServico.InscricaoMunicipal, '');
    Gerador.wGrupo('/IdentificacaoIntermediario');
    Gerador.wCampo(tcStr, '#48', 'RazaoSocial', 001, 115, 0, NFSe.IntermediarioServico.RazaoSocial, '');
    Gerador.wGrupo('/Intermediario');
  end;
end;

procedure TNFSeW_Agili.GerarServicoValores;
begin
  Gerador.wCampo(tcDe2, '#13', 'ValorServicos ', 01, 15, 1, NFSe.Servico.Valores.ValorServicos, '');
  Gerador.wCampo(tcDe2, '#14', 'ValorDescontos', 01, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, '');
  Gerador.wCampo(tcDe2, '#15', 'ValorPis      ', 01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampo(tcDe2, '#16', 'ValorCofins   ', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampo(tcDe2, '#17', 'ValorInss     ', 01, 15, 1, NFSe.Servico.Valores.ValorInss, '');

  if VersaoNFSe = ve100 then
    Gerador.wCampo(tcDe2, '#18', 'ValorIrrf', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '')
  else
    Gerador.wCampo(tcDe2, '#18', 'ValorIr  ', 01, 15, 1, NFSe.Servico.Valores.ValorIr, '');

  Gerador.wCampo(tcDe2, '#19', 'ValorCsll           ', 01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');
  Gerador.wCampo(tcDe2, '#23', 'ValorOutrasRetencoes', 01, 15, 1, NFSe.Servico.Valores.OutrasRetencoes, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampo(tcDe2, '#24', 'ValorBaseCalculoISSQN', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');
    Gerador.wCampo(tcDe2, '#25', 'AliquotaISSQN        ', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
    Gerador.wCampo(tcDe2, '#21', 'ValorISSQNCalculado  ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');

    if NFSe.OptanteSimplesNacional = snNao then
      Gerador.wCampo(tcDe2, '#21', 'ValorISSQNRecolher', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  end
  else
  begin
    Gerador.wCampo(tcDe2, '#24', 'ValorBaseCalculoIss  ', 01, 15, 0, NFSe.Servico.Valores.BaseCalculo, '');
    Gerador.wCampo(tcDe2, '#25', 'Aliquota             ', 01, 05, 0, NFSe.Servico.Valores.Aliquota, '');
    Gerador.wCampo(tcDe2, '#21', 'ValorIss             ', 01, 15, 0, NFSe.Servico.Valores.ValorIss, '');
  end;

  Gerador.wCampo(tcDe2, '', 'ValorDeducaoConstCivil', 01, 15, 1, 0, '');
  Gerador.wCampo(tcDe2, '', 'ValorLiquido          ', 01, 15, 1, NFSe.Servico.Valores.ValorLiquidoNfse, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampo(tcStr, '', 'Observacao            ', 01, 4000, 0, NFSe.OutrasInformacoes, '');
    Gerador.wCampo(tcStr, '', 'Complemento           ', 01, 3000, 0, '', '');  // Não enviar TAG
  end;

end;

procedure TNFSeW_Agili.GerarListaServicos;
var
  i: Integer;
  codLCServ: string;
begin
  Gerador.wGrupo('ListaServico');

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    codLCServ := IntToStr(StrToIntDef(OnlyNumber(NFSe.Servico.ItemServico[i].CodLCServ), 0));

    if Length(codLCServ) > 2 then
      Insert('.', codLCServ, Length(codLCServ) - 2 + 1);

    Gerador.wGrupo('DadosServico');
    Gerador.wCampo(tcStr, '#32', 'Discriminacao', 01, 2000, 1,
                    StringReplace( NFSe.Servico.ItemServico[i].Discriminacao, ';', FQuebradeLinha, [rfReplaceAll, rfIgnoreCase] ), '');

    if VersaoNFSe = ve100 then
    begin
      case StrToIntDef(NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0) of
        5104526: // Ipiranga do Norte/MT
          begin
            Gerador.wCampo(tcStr, '#29', 'ItemLei116', 01, 140, 1, codLCServ, '');
            Gerador.wCampo(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
          end;

        5105150, // Juina/MT
        1505031: // Novo Progresso/PA
          Gerador.wCampo(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
      else
        begin
          Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 0007, 0, FormatarCnae(NFSe.Servico.CodigoCnae), '');
          Gerador.wCampo(tcStr, '#29', 'ItemLei116', 01, 140, 1, codLCServ, '');
          Gerador.wCampo(tcDe4, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
        end;
      end;
    end
    else
    begin
      if (StrToInt(NFSe.PrestadorServico.Endereco.CodigoMunicipio) <> 5105150) then
        Gerador.wCampo(tcStr, '#29', 'ItemLei116', 01, 015, 0, codLCServ, '');

      Gerador.wCampo(tcDe2, '#13', 'Quantidade', 01, 17, 1, NFSe.Servico.ItemServico[i].Quantidade, '');
    end;

    Gerador.wCampo(tcDe2, '#13', 'ValorServico ', 01, 15, 1, NFSe.Servico.ItemServico[i].ValorUnitario, '');
    Gerador.wCampo(tcDe2, '#14', 'ValorDesconto', 01, 15, 1, NFSe.Servico.ItemServico[i].DescontoIncondicionado, '');

    Gerador.wGrupo('/DadosServico');
  end;

  Gerador.wGrupo('/ListaServico');
end;

procedure TNFSeW_Agili.GerarConstrucaoCivil;
begin
  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Gerador.wGrupo('ConstrucaoCivil');
    Gerador.wCampo(tcStr, '#51', 'CodigoObra', 01, 15, 1, NFSe.ConstrucaoCivil.CodigoObra, '');
    Gerador.wCampo(tcStr, '#52', 'Art       ', 01, 15, 1, NFSe.ConstrucaoCivil.Art, '');
    Gerador.wGrupo('/ConstrucaoCivil');
  end;
end;

procedure TNFSeW_Agili.GerarRegimeEspecialTributacao;
begin
  if VersaoNFSe = ve100 then
  begin
    if _RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao) <> '' then
    begin
      Gerador.wGrupo('RegimeEspecialTributacao');
      Gerador.wCampo(tcStr, '', 'Codigo   ', 01, 01, 1, _RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '');
      Gerador.wCampo(tcStr, '', 'Descricao', 01, 300, 0, '', '');
      Gerador.wGrupo('/RegimeEspecialTributacao');
    end;
  end
  else
  begin
    Gerador.wCampo(tcStr, '#6', 'RegimeEspecialTributacao', 01, 01, 1, RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), '')
  end;
end;

procedure TNFSeW_Agili.GerarResponsavelISSQN;
begin
  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupo('ResponsavelISSQN');
    Gerador.wCampo(tcStr, '', 'Codigo   ', 01, 01, 0, _ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');
    Gerador.wCampo(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wGrupo('/ResponsavelISSQN');
  end
  else
  begin
    if (NFSe.Servico.Valores.IssRetido <> stNormal) then
      Gerador.wCampo(tcStr, '', 'ResponsavelRetencao', 01, 01, 0, ResponsavelRetencaoToStr(NFSe.Servico.ResponsavelRetencao), '');
  end;
end;

procedure TNFSeW_Agili.GerarExigibilidadeISSQN;
begin
  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupo('ExigibilidadeISSQN');
    Gerador.wCampo(tcStr, '', 'Codigo   ', 01, 01, 1, _ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '');
    Gerador.wCampo(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wGrupo('/ExigibilidadeISSQN');
  end
  else
  begin
    Gerador.wCampo(tcStr, '', 'ExigibilidadeIss', 01, 01, 1, ExigibilidadeISSToStr(NFSe.Servico.ExigibilidadeISS), '')
  end;
end;

procedure TNFSeW_Agili.GerarXML_Agili;
var
 LTagAtividadeEconomica: String;
begin
  Gerador.wGrupo('InfDeclaracaoPrestacaoServico');

  if VersaoNFSe = ve100 then
  begin
    GerarPrestador;

    if OnlyNumber(NFSe.NfseSubstituida) <> '' then
      Gerador.wCampo(tcStr, '', 'NfseSubstituida', 01, 15, 1, OnlyNumber(NFSe.NfseSubstituida), '');

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

  Gerador.wCampo(tcStr, '#7', 'OptanteSimplesNacional', 01, 01, 1, SimNaoToStr(NFSe.OptanteSimplesNacional), '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wCampo(tcStr, '', 'OptanteMEISimei', 01, 01, 1, SimNaoToStr(NFSe.OptanteMEISimei), '');

    if NFSe.Servico.Valores.IssRetido = stRetencao then
      Gerador.wCampo(tcStr, '', 'ISSQNRetido', 01, 01, 1, SimNaoToStr(snSim), '')
    else
      Gerador.wCampo(tcStr, '', 'ISSQNRetido', 01, 01, 1, SimNaoToStr(snNao), '');
  end
  else
  begin
    if NFSe.Servico.Valores.IssRetido = stRetencao then
      Gerador.wCampo(tcStr, '', 'IssRetido', 01, 01, 1, SimNaoToStr(snSim), '')
    else
      Gerador.wCampo(tcStr, '', 'IssRetido', 01, 01, 1, SimNaoToStr(snNao), '');
  end;

  GerarResponsavelISSQN;

  case StrToInt(NFSe.PrestadorServico.Endereco.CodigoMunicipio) of
    1505031,
    5104526: LTagAtividadeEconomica := 'CodigoCnaeAtividadeEconomica';

    5105150,
    5107305: LTagAtividadeEconomica := 'ItemLei116AtividadeEconomica';
  else
    LTagAtividadeEconomica := 'CodigoAtividadeEconomica';
  end;

  if NaoEstaVazio(NFSe.Servico.CodigoTributacaoMunicipio) then
    Gerador.wCampo(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, NFSe.Servico.CodigoTributacaoMunicipio, '')
  else
  begin
    if VersaoNFSe = ve100 then
      Gerador.wCampo(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, FormatarCnae(NFSe.Servico.CodigoCnae), '')
    else
      Gerador.wCampo(tcStr, '', LTagAtividadeEconomica, 01, 140, 1, NFSe.Servico.CodigoCnae, '');
  end;

  if (VersaoNFSe = ve200) and
     (StrToInt(NFSe.PrestadorServico.Endereco.CodigoMunicipio) <> 5105150) then
    Gerador.wCampo(tcStr, '#30', 'CodigoCnae', 01, 15, 0, OnlyNumber(NFSe.Servico.CodigoCnae), '');

  GerarExigibilidadeISSQN;
  Gerador.wCampo(tcStr, '', 'BeneficioProcesso', 01, 30, 0, NFSe.Servico.NumeroProcesso, '');

  if VersaoNFSe = ve100 then
  begin
    Gerador.wGrupo('MunicipioIncidencia');
    Gerador.wCampo(tcInt, '#36', 'CodigoMunicipioIBGE', 07, 07, 1, NFSe.Servico.MunicipioIncidencia, '');
    Gerador.wCampo(tcStr, '', 'Descricao', 01, 300, 0, '', '');
    Gerador.wCampo(tcStr, '', 'Uf', 02, 02, 0, '', '');
    Gerador.wGrupo('/MunicipioIncidencia');
  end
  else begin
    Gerador.wCampo(tcStr, '', 'MunicipioIncidencia', 7, 7, 0, NFSe.Servico.MunicipioIncidencia, '')
  end;

  GerarServicoValores;
  GerarListaServicos;

  if VersaoNFSe = ve100 then
    Gerador.wCampo(tcStr, '', 'Versao', 04, 04, 1, '1.00', '');

  Gerador.wGrupo('/InfDeclaracaoPrestacaoServico');
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
