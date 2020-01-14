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

unit pnfsNFSeW_ISSDSF;

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
  pnfsNFSe, pnfsConversao, pnfsConsts;

type
  { TNFSeW_ISSDSF }

  TNFSeW_ISSDSF = class(TNFSeWClass)
  private
    FSituacao: String;
    FTipoRecolhimento: String;
  protected

    procedure GerarIdentificacaoRPS;
    procedure GerarRPSSubstituido;

    procedure GerarPrestador;
    procedure GerarTomador;
    procedure GerarIntermediarioServico;

    procedure GerarListaServicos;
    procedure GerarValoresServico;

    procedure GerarXML_ISSDSF;

  public
    constructor Create(ANFSeW: TNFSeW); override;

    function ObterNomeArquivo: String; override;
    function GerarXml: Boolean; override;

    property Situacao: String         read FSituacao;
    property TipoRecolhimento: String read FTipoRecolhimento;
  end;

implementation

uses
  ACBrUtil;

{==============================================================================}
{ Essa unit tem por finalidade exclusiva de gerar o XML do RPS segundo o       }
{ layout do ISSDSF.                                                            }
{ Sendo assim só será criado uma nova unit para um novo layout.                }
{==============================================================================}

{ TNFSeW_ISSDSF }

procedure TNFSeW_ISSDSF.GerarIdentificacaoRPS;
begin
  Gerador.wCampoNFSe(tcStr, '', 'TipoRPS', 01, 20, 1, 'RPS', '');

  if NFSe.IdentificacaoRps.Serie = '' then
    Gerador.wCampoNFSe(tcStr, '', 'SerieRPS', 01, 02, 1, 'NF', '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'SerieRPS', 01, 02, 1, NFSe.IdentificacaoRps.Serie, '');

  Gerador.wCampoNFSe(tcStr,    '', 'NumeroRPS',      01, 12, 1, NFSe.IdentificacaoRps.Numero, DSC_NUMRPS);
  Gerador.wCampoNFSe(tcDatHor, '', 'DataEmissaoRPS', 01, 21, 1, NFse.DataEmissaoRps, '');
  Gerador.wCampoNFSe(tcStr,    '', 'SituacaoRPS',    01, 01, 1, Situacao, '');

  GerarRPSSubstituido;

  if (NFSe.SeriePrestacao = '') then
    Gerador.wCampoNFSe(tcInt, '', 'SeriePrestacao', 01, 02, 1, '99', '')
  else
    Gerador.wCampoNFSe(tcInt, '', 'SeriePrestacao', 01, 02, 1, NFSe.SeriePrestacao, '');
end;

procedure TNFSeW_ISSDSF.GerarRPSSubstituido;
begin
  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    if NFSe.RpsSubstituido.Serie = '' then
      Gerador.wCampoNFSe(tcStr, '', 'SerieRPSSubstituido', 00, 02, 1, 'NF', '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'SerieRPSSubstituido', 00, 02, 1, NFSe.RpsSubstituido.Serie, '');

    Gerador.wCampoNFSe(tcStr, '', 'NumeroNFSeSubstituida', 00, 02, 1, NFSe.RpsSubstituido.Numero, '');
    Gerador.wCampoNFSe(tcStr, '', 'NumeroRPSSubstituido',  00, 02, 1, NFSe.RpsSubstituido.Numero, '');
  end;
end;

procedure TNFSeW_ISSDSF.GerarPrestador;
begin
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 011, 1, NFSe.Prestador.InscricaoMunicipal, '');
  Gerador.wCampoNFSe(tcStr, '', 'RazaoSocialPrestador',        01, 120, 1, NFSe.PrestadorServico.RazaoSocial, '');
end;

procedure TNFSeW_ISSDSF.GerarTomador;
begin
  //TO DO - formatar segundo padrao da cidade
  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalTomador',  01, 011, 1, NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, '');  // CORRETO
  //  LINHA COMENTADA - ESTÁ ABAIXO QUE FOI SUBSTITUIDA PELA LINHA ACIMA.
  //  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalTomador',  01, 011, 1, '', '');
  Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJTomador',             01, 014, 1, OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), '');
  Gerador.wCampoNFSe(tcStr, '', 'RazaoSocialTomador',         01, 120, 1, NFSe.Tomador.RazaoSocial, '');
  if FProvedor = proIssDSF then
    Gerador.wCampoNFSe(tcStr, '', 'DocTomadorEstrangeiro',      00, 020, 1, NFSe.Tomador.IdentificacaoTomador.DocTomadorEstrangeiro, '');

  Gerador.wCampoNFSe(tcStr, '', 'TipoLogradouroTomador',      00, 10, 1, NFSe.Tomador.Endereco.TipoLogradouro, '');
  Gerador.wCampoNFSe(tcStr, '', 'LogradouroTomador',          01, 50, 1, NFSe.Tomador.Endereco.Endereco, '');
  Gerador.wCampoNFSe(tcStr, '', 'NumeroEnderecoTomador',      01, 09, 1, NFSe.Tomador.Endereco.Numero, '');
  Gerador.wCampoNFSe(tcStr, '', 'ComplementoEnderecoTomador', 01, 30, 0, NFSe.Tomador.Endereco.Complemento, '');
  Gerador.wCampoNFSe(tcStr, '', 'TipoBairroTomador',          00, 10, 1, NFSe.Tomador.Endereco.TipoBairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'BairroTomador',              01, 50, 1, NFSe.Tomador.Endereco.Bairro, '');
  Gerador.wCampoNFSe(tcStr, '', 'CidadeTomador',              01, 10, 1, CodCidadeToCodSiafi(strtoint64(NFSe.Tomador.Endereco.CodigoMunicipio)), '');

  if (Trim(NFSe.Tomador.Endereco.xMunicipio) <> '') then
    Gerador.wCampoNFSe(tcStr, '', 'CidadeTomadorDescricao', 01, 50, 1, NFSe.Tomador.Endereco.xMunicipio, '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'CidadeTomadorDescricao', 01, 50, 1, CodCidadeToCidade(strtoint64(NFSe.Tomador.Endereco.CodigoMunicipio)), '');

  Gerador.wCampoNFSe(tcStr, '', 'CEPTomador',   01, 08, 1, OnlyNumber(NFSe.Tomador.Endereco.CEP), '');
  Gerador.wCampoNFSe(tcStr, '', 'EmailTomador', 01, 60, 1, NFSe.Tomador.Contato.Email, '');
end;

procedure TNFSeW_ISSDSF.GerarIntermediarioServico;
begin
  Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJIntermediario', 00, 14, 0, OnlyNumber(NFSe.IntermediarioServico.CpfCnpj), '');
end;

procedure TNFSeW_ISSDSF.GerarListaServicos;
var
  i: Integer;
  sDeducaoPor, sTipoDeducao, sTributavel: String;
begin
  if NFSe.Servico.Deducao.Count > 0 then
  begin
    Gerador.wGrupoNFSe('Deducoes');

    for i := 0 to NFSe.Servico.Deducao.Count -1 do
    begin
      Gerador.wGrupoNFSe('Deducao');

      sDeducaoPor := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].DeducaoPor,
                                           ['Percentual', 'Valor'], [dpPercentual, dpValor]);
      Gerador.wCampoNFSe(tcStr, '', 'DeducaoPor', 01, 20, 1, sDeducaoPor , '');

      sTipoDeducao := EnumeradoToStr( NFSe.Servico.Deducao.Items[i].TipoDeducao,
                                            ['', 'Despesas com Materiais', 'Despesas com Subempreitada', 'Deducao de Valor'],
                                            [tdNenhum, tdMateriais, tdSubEmpreitada, tdValor]);
      Gerador.wCampoNFSe(tcStr, '', 'TipoDeducao', 00, 255, 1, sTipoDeducao , '');

      Gerador.wCampoNFSe(tcStr, '', 'CPFCNPJReferencia'   , 00, 14, 1, OnlyNumber(NFSe.Servico.Deducao.Items[i].CpfCnpjReferencia) , '');
      Gerador.wCampoNFSe(tcStr, '', 'NumeroNFReferencia'  , 00, 10, 1, NFSe.Servico.Deducao.Items[i].NumeroNFReferencia, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorTotalReferencia', 00, 18, 1, NFSe.Servico.Deducao.Items[i].ValorTotalReferencia, '');
      Gerador.wCampoNFSe(tcDe2, '', 'PercentualDeduzir', 00, 18, 1, NFSe.Servico.Deducao.Items[i].PercentualDeduzir, '');
      Gerador.wCampoNFSe(tcDe2, '', 'ValorDeduzir'   , 00, 08, 1, NFSe.Servico.Deducao.Items[i].ValorDeduzir, '');

      Gerador.wGrupoNFSe('/Deducao');
    end;

    Gerador.wGrupoNFSe('/Deducoes');
  end;

  if NFSe.Servico.ItemServico.Count > 0 then
  begin
    Gerador.wGrupoNFSe('Itens');

    for i := 0 to NFSe.Servico.ItemServico.Count -1 do
    begin
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
  end;
end;

procedure TNFSeW_ISSDSF.GerarValoresServico;
begin
  Gerador.wCampoNFSe(tcStr, '', 'CodigoAtividade', 01, 09, 1, NFSe.Servico.CodigoCnae, '');

  if FProvedor = proCTA then
    Gerador.wCampoNFSe(tcInt, '', 'CodigoServico', 04, 05, 1, OnlyNumber(NFSe.Servico.ItemListaServico), '');
    
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaAtividade', 01, 11, 1, NFSe.Servico.Valores.Aliquota, '');

  // "A" a receber; "R" retido na Fonte
  FTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido,
                                       ['A','R'], [stNormal, stRetencao]);

  Gerador.wCampoNFSe(tcStr, '', 'TipoRecolhimento', 01, 01,  1, TipoRecolhimento, '');

  Gerador.wCampoNFSe(tcStr, '', 'MunicipioPrestacao',          01, 10,  1, CodCidadeToCodSiafi(strtoint64(NFSe.Servico.CodigoMunicipio)), '');
  Gerador.wCampoNFSe(tcStr, '', 'MunicipioPrestacaoDescricao', 01, 30,  1, CodCidadeToCidade(strtoint64(NFSe.Servico.CodigoMunicipio)), '');

  (*
  case NFSe.NaturezaOperacao of
    no1: begin
           Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, EnumeradoToStr( NFSe.DeducaoMateriais, ['B','A'], [snSim, snNao]), '');
           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','T'], [snSim, snNao]), '');
         end;
    no2: begin
           Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, EnumeradoToStr( NFSe.DeducaoMateriais, ['B','A'], [snSim, snNao]), '');
           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','G'], [snSim, snNao]), '');
         end;
    no3: begin
           Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, 'C', '');
           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'C', '');
         end;
    no4: begin
           Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, 'C', '');
           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'F', '');
         end;
    no5,
    no6: begin
           if NFSe.DeducaoMateriais = snSim then
             Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, 'B', '')
           else
             Gerador.wCampoNFSe(tcStr, '', 'Operacao', 01, 01, 1, 'A', '');

           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, 'K', '');
         end;
    no7: begin
           Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01,  1, 'A', '');
           Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01,  1, 'N', '');
         end;
  end;

  if NFse.RegimeEspecialTributacao = retMicroempresarioIndividual then
    Gerador.wCampoNFSe(tcStr, '', 'Tributacao',01, 01,  1, 'M', '');
 *)

  Gerador.wCampoNFSe(tcStr, '', 'Operacao',   01, 01, 1, OperacaoToStr(NFSe.Servico.Operacao), '');
  Gerador.wCampoNFSe(tcStr, '', 'Tributacao', 01, 01, 1, TributacaoToStr(NFSe.Servico.Tributacao), '');

  Gerador.wCampoNFSe(tcDe2, '', 'ValorPIS',    01, 15, 1, NFSe.Servico.Valores.ValorPis, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCOFINS', 01, 15, 1, NFSe.Servico.Valores.ValorCofins, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorINSS',   01, 15, 1, NFSe.Servico.Valores.ValorInss, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorIR',     01, 15, 1, NFSe.Servico.Valores.ValorIr, '');
  Gerador.wCampoNFSe(tcDe2, '', 'ValorCSLL',   01, 15, 1, NFSe.Servico.Valores.ValorCsll, '');

  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaPIS',    01, 06, 1, NFSe.Servico.Valores.AliquotaPIS, '');
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaCOFINS', 01, 06, 1, NFSe.Servico.Valores.AliquotaCOFINS, '');
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaINSS',   01, 06, 1, NFSe.Servico.Valores.AliquotaINSS, '');
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaIR',     01, 06, 1, NFSe.Servico.Valores.AliquotaIR, '');
  Gerador.wCampoNFSe(tcDe4, '', 'AliquotaCSLL',   01, 06, 1, NFSe.Servico.Valores.AliquotaCSLL, '');

  Gerador.wCampoNFSe(tcStr, '', 'DescricaoRPS', 01, 1500, 1, NFSe.OutrasInformacoes, '');
end;

procedure TNFSeW_ISSDSF.GerarXML_ISSDSF;
var
  sIEEmit, SerieRPS, NumeroRPS, sDataEmis,
  sTributacao, sSituacaoRPS, sTipoRecolhimento,
  sValorServico, sValorDeducao, sCodAtividade,
  sCPFCNPJTomador, sAssinatura: String;
begin
  Gerador.Prefixo := '';
  Gerador.wGrupoNFSe('RPS ' + FIdentificador + '="rps:' + NFSe.InfID.ID + '"');

  FSituacao := EnumeradoToStr( NFSe.Status, ['N','C'], [srNormal, srCancelado]);

  sIEEmit           := Poem_Zeros(NFSe.Prestador.InscricaoMunicipal, 11);
  SerieRPS          := PadRight( NFSe.IdentificacaoRps.Serie, 5 , ' ');
  NumeroRPS         := Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12);
  sDataEmis         := FormatDateTime('yyyymmdd',NFse.DataEmissaoRps);
//  sTributacao := EnumeradoToStr( NFSe.OptanteSimplesNacional, ['H','T'], [snSim, snNao]) + ' ';
  sTributacao       := TributacaoToStr(NFSe.Servico.Tributacao) + ' ';
  sSituacaoRPS      := FSituacao;
  sTipoRecolhimento := EnumeradoToStr( NFSe.Servico.Valores.IssRetido, ['N','S'], [stNormal, stRetencao]);

  sValorServico   := Poem_Zeros( OnlyNumber( FormatFloat('#0.00',
                                (NFSe.Servico.Valores.ValorServicos -
                                 NFSe.Servico.Valores.ValorDeducoes) ) ), 15);
  sValorDeducao   := Poem_Zeros( OnlyNumber( FormatFloat('#0.00',
                                 NFSe.Servico.Valores.ValorDeducoes)), 15 );
  sCodAtividade   := Poem_Zeros( OnlyNumber( NFSe.Servico.CodigoCnae ), 10 );
  sCPFCNPJTomador := Poem_Zeros( OnlyNumber( NFSe.Tomador.IdentificacaoTomador.CpfCnpj), 14);


  sAssinatura := sIEEmit + SerieRPS + NumeroRPS + sDataEmis + sTributacao +
                 sSituacaoRPS + sTipoRecolhimento + sValorServico +
                 sValorDeducao + sCodAtividade + sCPFCNPJTomador;

  sAssinatura := AsciiToHex(SHA1(sAssinatura));
  sAssinatura := LowerCase(sAssinatura);

  Gerador.wCampoNFSe(tcStr, '', 'Assinatura', 01, 2000, 1, sAssinatura, '');

  GerarPrestador;
  GerarIdentificacaoRPS;
  GerarTomador;
  GerarValoresServico;

  if Length(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone)) = 11 then
    Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),3), '')
  else
    if Length(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone)) = 10 then
      Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),2), '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'DDDPrestador', 00, 03, 1, '', '');

  Gerador.wCampoNFSe(tcStr, '', 'TelefonePrestador', 00, 08, 1, RightStr(OnlyNumber(NFSe.PrestadorServico.Contato.Telefone),8), '');

  if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then
    Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),3), '')
  else
    if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then
      Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),2), '')
    else
      Gerador.wCampoNFSe(tcStr, '', 'DDDTomador', 00, 03, 1, '', '');

  Gerador.wCampoNFSe(tcStr, '', 'TelefoneTomador', 00, 08, 1, RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone),8), '');

  if (NFSe.Status = srCancelado) then
    Gerador.wCampoNFSe(tcStr, '', 'MotCancelamento',01, 80, 1, NFSE.MotivoCancelamento, '')
  else
    Gerador.wCampoNFSe(tcStr, '', 'MotCancelamento',00, 80, 0, NFSE.MotivoCancelamento, '');

  GerarIntermediarioServico;
  GerarListaServicos;

  Gerador.wGrupoNFSe('/RPS');
end;

constructor TNFSeW_ISSDSF.Create(ANFSeW: TNFSeW);
begin
  inherited Create(ANFSeW);

end;

function TNFSeW_ISSDSF.ObterNomeArquivo: String;
begin
  Result := OnlyNumber(NFSe.infID.ID) + '.xml';
end;

function TNFSeW_ISSDSF.GerarXml: Boolean;
begin
  Gerador.ListaDeAlertas.Clear;

  Gerador.ArquivoFormatoXML := '';
  Gerador.Prefixo           := FPrefixo4;

  Gerador.Opcoes.QuebraLinha := FQuebradeLinha;

  FDefTipos := FServicoEnviar;

  if (RightStr(FURL, 1) <> '/') and (FDefTipos <> '')
    then FDefTipos := '/' + FDefTipos;

  if Trim(FPrefixo4) <> ''
    then Atributo := ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FURL + FDefTipos + '"'
    else Atributo := ' xmlns="' + FURL + FDefTipos + '"';

  FNFSe.InfID.ID := FNFSe.IdentificacaoRps.Numero;

  GerarXML_ISSDSF;

  Gerador.gtAjustarRegistros(NFSe.InfID.ID);
  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.
